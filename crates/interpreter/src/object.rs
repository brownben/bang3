use crate::{
  bytecode::Chunk,
  value::Value,
  vm::{ErrorKind, VM},
};
use bang_gc::{Gc, GcList, Heap};
use std::{fmt, ptr, slice, str};

/// A descriptor for a type in the virtual machine.
/// Is only defined for primitive types which fit soley within a single [Value].
///
/// Describes the various methods which all types should be able to perform.
#[derive(Debug, Clone)]
pub struct TypeDescriptor {
  /// The name of the type
  pub type_name: &'static str,
  /// Trace the value for the garbage collector
  pub trace: fn(vm: &VM, object_pointer: Gc<u8>, trace_value: TraceValueFunction) -> (),
  /// Display the value as a string
  pub display: fn(heap: &Heap, object_pointer: Gc<u8>) -> String,
  /// Whether the value is falsy
  pub is_falsy: fn(heap: &Heap, object_pointer: Gc<u8>) -> bool,
}
type TraceValueFunction = fn(vm: &VM, Value) -> ();

pub const DEFAULT_TYPE_DESCRIPTORS: &[TypeDescriptor] =
  &[STRING, NATIVE_FUNCTION, CLOSURE, ALLOCATED, NATIVE_CLOSURE];

/// A string on the heap
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BangString(GcList<u8>);
impl BangString {
  pub fn as_str(self, heap: &Heap) -> &str {
    unsafe { str::from_utf8_unchecked(heap.get_list_buffer(self.0)) }
  }

  /// Concatenate two strings together
  ///
  /// As the parts may also be borrowed from the heap, we pass the strings as pointers
  pub fn concatenate(
    heap: &mut Heap,
    (left_ptr, left_len): (*const u8, usize),
    (right_ptr, right_len): (*const u8, usize),
  ) -> Value {
    let length = left_len + right_len;
    let new_string = heap.allocate_list_header(length);
    let new_string_buffer = heap.get_list_buffer_mut(new_string);
    unsafe {
      new_string_buffer[..left_len].copy_from_slice(slice::from_raw_parts(left_ptr, left_len));
      new_string_buffer[left_len..].copy_from_slice(slice::from_raw_parts(right_ptr, right_len));
    }

    Value::from_object(*new_string, STRING_TYPE_ID)
  }
}
impl From<Gc<usize>> for BangString {
  fn from(value: Gc<usize>) -> Self {
    Self(value.into())
  }
}
const STRING: TypeDescriptor = TypeDescriptor {
  type_name: "string",
  trace: |vm, value, _trace| vm.heap.mark(value),
  display: |heap, value| heap[value.cast::<BangString>()].as_str(heap).to_owned(),
  is_falsy: |heap, value| heap[value.cast::<BangString>()].as_str(heap).is_empty(),
};
pub const STRING_TYPE_ID: usize = 0;

/// A native function
#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunction {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value) -> Result<Value, ErrorKind>,
}
impl NativeFunction {
  /// Create a new naative function
  pub(crate) fn new(
    name: &'static str,
    func: fn(&mut VM, Value) -> Result<Value, ErrorKind>,
  ) -> Self {
    Self { name, func }
  }
}
impl fmt::Display for NativeFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<function {}>", self.name)
  }
}
const NATIVE_FUNCTION: TypeDescriptor = TypeDescriptor {
  type_name: "function",
  trace: |vm, value, _trace_value| vm.heap.mark(value),
  display: |heap, value| heap[value.cast::<NativeFunction>()].to_string(),
  is_falsy: |_, _| false,
};
pub const NATIVE_FUNCTION_TYPE_ID: usize = 1;

/// A closure - a function which has captured variables from the surrounding scope.
#[derive(Clone, Debug)]
pub struct Closure {
  func: *const Chunk,
  pub upvalues: GcList<Value>,
}
impl Closure {
  pub fn new(func: &Chunk, upvalues: GcList<Value>) -> Self {
    Self {
      func: ptr::from_ref(func),
      upvalues,
    }
  }

  #[inline]
  pub fn function(&self) -> &Chunk {
    unsafe { &*self.func }
  }
}
impl fmt::Display for Closure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure {}>", self.function().display())
  }
}
const CLOSURE: TypeDescriptor = TypeDescriptor {
  type_name: "function",
  trace: |vm, value, trace_value| {
    vm.heap.mark(value);
    let closure = &vm.heap[value.cast::<Closure>()];
    vm.heap.mark(*closure.upvalues);
    for upvalue in vm.heap.get_list_buffer(closure.upvalues) {
      trace_value(vm, *upvalue);
    }
  },
  display: |heap, value| heap[value.cast::<Closure>()].to_string(),
  is_falsy: |_, _| false,
};
pub const CLOSURE_TYPE_ID: usize = 2;

/// A value which has been moved from the stack to the heap, as it has been captured by a closure
const ALLOCATED: TypeDescriptor = TypeDescriptor {
  type_name: "allocated",
  trace: |vm, value, trace_value| {
    vm.heap.mark(value);
    // If the allocated value is a compound value, we may need to trace the value inside
    trace_value(vm, vm.heap[value.cast::<Value>()]);
  },
  display: |_, _| unreachable!("Not accessed as a value"),
  is_falsy: |_, _| unreachable!("Not accessed as a value"),
};
pub const ALLOCATED_TYPE_ID: usize = 3;

/// A native closure - a native function which has got a value from a previous call
#[derive(Clone, Debug)]
pub struct NativeClosure {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value, Value) -> Result<Value, ErrorKind>,
  pub(crate) captured_value: Value,
}
impl NativeClosure {
  /// Create a new naative function
  pub(crate) fn new(
    name: &'static str,
    func: fn(&mut VM, Value, Value) -> Result<Value, ErrorKind>,
    captured_value: Value,
  ) -> Self {
    Self {
      name,
      func,
      captured_value,
    }
  }
}
impl fmt::Display for NativeClosure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure <function {}>>", self.name)
  }
}
const NATIVE_CLOSURE: TypeDescriptor = TypeDescriptor {
  type_name: "function",
  trace: |vm, value, trace_value| {
    vm.heap.mark(value);
    let closure = &vm.heap[value.cast::<NativeClosure>()];
    trace_value(vm, closure.captured_value);
  },
  display: |heap, value| heap[value.cast::<NativeClosure>()].to_string(),
  is_falsy: |_, _| false,
};
pub const NATIVE_CLOSURE_TYPE_ID: usize = 4;

#[cfg(test)]
mod test {
  use super::{
    ALLOCATED_TYPE_ID, CLOSURE_TYPE_ID, NATIVE_CLOSURE_TYPE_ID, NATIVE_FUNCTION_TYPE_ID,
    STRING_TYPE_ID,
  };
  use crate::{object::DEFAULT_TYPE_DESCRIPTORS, EmptyContext, VM};
  use bang_gc::HeapSize;

  #[test]
  fn type_ids_match() {
    let objects = &DEFAULT_TYPE_DESCRIPTORS;

    assert_eq!(objects[STRING_TYPE_ID].type_name, "string");
    assert_eq!(objects[NATIVE_FUNCTION_TYPE_ID].type_name, "function");
    assert_eq!(objects[CLOSURE_TYPE_ID].type_name, "function");
    assert_eq!(objects[ALLOCATED_TYPE_ID].type_name, "allocated");
    assert_eq!(objects[NATIVE_CLOSURE_TYPE_ID].type_name, "function");
  }

  #[test]
  fn type_ids_match_in_vm() {
    let vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();

    assert_eq!(vm.types[STRING_TYPE_ID].type_name, "string");
    assert_eq!(vm.types[NATIVE_FUNCTION_TYPE_ID].type_name, "function");
    assert_eq!(vm.types[CLOSURE_TYPE_ID].type_name, "function");
    assert_eq!(vm.types[ALLOCATED_TYPE_ID].type_name, "allocated");
    assert_eq!(vm.types[NATIVE_CLOSURE_TYPE_ID].type_name, "function");
  }
}
