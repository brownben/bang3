use crate::{
  bytecode::Chunk,
  value::{TypeId, Value},
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
  pub display: fn(vm: &VM, object_pointer: Gc<u8>) -> String,
  /// Whether the value is falsy
  pub is_falsy: fn(vm: &VM, object_pointer: Gc<u8>) -> bool,
  /// Check if the value is equal to another value (of the same type)
  pub equals: fn(vm: &VM, a: Gc<u8>, b: Gc<u8>) -> bool,
  /// Call the object as a function
  pub call: Option<NativeFunctionCall>,
}
type NativeFunctionCall = fn(vm: &mut VM, callee: Gc<u8>, arg: Value) -> Result<Value, ErrorKind>;
type TraceValueFunction = fn(vm: &VM, Value) -> ();

pub const DEFAULT_TYPE_DESCRIPTORS: &[TypeDescriptor] = &[
  STRING,
  STRING_SLICE,
  CLOSURE,
  NATIVE_FUNCTION,
  NATIVE_CLOSURE,
  NATIVE_CLOSURE_TWO,
  ALLOCATED,
];

/// A string on the heap
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BangString(GcList<u8>);
impl BangString {
  pub fn as_str<'a>(self, vm: &'a VM) -> &'a str {
    unsafe { str::from_utf8_unchecked(vm.heap.get_list_buffer(self.0)) }
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
  display: |vm, value| vm.heap[value.cast::<BangString>()].as_str(vm).to_owned(),
  is_falsy: |vm, value| vm.heap[value.cast::<BangString>()].as_str(vm).is_empty(),
  equals: |_vm, _a, _b| unreachable!("Strings handled separately"),
  call: None,
};
pub const STRING_TYPE_ID: TypeId = TypeId(0);

/// A string slice - a reference to part of a string
#[derive(Clone)]
pub struct StringSlice {
  pub(crate) string: Value,
  pub(crate) start: usize,
  pub(crate) end: usize,
}
impl StringSlice {
  pub fn new(string: Value, start: usize, end: usize) -> Self {
    debug_assert!(string.is_string());
    Self { string, start, end }
  }

  pub fn as_str<'a>(&'a self, vm: &'a VM) -> &'a str {
    debug_assert!(self.string.is_string());

    &self.string.as_string(vm)[self.start..self.end]
  }
}
const STRING_SLICE: TypeDescriptor = TypeDescriptor {
  type_name: "string",
  trace: |vm, value, trace_value| {
    let slice = &vm.heap[value.cast::<StringSlice>()];
    trace_value(vm, slice.string);
  },
  display: |vm, value| vm.heap[value.cast::<StringSlice>()].as_str(vm).to_owned(),
  is_falsy: |vm, value| vm.heap[value.cast::<StringSlice>()].as_str(vm).is_empty(),
  equals: |_vm, _a, _b| unreachable!("Strings handled separately"),
  call: None,
};
pub const STRING_SLICE_TYPE_ID: TypeId = TypeId(1);

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
    let closure = &vm.heap[value.cast::<Closure>()];
    vm.heap.mark(*closure.upvalues);
    for upvalue in vm.heap.get_list_buffer(closure.upvalues) {
      trace_value(vm, *upvalue);
    }
  },
  display: |vm, value| vm.heap[value.cast::<Closure>()].to_string(),
  is_falsy: |_, _| false,
  equals: |_vm, a, b| a == b,
  call: None,
};
pub const CLOSURE_TYPE_ID: TypeId = TypeId(2);

/// A native function
#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunction {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value) -> Result<Value, ErrorKind>,
}
impl NativeFunction {
  /// Create a new native function
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
  trace: |_vm, _value, _trace_value| {},
  display: |vm, value| vm.heap[value.cast::<NativeFunction>()].to_string(),
  is_falsy: |_, _| false,
  equals: |_vm, a, b| a == b,
  call: Some(|vm, object, arg| {
    let function = &vm.heap[object.cast::<NativeFunction>()];
    (function.func)(vm, arg)
  }),
};
pub const NATIVE_FUNCTION_TYPE_ID: TypeId = TypeId(3);

/// A native closure - a native function which has got a value from a previous call
#[derive(Clone, Debug)]
pub struct NativeClosure {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value, Value) -> Result<Value, ErrorKind>,
  pub(crate) arg1: Value,
}
impl NativeClosure {
  /// Create a new native closure
  pub(crate) fn new(
    name: &'static str,
    func: fn(&mut VM, Value, Value) -> Result<Value, ErrorKind>,
    arg1: Value,
  ) -> Self {
    Self { name, func, arg1 }
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
    let closure = &vm.heap[value.cast::<NativeClosure>()];
    trace_value(vm, closure.arg1);
  },
  display: |vm, value| vm.heap[value.cast::<NativeClosure>()].to_string(),
  is_falsy: |_, _| false,
  equals: |_vm, a, b| a == b,
  call: Some(|vm, object, arg2| {
    let function = &vm.heap[object.cast::<NativeClosure>()];
    (function.func)(vm, function.arg1, arg2)
  }),
};
pub const NATIVE_CLOSURE_TYPE_ID: TypeId = TypeId(4);

/// A native closure with 2 args - a native function which has got two values from a previous calls
#[derive(Clone, Debug)]
pub struct NativeClosureTwo {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value, Value, Value) -> Result<Value, ErrorKind>,
  pub(crate) arg1: Value,
  pub(crate) arg2: Value,
}
impl NativeClosureTwo {
  /// Create a new native function
  pub(crate) fn new(
    name: &'static str,
    func: fn(&mut VM, Value, Value, Value) -> Result<Value, ErrorKind>,
    arg1: Value,
    arg2: Value,
  ) -> Self {
    Self {
      name,
      func,
      arg1,
      arg2,
    }
  }
}
impl fmt::Display for NativeClosureTwo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure <function {}>>", self.name)
  }
}
const NATIVE_CLOSURE_TWO: TypeDescriptor = TypeDescriptor {
  type_name: "function",
  trace: |vm, value, trace_value| {
    let closure = &vm.heap[value.cast::<NativeClosureTwo>()];
    trace_value(vm, closure.arg1);
    trace_value(vm, closure.arg2);
  },
  display: |vm, value| vm.heap[value.cast::<NativeClosureTwo>()].to_string(),
  is_falsy: |_, _| false,
  equals: |_vm, a, b| a == b,
  call: Some(|vm, object, arg3| {
    let function = &vm.heap[object.cast::<NativeClosureTwo>()];
    (function.func)(vm, function.arg1, function.arg2, arg3)
  }),
};
pub const NATIVE_CLOSURE_TWO_TYPE_ID: TypeId = TypeId(5);

/// A value which has been moved from the stack to the heap, as it has been captured by a closure
const ALLOCATED: TypeDescriptor = TypeDescriptor {
  type_name: "allocated",
  trace: |vm, value, trace_value| {
    // If the allocated value is a compound value, we may need to trace the value inside
    trace_value(vm, vm.heap[value.cast::<Value>()]);
  },
  display: |_, _| unreachable!("Not accessed as a value"),
  is_falsy: |_, _| unreachable!("Not accessed as a value"),
  equals: |_, _, _| unreachable!("Not accessed as a value"),
  call: None,
};
pub const ALLOCATED_TYPE_ID: TypeId = TypeId(6);

#[cfg(test)]
mod test {
  use super::*;
  use crate::{EmptyContext, VM};
  use bang_gc::HeapSize;

  #[test]
  fn type_ids_match() {
    let objects = &DEFAULT_TYPE_DESCRIPTORS;

    assert_eq!(objects[STRING_TYPE_ID.0].type_name, "string");
    assert_eq!(objects[STRING_SLICE_TYPE_ID.0].type_name, "string");
    assert_eq!(objects[CLOSURE_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_FUNCTION_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_CLOSURE_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_CLOSURE_TWO_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[ALLOCATED_TYPE_ID.0].type_name, "allocated");
  }

  #[test]
  fn type_ids_match_in_vm() {
    let mut vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();

    let empty_allocation = vm.heap.allocate(());

    let string = Value::from_object(empty_allocation, STRING_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string).type_name, "string");
    let string_slice = Value::from_object(empty_allocation, STRING_SLICE_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string_slice).type_name, "string");

    let closure = Value::from_object(empty_allocation, CLOSURE_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(closure).type_name, "function");

    let native_function = Value::from_object(empty_allocation, NATIVE_FUNCTION_TYPE_ID);
    assert_eq!(
      vm.get_type_descriptor(native_function).type_name,
      "function"
    );
    let native_closure = Value::from_object(empty_allocation, NATIVE_CLOSURE_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(native_closure).type_name, "function");
    let native_closure_two = Value::from_object(empty_allocation, NATIVE_CLOSURE_TWO_TYPE_ID);
    assert_eq!(
      vm.get_type_descriptor(native_closure_two).type_name,
      "function"
    );

    let allocated = Value::from_object(empty_allocation, ALLOCATED_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(allocated).type_name, "allocated");
  }
}
