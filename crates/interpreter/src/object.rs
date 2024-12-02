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
  /// Display the value as a string for debug, or part of structure
  pub debug: fn(vm: &VM, object_pointer: Gc<u8>) -> String,
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
  STRING_VIEW,
  CLOSURE,
  NATIVE_FUNCTION,
  NATIVE_CLOSURE,
  NATIVE_CLOSURE_TWO,
  LIST,
  ALLOCATED,
];

/// A string on the heap
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BangString(GcList<u8>);
impl BangString {
  pub fn as_str(self, heap: &Heap) -> &str {
    unsafe { str::from_utf8_unchecked(heap.get_list_buffer(self.0)) }
  }

  pub fn is_empty(self, vm: &VM) -> bool {
    vm.heap[*self.0] == 0
  }

  /// Concatenate two strings together
  ///
  /// As the parts may also be borrowed from the heap, we pass the strings as pointers
  pub fn concatenate(heap: &mut Heap, left: Value, right: Value) -> Value {
    let left_string = left.as_string(heap);
    let right_string = right.as_string(heap);

    if left_string.is_empty() {
      return right;
    }
    if right_string.is_empty() {
      return left;
    }

    let (left_ptr, left_len) = (left_string.as_ptr(), left_string.len());
    let (right_ptr, right_len) = (right_string.as_ptr(), right_string.len());

    let new_string = heap.allocate_list_header(left_len + right_len);
    let new_string_buffer = heap.get_list_buffer_mut(new_string);

    unsafe {
      new_string_buffer[..left_len].copy_from_slice(slice::from_raw_parts(left_ptr, left_len));
      new_string_buffer[left_len..].copy_from_slice(slice::from_raw_parts(right_ptr, right_len));
    }

    Value::from_object(*new_string, STRING_TYPE_ID)
  }
}
impl<T> From<Gc<T>> for BangString {
  fn from(value: Gc<T>) -> Self {
    Self(value.cast().into())
  }
}
const STRING: TypeDescriptor = TypeDescriptor {
  type_name: "string",
  trace: |_, _, _| {},
  display: |vm, value| BangString::from(value).as_str(&vm.heap).to_owned(),
  debug: |vm, value| format!("'{}'", BangString::from(value).as_str(&vm.heap)),
  is_falsy: |vm, value| BangString::from(value).is_empty(vm),
  equals: |_, _, _| unreachable!("Strings handled separately"),
  call: None,
};
pub const STRING_TYPE_ID: TypeId = TypeId(0);

/// A string view - a reference to part of a string
#[derive(Clone)]
pub struct StringView {
  pub(crate) string: Value,
  pub(crate) start: usize,
  pub(crate) end: usize,
}
impl StringView {
  pub fn new(string: Value, start: usize, end: usize) -> Self {
    debug_assert!(string.is_string());
    debug_assert!(start <= end);

    Self { string, start, end }
  }

  pub fn from_ptr<T>(ptr: Gc<T>, heap: &Heap) -> &str {
    heap[ptr.cast::<StringView>()].as_str(heap)
  }

  pub fn as_str<'a>(&'a self, heap: &'a Heap) -> &'a str {
    debug_assert!(self.string.is_string());

    &self.string.as_string(heap)[self.start..self.end]
  }
}
const STRING_VIEW: TypeDescriptor = TypeDescriptor {
  type_name: "string",
  trace: |vm, value, trace_value| {
    let view = &vm.heap[value.cast::<StringView>()];
    trace_value(vm, view.string);
  },
  display: |vm, value| StringView::from_ptr(value, &vm.heap).to_owned(),
  debug: |vm, value| format!("'{}'", StringView::from_ptr(value, &vm.heap)),
  is_falsy: |vm, value| StringView::from_ptr(value, &vm.heap).is_empty(),
  equals: |_vm, _a, _b| unreachable!("Strings handled separately"),
  call: None,
};
pub const STRING_VIEW_TYPE_ID: TypeId = TypeId(1);

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
  debug: |vm, value| vm.heap[value.cast::<Closure>()].to_string(),
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
  debug: |vm, value| vm.heap[value.cast::<NativeFunction>()].to_string(),
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
  debug: |vm, value| vm.heap[value.cast::<NativeClosure>()].to_string(),
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
  debug: |vm, value| vm.heap[value.cast::<NativeClosureTwo>()].to_string(),
  is_falsy: |_, _| false,
  equals: |_vm, a, b| a == b,
  call: Some(|vm, object, arg3| {
    let function = &vm.heap[object.cast::<NativeClosureTwo>()];
    (function.func)(vm, function.arg1, function.arg2, arg3)
  }),
};
pub const NATIVE_CLOSURE_TWO_TYPE_ID: TypeId = TypeId(5);

/// A list of values
#[derive(Clone, Debug)]
pub struct List(GcList<Value>);
impl List {
  pub fn items<'a>(&self, vm: &'a VM) -> &'a [Value] {
    vm.heap.get_list_buffer(self.0)
  }

  pub fn to_string(&self, vm: &VM) -> Result<String, fmt::Error> {
    use std::fmt::Write;

    let mut string = String::new();

    write!(string, "[")?;
    for (i, item) in self.items(vm).iter().enumerate() {
      if i > 0 {
        write!(string, ", ")?;
      }
      write!(string, "{}", item.debug(vm))?;
    }
    write!(string, "]")?;

    Ok(string)
  }
}
impl<T> From<Gc<T>> for List {
  fn from(value: Gc<T>) -> Self {
    Self(value.cast().into())
  }
}
const LIST: TypeDescriptor = TypeDescriptor {
  type_name: "list",
  trace: |vm, value, trace_value| {
    let list = List::from(value);
    for item in vm.heap.get_list_buffer(list.0) {
      trace_value(vm, *item);
    }
  },
  display: |vm, value| List::from(value).to_string(vm).unwrap(),
  debug: |vm, value| List::from(value).to_string(vm).unwrap(),
  is_falsy: |vm, value| List::from(value).items(vm).is_empty(),
  equals: |vm, a, b| {
    let a = List::from(a).items(vm);
    let b = List::from(b).items(vm);

    if a.len() != b.len() {
      return false;
    }

    a.iter().zip(b.iter()).all(|(a, b)| vm.equals(*a, *b))
  },
  call: None,
};
pub const LIST_TYPE_ID: TypeId = TypeId(6);

/// A value which has been moved from the stack to the heap, as it has been captured by a closure
const ALLOCATED: TypeDescriptor = TypeDescriptor {
  type_name: "allocated",
  trace: |vm, value, trace_value| {
    // If the allocated value is a compound value, we may need to trace the value inside
    trace_value(vm, vm.heap[value.cast::<Value>()]);
  },
  display: |_, _| unreachable!("Not accessed as a value"),
  debug: |_, _| unreachable!("Not accessed as a value"),
  is_falsy: |_, _| unreachable!("Not accessed as a value"),
  equals: |_, _, _| unreachable!("Not accessed as a value"),
  call: None,
};
pub const ALLOCATED_TYPE_ID: TypeId = TypeId(7);

#[cfg(test)]
mod test {
  use super::*;
  use crate::{EmptyContext, VM};
  use bang_gc::HeapSize;

  #[test]
  fn type_ids_match() {
    let objects = &DEFAULT_TYPE_DESCRIPTORS;

    assert_eq!(objects[STRING_TYPE_ID.0].type_name, "string");
    assert_eq!(objects[STRING_VIEW_TYPE_ID.0].type_name, "string");
    assert_eq!(objects[CLOSURE_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_FUNCTION_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_CLOSURE_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[NATIVE_CLOSURE_TWO_TYPE_ID.0].type_name, "function");
    assert_eq!(objects[LIST_TYPE_ID.0].type_name, "list");
    assert_eq!(objects[ALLOCATED_TYPE_ID.0].type_name, "allocated");
  }

  #[test]
  fn type_ids_match_in_vm() {
    let mut vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();

    let empty_allocation = vm.heap.allocate(());

    let string = Value::from_object(empty_allocation, STRING_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string).type_name, "string");
    let string_view = Value::from_object(empty_allocation, STRING_VIEW_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string_view).type_name, "string");

    let closure = Value::from_object(empty_allocation, CLOSURE_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(closure).type_name, "function");

    let native_function = Value::from_object(empty_allocation, NATIVE_FUNCTION_TYPE_ID);
    let type_name = vm.get_type_descriptor(native_function).type_name;
    assert_eq!(type_name, "function");
    let native_closure = Value::from_object(empty_allocation, NATIVE_CLOSURE_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(native_closure).type_name, "function");
    let native_closure_two = Value::from_object(empty_allocation, NATIVE_CLOSURE_TWO_TYPE_ID);
    let type_name = vm.get_type_descriptor(native_closure_two).type_name;
    assert_eq!(type_name, "function");

    let list = Value::from_object(empty_allocation, LIST_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(list).type_name, "list");

    let allocated = Value::from_object(empty_allocation, ALLOCATED_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(allocated).type_name, "allocated");
  }
}
