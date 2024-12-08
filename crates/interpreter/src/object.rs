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
  pub equals: fn(vm: &VM, a: Value, b: Value) -> bool,
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
  LIST_VIEW,
  OPTION_SOME,
  OPTION_NONE,
  ITERATOR,
  ITERATOR_TRANSFORM,
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
  equals: string_equality,
  call: None,
};
pub const STRING_TYPE_ID: TypeId = TypeId(0);

/// A string view - a reference to part of a string
#[derive(Clone, Debug)]
pub struct StringView {
  pub(crate) string: Value,
  pub(crate) start: usize,
  pub(crate) end: usize,
}
impl StringView {
  pub fn new(vm: &VM, string: Value, start: usize, end: usize) -> Self {
    debug_assert!(string.is_string());
    debug_assert!(start <= end);

    if string.is_object_type(STRING_TYPE_ID) {
      return Self { string, start, end };
    }

    let Self {
      string,
      start: old_start,
      ..
    } = vm.heap[string.as_object()];

    Self {
      string,
      start: old_start + start,
      end: old_start + end,
    }
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
  equals: string_equality,
  call: None,
};
pub const STRING_VIEW_TYPE_ID: TypeId = TypeId(1);

impl Value {
  /// Is the [Value] a string?
  #[must_use]
  pub fn is_string(&self) -> bool {
    self.is_object_type(STRING_TYPE_ID) || self.is_object_type(STRING_VIEW_TYPE_ID)
  }
  /// View the [Value] as a string.
  /// It can be a constant string or a string on the heap.
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a string
  /// Use [`Value::is_string`] to check if it is a string
  #[must_use]
  pub(crate) fn as_string(self, heap: &Heap) -> &str {
    if self.is_object_type(STRING_TYPE_ID) {
      BangString::from(self.as_object::<usize>()).as_str(heap)
    } else {
      heap[self.as_object::<StringView>()].as_str(heap)
    }
  }
}
fn string_equality(vm: &VM, a: Value, b: Value) -> bool {
  // `a` must be a string to be dispatched to this function
  debug_assert!(a.is_string());
  if !b.is_string() {
    return false;
  }

  a.as_string(&vm.heap) == b.as_string(&vm.heap)
}

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
  pub fn with_capacity(heap: &mut Heap, capacity: usize) -> Self {
    let new_list = heap.allocate_list_header::<Value>(capacity);
    heap[*new_list] = 0;
    Self(new_list)
  }

  pub fn length(&self, heap: &Heap) -> usize {
    heap[*self.0]
  }

  pub fn is_empty(&self, heap: &Heap) -> bool {
    heap[*self.0] == 0
  }

  pub fn push(self, heap: &mut Heap, new_item: Value) -> Self {
    let capacity = self.0.capacity(heap);
    let current_length = self.length(heap);

    // If the item fits in the list, just add it to the list
    if current_length < capacity {
      heap[*self.0] = current_length + 1;
      heap.get_list_buffer_mut(self.0)[current_length] = new_item;
      return self;
    }

    // Else we need to allocate a new list with a bigger capacity
    let new_capacity = capacity * 2;
    let new_list = Self::with_capacity(heap, new_capacity);
    heap[*new_list.0] = current_length + 1;

    // Copy the old list to the new list
    let old_list_ptr = heap.get_list_buffer_ptr(self.0);
    let old_list_buffer = unsafe { slice::from_raw_parts(old_list_ptr, current_length) };
    let new_list_buffer = heap.get_list_buffer_mut(new_list.0);
    new_list_buffer[..current_length].copy_from_slice(old_list_buffer);

    // add the new item to the list
    new_list_buffer[current_length] = new_item;

    new_list
  }

  pub fn items<'a>(&self, heap: &'a Heap) -> &'a [Value] {
    heap.get_list_buffer(self.0)
  }

  pub fn as_ptr(&self) -> Gc<usize> {
    *self.0
  }
}
impl From<Gc<u8>> for List {
  fn from(value: Gc<u8>) -> Self {
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
  display: |vm, value| list_display(vm, List::from(value).items(&vm.heap)),
  debug: |vm, value| list_display(vm, List::from(value).items(&vm.heap)),
  is_falsy: |vm, value| List::from(value).is_empty(&vm.heap),
  equals: list_equality,
  call: None,
};
pub const LIST_TYPE_ID: TypeId = TypeId(6);

/// A list view - a reference to part of a list
#[derive(Clone)]
pub struct ListView {
  pub(crate) list: Value,
  pub(crate) start: usize,
  pub(crate) end: usize,
}
impl ListView {
  pub fn new(vm: &VM, list: Value, start: usize, end: usize) -> Self {
    debug_assert!(list.is_object_type(LIST_TYPE_ID) || list.is_object_type(LIST_VIEW_TYPE_ID));
    debug_assert!(start <= end);

    if list.is_object_type(LIST_TYPE_ID) {
      return Self { list, start, end };
    }

    let Self {
      list,
      start: old_start,
      ..
    } = vm.heap[list.as_object()];

    Self {
      list,
      start: old_start + start,
      end: old_start + end,
    }
  }

  pub fn from_ptr<T>(ptr: Gc<T>, heap: &Heap) -> &ListView {
    &heap[ptr.cast::<ListView>()]
  }

  pub fn is_empty(&self, vm: &VM) -> bool {
    self.items(&vm.heap).is_empty()
  }

  pub fn items<'a>(&'a self, heap: &'a Heap) -> &'a [Value] {
    &self.list.as_list(heap)[self.start..self.end]
  }
}
const LIST_VIEW: TypeDescriptor = TypeDescriptor {
  type_name: "list",
  trace: |vm, value, trace_value| {
    let view = &vm.heap[value.cast::<ListView>()];
    trace_value(vm, view.list);
  },
  display: |vm, value| list_display(vm, ListView::from_ptr(value, &vm.heap).items(&vm.heap)),
  debug: |vm, value| list_display(vm, ListView::from_ptr(value, &vm.heap).items(&vm.heap)),
  is_falsy: |vm, value| ListView::from_ptr(value, &vm.heap).is_empty(vm),
  equals: list_equality,
  call: None,
};
pub const LIST_VIEW_TYPE_ID: TypeId = TypeId(7);

impl Value {
  /// Is the [Value] a list?
  #[must_use]
  pub fn is_list(&self) -> bool {
    self.is_object_type(LIST_TYPE_ID) || self.is_object_type(LIST_VIEW_TYPE_ID)
  }
  /// View the [Value] as a list.
  /// It can be a list, or a list view
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a string
  /// Use [`Value::is_list`] to check if it is a string
  #[must_use]
  pub(crate) fn as_list(self, heap: &Heap) -> &[Value] {
    debug_assert!(self.is_list());

    if self.is_object_type(LIST_TYPE_ID) {
      List::from(self.as_object()).items(heap)
    } else {
      heap[self.as_object::<ListView>()].items(heap)
    }
  }
}
fn list_equality(vm: &VM, a: Value, b: Value) -> bool {
  // `a` must be a list to be dispatched to this function
  debug_assert!(a.is_list());
  if !b.is_list() {
    return false;
  }

  let a = a.as_list(&vm.heap);
  let b = b.as_list(&vm.heap);

  if a.len() != b.len() {
    return false;
  }

  a.iter().zip(b.iter()).all(|(a, b)| vm.equals(*a, *b))
}
fn list_display(vm: &VM, list: &[Value]) -> String {
  fn inner(vm: &VM, list: &[Value]) -> Result<String, fmt::Error> {
    use std::fmt::Write;

    let mut string = String::new();

    write!(string, "[")?;
    for (i, item) in list.iter().enumerate() {
      if i > 0 {
        write!(string, ", ")?;
      }
      write!(string, "{}", item.debug(vm))?;
    }
    write!(string, "]")?;

    Ok(string)
  }

  inner(vm, list).unwrap()
}

const OPTION_SOME: TypeDescriptor = TypeDescriptor {
  type_name: "option::Some",
  trace: |vm, value, trace_value| trace_value(vm, vm.heap[value.cast::<Value>()]),
  display: |vm, value| format!("Some({})", vm.heap[value.cast::<Value>()].debug(vm)),
  debug: |vm, value| format!("Some({})", vm.heap[value.cast::<Value>()].debug(vm)),
  is_falsy: |_, _| false,
  equals: |vm, a, b| {
    if !b.is_object_type(SOME_TYPE_ID) {
      return false;
    }

    vm.equals(
      vm.heap[a.as_object::<Value>()],
      vm.heap[b.as_object::<Value>()],
    )
  },
  call: None,
};
pub const SOME_TYPE_ID: TypeId = TypeId(8);

const OPTION_NONE: TypeDescriptor = TypeDescriptor {
  type_name: "option::None",
  trace: |_, _, _| {},
  display: |_, _| "None".to_owned(),
  debug: |_, _| "None".to_owned(),
  is_falsy: |_, _| true,
  equals: |_, a, b| a == b,
  call: None,
};
pub const NONE_TYPE_ID: TypeId = TypeId(9);

pub const NONE: Value = Value::from_object(Gc::NULL, NONE_TYPE_ID);

pub struct Iterator {
  pub base: Value,
  pub next: fn(&mut VM, state: usize, base: Value) -> IteratorReturn,

  pub length: IteratorLength,
}
const ITERATOR: TypeDescriptor = TypeDescriptor {
  type_name: "iterator",
  trace: |vm, value, trace_value| {
    let iterator = &vm.heap[value.cast::<Iterator>()];
    trace_value(vm, iterator.base);
  },
  display: |_, _| "<iterator>".to_owned(),
  debug: |_, _| "<iterator>".to_owned(),
  is_falsy: |_, _| false,
  equals: |_, a, b| a == b,
  call: None,
};
pub const ITERATOR_TYPE_ID: TypeId = TypeId(10);

#[derive(Debug)]
pub struct IteratorTransform {
  pub iterator: Value,
  pub arg: Value,
  pub next: IteratorTransformNextFunction,

  pub length: IteratorLength,
}
const ITERATOR_TRANSFORM: TypeDescriptor = TypeDescriptor {
  type_name: "iterator",
  trace: |vm, value, trace_value| {
    let transform = &vm.heap[value.cast::<IteratorTransform>()];
    trace_value(vm, transform.arg);
    trace_value(vm, transform.iterator);
  },
  display: |_, _| "<iterator>".to_owned(),
  debug: |_, _| "<iterator>".to_owned(),
  is_falsy: |_, _| false,
  equals: |_, a, b| a == b,
  call: None,
};
pub const ITERATOR_TRANSFORM_TYPE_ID: TypeId = TypeId(11);

type IteratorReturn = Option<(Value, usize)>;
type IteratorTransformNextFunction = fn(
  &mut VM,
  iterator: Value,
  state: usize,
  transfom_arg: Value,
) -> Result<IteratorReturn, ErrorKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IteratorLength {
  Infinite,
  Unknown,
  Max(usize),
}

impl Value {
  /// Is the [Value] an iterator?
  #[must_use]
  pub fn is_iterator(&self) -> bool {
    self.is_object_type(ITERATOR_TYPE_ID) || self.is_object_type(ITERATOR_TRANSFORM_TYPE_ID)
  }
}

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
pub const ALLOCATED_TYPE_ID: TypeId = TypeId(12);

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
    assert_eq!(objects[LIST_VIEW_TYPE_ID.0].type_name, "list");
    assert_eq!(objects[SOME_TYPE_ID.0].type_name, "option::Some");
    assert_eq!(objects[NONE_TYPE_ID.0].type_name, "option::None");
    assert_eq!(objects[ITERATOR_TYPE_ID.0].type_name, "iterator");
    assert_eq!(objects[ITERATOR_TRANSFORM_TYPE_ID.0].type_name, "iterator");
    assert_eq!(objects[ALLOCATED_TYPE_ID.0].type_name, "allocated");
  }

  #[test]
  fn type_ids_match_in_vm() {
    let mut vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();

    let empty_allocation = vm.heap.allocate(());
    let get_type_name = |value| vm.get_type_descriptor(value).type_name;

    let string = Value::from_object(empty_allocation, STRING_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string).type_name, "string");
    let string_view = Value::from_object(empty_allocation, STRING_VIEW_TYPE_ID);
    assert_eq!(vm.get_type_descriptor(string_view).type_name, "string");

    let closure = Value::from_object(empty_allocation, CLOSURE_TYPE_ID);
    assert_eq!(get_type_name(closure), "function");

    let native_function = Value::from_object(empty_allocation, NATIVE_FUNCTION_TYPE_ID);
    assert_eq!(get_type_name(native_function), "function");
    let native_closure = Value::from_object(empty_allocation, NATIVE_CLOSURE_TYPE_ID);
    assert_eq!(get_type_name(native_closure), "function");
    let native_closure_two = Value::from_object(empty_allocation, NATIVE_CLOSURE_TWO_TYPE_ID);
    assert_eq!(get_type_name(native_closure_two), "function");

    let list = Value::from_object(empty_allocation, LIST_TYPE_ID);
    assert_eq!(get_type_name(list), "list");
    let list_view = Value::from_object(empty_allocation, LIST_VIEW_TYPE_ID);
    assert_eq!(get_type_name(list_view), "list");

    let some = Value::from_object(empty_allocation, SOME_TYPE_ID);
    assert_eq!(get_type_name(some), "option::Some");
    let none = Value::from_object(empty_allocation, NONE_TYPE_ID);
    assert_eq!(get_type_name(none), "option::None");

    let iterator = Value::from_object(empty_allocation, ITERATOR_TYPE_ID);
    assert_eq!(get_type_name(iterator), "iterator");
    let iterator_transform = Value::from_object(empty_allocation, ITERATOR_TRANSFORM_TYPE_ID);
    assert_eq!(get_type_name(iterator_transform), "iterator");

    let allocated = Value::from_object(empty_allocation, ALLOCATED_TYPE_ID);
    assert_eq!(get_type_name(allocated), "allocated");
  }
}
