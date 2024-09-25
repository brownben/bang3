use crate::{
  value::{Type, Value},
  Chunk, VM,
};
use bang_gc::{Gc, GcList, Heap};
use std::{fmt, ptr, slice, str};

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

    Value::from_object(*new_string, Type::String)
  }
}
impl From<Gc<usize>> for BangString {
  fn from(value: Gc<usize>) -> Self {
    Self(value.into())
  }
}
impl From<GcList<u8>> for BangString {
  fn from(value: GcList<u8>) -> Self {
    Self(value)
  }
}

/// A native function
#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunction {
  pub(crate) name: &'static str,
  pub(crate) func: fn(&mut VM, Value) -> Value,
}
impl NativeFunction {
  /// Create a new naative function
  pub fn new(name: &'static str, func: fn(&mut VM, Value) -> Value) -> Self {
    Self { name, func }
  }
}
impl fmt::Display for NativeFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<function {}>", self.name)
  }
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
