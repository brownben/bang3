use crate::{
  value::{Type, Value},
  Chunk, VM,
};
use bang_gc::{Gc, Heap};
use std::{fmt, mem, ops, ptr, slice};

/// A string on the heap
#[repr(C)]
pub struct GcString {
  pub length: u32,
  // Index doesn't work with DST, so we fudge this with pointers to look sized
  // This only works because this never exists on the stack, only in the Bang heap
  // We have to ensure to allocate enough space for the header (this struct) and the buffer
  /* buffer: [u8] */
}
impl GcString {
  pub fn size(length: usize) -> usize {
    mem::size_of::<Self>() + length * mem::size_of::<u8>()
  }

  pub fn buffer(&self) -> &[u8] {
    unsafe {
      let self_ptr = ptr::from_ref(self);
      let buffer_ptr = self_ptr.add(1).cast::<u8>();
      let length = usize::try_from(self.length).unwrap();

      std::slice::from_raw_parts(buffer_ptr, length)
    }
  }
  pub fn buffer_mut(&mut self) -> &mut [u8] {
    unsafe {
      let self_ptr = ptr::from_mut(self);
      let buffer_ptr = self_ptr.add(1).cast::<u8>();
      let length = usize::try_from(self.length).unwrap();

      std::slice::from_raw_parts_mut(buffer_ptr, length)
    }
  }

  pub fn as_str(&self) -> &str {
    unsafe { std::str::from_utf8_unchecked(self.buffer()) }
  }

  /// Concatenate two strings together
  ///
  /// As the heaps may also be borrowed from the heap, we pass the strings as pointers
  pub fn concatenate(
    heap: &mut Heap,
    (left_ptr, left_len): (*const u8, usize),
    (right_ptr, right_len): (*const u8, usize),
  ) -> Value {
    let length = left_len + right_len;

    let new_string_ptr = heap.allocate_bytes(mem::size_of::<u32>() + length);
    let new_string = &mut heap[new_string_ptr.cast::<GcString>()];
    new_string.length = u32::try_from(length).expect("allocation to succeed");

    unsafe {
      let new_string_buffer = new_string.buffer_mut();
      new_string_buffer[..left_len].copy_from_slice(slice::from_raw_parts(left_ptr, left_len));
      new_string_buffer[left_len..].copy_from_slice(slice::from_raw_parts(right_ptr, right_len));
    }

    Value::from_object(new_string_ptr, Type::String)
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
  pub upvalues: Gc<UpvalueList>,
}
impl Closure {
  pub fn new(func: &Chunk, upvalues: Gc<UpvalueList>) -> Self {
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

/// A list of upvalues for a closure
#[repr(C)]
pub struct UpvalueList {
  pub length: usize,
  // Index doesn't work with DST, so we fudge this with pointers to look sized
  // This only works because this never exists on the stack, only in the Bang heap
  // We have to ensure to allocate enough space for the header (this struct) and the buffer
  /* buffer: [Value] */
}
impl UpvalueList {
  pub fn new(heap: &mut Heap, length: u8, iter: impl Iterator<Item = Value>) -> Gc<Self> {
    let upvalue_list_ptr = heap.allocate_bytes(Self::size(length)).cast::<Self>();
    let upvalue_list = &mut heap[upvalue_list_ptr];
    upvalue_list.length = length.into();

    for (index, upvalue) in iter.enumerate() {
      upvalue_list.buffer_mut()[index] = upvalue;
    }

    upvalue_list_ptr
  }

  fn size(length: u8) -> usize {
    mem::size_of::<Self>() + usize::from(length) * mem::size_of::<Value>()
  }

  fn buffer(&self) -> &[Value] {
    unsafe {
      let self_ptr = ptr::from_ref(self);
      let buffer_ptr = self_ptr.add(1).cast::<Value>();
      let length = self.length;

      std::slice::from_raw_parts(buffer_ptr, length)
    }
  }
  fn buffer_mut(&mut self) -> &mut [Value] {
    unsafe {
      let self_ptr = ptr::from_mut(self);
      let buffer_ptr = self_ptr.add(1).cast::<Value>();
      let length = self.length;

      std::slice::from_raw_parts_mut(buffer_ptr, length)
    }
  }

  pub fn iter(&self) -> impl Iterator<Item = &Value> {
    self.buffer().iter()
  }
}
impl ops::Index<u8> for UpvalueList {
  type Output = Value;

  fn index(&self, index: u8) -> &Self::Output {
    &self.buffer()[usize::from(index)]
  }
}
