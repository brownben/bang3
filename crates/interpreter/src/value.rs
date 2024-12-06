use super::{bytecode::Chunk, vm::VM};
use bang_gc::{Gc, Heap};
use std::{fmt, mem, ptr};

/// A value on the stack of the interpreter.
///
/// It can be:
/// - True
/// - False
/// - Null (only created when an error has occurred)
/// - Number (f64)
/// - A heap pointer [`Gc`], and the type of the object
/// - A static function
///
/// It uses NaN boxing and tagged pointers,
/// to allow us to store all these values within a single pointer.
///
/// It is only valid for the VM for which it was created,
/// and errors may occur when used outside of it.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(*const ());
impl Value {
  /// Boolean true value
  pub const TRUE: Self = Self(TRUE);

  /// Boolean false value
  pub const FALSE: Self = Self(FALSE);

  /// Created when a value is missing, only accessible when an error has occurred
  pub const NULL: Self = Self(NULL);
}

/// Methods to access the underlying data of the Value
impl Value {
  /// Is the [Value] a number?
  #[must_use]
  pub fn is_number(&self) -> bool {
    (self.0.addr() & IS_NUMBER) != IS_NUMBER
  }
  /// View the [Value] as a number
  ///
  /// Use [`Value::is_number`] to check if it is a number.
  #[must_use]
  pub fn as_number(&self) -> f64 {
    debug_assert!(self.is_number());

    // SAFETY: All u64 should be valid f64s
    unsafe { mem::transmute(self.0) }
  }

  /// Is the [Value] a constant function stored in the bytecode?
  #[must_use]
  pub(crate) fn is_constant_function(&self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == CONSTANT_FUNCTION
  }
  /// View the [Value] as a constant function, from the bytecode
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a constant function.
  /// Use [`Value::is_constant_function`] to check if it is a constant function.
  pub(crate) fn as_constant_function(&self) -> &Chunk {
    debug_assert!(self.is_constant_function());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer.cast::<Chunk>() }
  }

  /// Create a new [Value] from a pointer to the heap
  #[must_use]
  pub(crate) fn from_object<T>(object: Gc<T>, type_: TypeId) -> Self {
    Self(ptr::without_provenance(
      TO_POINTER | type_.0 << 32 | object.addr() | OBJECT,
    ))
  }
  /// Is the [Value] a pointer to the heap?
  #[must_use]
  pub(crate) fn is_object(self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == OBJECT
  }
  /// Is the [Value] an object of the given type?
  #[must_use]
  pub(crate) fn is_object_type(self, type_: TypeId) -> bool {
    self.is_object() && self.object_type() == type_
  }
  /// Get the type of a pointer to the heap
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a pointer to the heap.
  /// Use [`Value::is_object`] to check if it is a pointer to the heap.
  #[must_use]
  pub(crate) fn object_type(self) -> TypeId {
    debug_assert!(self.is_object());

    let raw = self.0.addr() & FROM_POINTER;
    TypeId(raw >> 32)
  }
  /// View the [Value] as a object
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a pointer to the heap.
  /// Use [`Value::is_object`] to check if it is a pointer to the heap.
  pub(crate) fn as_object<T>(self) -> Gc<T> {
    debug_assert!(self.is_object());

    let raw = self.0.addr() & FROM_POINTER;
    #[expect(
      clippy::cast_possible_truncation,
      reason = "we want truncate the type info"
    )]
    Gc::new((raw as u32).try_into().unwrap())
  }

  /// View the [Value] as a function.
  /// It can be a constant function or a function on the heap.
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a function
  /// Use [`Value::is_function`] to check if it is a function
  pub(crate) fn as_function<'a>(&'a self, heap: &'a Heap) -> &'a Chunk {
    if self.is_constant_function() {
      self.as_constant_function()
    } else {
      &heap[self.as_object::<Chunk>()]
    }
  }
}
impl fmt::Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self(TRUE) => write!(f, "true"),
      Self(FALSE) => write!(f, "false"),
      Self(NULL) => write!(f, "null"),
      x if x.is_number() => write!(f, "{}", x.as_number()),
      x if x.is_constant_function() => f.write_str(&x.as_constant_function().display()),
      x => write!(f, "{:?}", x.as_object::<()>()),
    }
  }
}

/// Methods which work on a generic Value
impl Value {
  /// Check if the current value is truthy
  ///
  /// It is truthy if not `false`, `0`, or an empty collection
  #[inline]
  #[must_use]
  pub fn is_truthy(&self, vm: &VM) -> bool {
    !self.is_falsy(vm)
  }

  /// Check if the current value is falsy
  ///
  /// It is falsy if `false`, `0`, or an empty collection
  #[inline]
  #[must_use]
  pub fn is_falsy(&self, vm: &VM) -> bool {
    match self {
      Self(TRUE) => false,
      Self(FALSE | NULL) => true,
      x if x.is_number() => (x.as_number() - 0.0).abs() < f64::EPSILON,
      x if x.is_constant_function() => false,
      x => (vm.get_type_descriptor(*x).is_falsy)(vm, x.as_object()),
    }
  }

  /// Check if the current value can be called
  #[inline]
  #[must_use]
  pub fn is_callable(&self, vm: &VM) -> bool {
    if self.is_constant_function() {
      true
    } else if self.is_object() {
      self.object_type() == crate::object::CLOSURE_TYPE_ID
        || vm.get_type_descriptor(*self).call.is_some()
    } else {
      false
    }
  }

  /// Get the type of the value as a string
  #[must_use]
  pub fn get_type(&self, vm: &VM) -> &'static str {
    match self {
      Self(TRUE | FALSE) => "boolean",
      Self(NULL) => "null",
      x if x.is_number() => "number",
      x if x.is_constant_function() => "function",
      x => vm.get_type_descriptor(*x).type_name,
    }
  }

  /// Get the value as a string
  #[must_use]
  pub fn display(&self, vm: &VM) -> String {
    match self {
      Self(TRUE) => "true".to_owned(),
      Self(FALSE) => "false".to_owned(),
      Self(NULL) => "null".to_owned(),
      x if x.is_number() => x.as_number().to_string(),
      x if x.is_constant_function() => x.as_function(&vm.heap).display(),
      x => (vm.get_type_descriptor(*x).display)(vm, x.as_object()),
    }
  }

  /// Get the value as a string
  ///
  /// Prints quotes around strings, for use in compound structures
  #[must_use]
  pub fn debug(&self, vm: &VM) -> String {
    match self {
      Self(TRUE) => "true".to_owned(),
      Self(FALSE) => "false".to_owned(),
      Self(NULL) => "null".to_owned(),
      x if x.is_number() => x.as_number().to_string(),
      x if x.is_constant_function() => x.as_function(&vm.heap).display(),
      x => (vm.get_type_descriptor(*x).debug)(vm, x.as_object()),
    }
  }
}

// Wrap raw values into a Value
impl From<bool> for Value {
  fn from(value: bool) -> Self {
    if value { Self::TRUE } else { Self::FALSE }
  }
}
impl From<f64> for Value {
  fn from(value: f64) -> Self {
    Self(ptr::without_provenance(value.to_bits().try_into().unwrap()))
  }
}
impl From<*const Chunk> for Value {
  fn from(value: *const Chunk) -> Self {
    Self(
      value
        .map_addr(|ptr| ptr | TO_POINTER | CONSTANT_FUNCTION)
        .cast(),
    )
  }
}
impl From<()> for Value {
  fn from((): ()) -> Self {
    Self::NULL
  }
}

/// The id of an object type in the VM
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeId(pub(crate) usize);

// To check if a value is a number, or a tagged pointer
// Sign bit is not set, but all bits of the exponent, and 2 bits of the mantissa are set (so real NaNs can be represented)
const IS_NUMBER: usize = 0x7FFC_0000_0000_0000;

// Tagged Pointers
const TO_POINTER: usize = 0xFFFF_0000_0000_0000;
const FROM_POINTER: usize = 0x0000_FFFF_FFFF_FFF8;
const TAG: usize = 0x7;
const OBJECT: usize = 1;
const CONSTANT_FUNCTION: usize = 2;
const _SINGLETONS: usize = 7;

// Singleton Constants
pub const TRUE: *const () = ptr::without_provenance(0xFFFF_0000_0000_0017);
pub const FALSE: *const () = ptr::without_provenance(0xFFFF_0000_0000_0027);
pub const NULL: *const () = ptr::without_provenance(0xFFFF_0000_0000_0037);

// We store bits at the end of the pointer, to tag different values
// Check that the alignment is greater than 8, so we can use the last 3 bits
const _FUNCTION_ALIGNMENT_ASSERT: () = assert!(mem::align_of::<Chunk>() >= 8);

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn boolean() {
    let true_ = Value::TRUE;
    assert!(!true_.is_number());
    assert!(!true_.is_constant_function());
    assert!(!true_.is_object());
    assert!(!true_.is_string());

    let false_ = Value::FALSE;
    assert!(!false_.is_number());
    assert!(!false_.is_constant_function());
    assert!(!false_.is_object());
    assert!(!false_.is_string());

    assert_eq!(true_, Value::from(true));
    assert_eq!(false_, Value::from(false));
  }

  #[test]
  fn null() {
    let null = Value::NULL;
    assert!(!null.is_number());
    assert!(!null.is_constant_function());
    assert!(!null.is_object());
    assert!(!null.is_string());
  }

  #[test]
  fn number() {
    for number in [0.0, 1.0, 2.0, 4.0, 8.0, 123.0, -0.0, -2.0, 123.45] {
      let num = Value::from(number);
      assert!(num.is_number());
      assert!(!num.is_constant_function());
      assert!(!num.is_object());
      assert!(!num.is_string());

      assert_eq!(num.as_number(), number);
    }

    let num = Value::from(f64::NAN);
    assert!(num.is_number());
    assert!(!num.is_constant_function());
    assert!(!num.is_object());
    assert!(!num.is_string());
    assert!(num.as_number().is_nan());

    let num = Value::from(f64::INFINITY);
    assert!(num.is_number());
    assert!(!num.is_constant_function());
    assert!(!num.is_object());
    assert!(!num.is_string());

    assert_eq!(num.as_number(), f64::INFINITY);

    let num = Value::from(-f64::INFINITY);
    assert!(num.is_number());
    assert!(!num.is_constant_function());
    assert!(!num.is_object());
    assert!(!num.is_string());

    assert_eq!(num.as_number(), -f64::INFINITY);

    let num = Value::from(f64::asin(55.0));
    assert!(num.is_number());
    assert!(!num.is_constant_function());
    assert!(!num.is_object());
    assert!(!num.is_string());
    assert!(num.as_number().is_nan());
  }

  #[test]
  fn constant_function() {
    let function = crate::ChunkBuilder::new("").finalize();
    let function = Value::from(ptr::from_ref(&function));
    assert!(!function.is_number());
    assert!(function.is_constant_function());
    assert!(!function.is_object());
    assert!(!function.is_string());
  }

  #[test]
  fn gc_null() {
    let type_id = TypeId(u16::MAX.into()); // Hopefully an untaken type id
    let gc_null = Value::from_object(Gc::NULL, type_id);
    assert!(!gc_null.is_number());
    assert!(!gc_null.is_constant_function());
    assert!(!gc_null.is_string());
  }
}
