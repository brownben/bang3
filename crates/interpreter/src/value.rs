use super::{
  bytecode::Chunk,
  object::{BangString, Closure, NativeFunction},
};
use bang_gc::{Gc, Heap};
use smartstring::alias::String as SmartString;
use std::{fmt, mem, ptr};

/// A value on the stack of the interpreter.
///
/// It can be:
/// - True
/// - False
/// - Null (only created when an error has occurred)
/// - Number (f64)
/// - A heap pointer [`Gc`], and the type of the object
/// - A static string
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

  /// Is the [Value] a constant string stored in the bytecode?
  #[must_use]
  pub(crate) fn is_constant_string(&self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == CONSTANT_STRING
  }
  /// View the [Value] as a constant string, from the bytecode
  ///
  /// SAFETY: Undefined behaviour if the [Value] is not a constant string.
  /// Use [`Value::is_constant_string`] to check if it is a constant string.
  #[must_use]
  pub(crate) fn as_constant_string(&self) -> &SmartString {
    debug_assert!(self.is_constant_string());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer.cast::<SmartString>() }
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
  #[must_use]
  pub(crate) fn as_constant_function(&self) -> &Chunk {
    debug_assert!(self.is_constant_function());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer.cast::<Chunk>() }
  }

  /// Create a new [Value] from a pointer to the heap
  #[must_use]
  pub(crate) fn from_object<T>(object: Gc<T>, type_: Type) -> Self {
    Self(ptr::without_provenance(
      TO_POINTER | usize::from(type_) << 32 | object.addr() | OBJECT,
    ))
  }
  /// Is the [Value] a pointer to the heap?
  #[must_use]
  pub(crate) fn is_object(self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == OBJECT
  }
  /// Is the [Value] an object of the given [Type]?
  #[must_use]
  pub(crate) fn is_object_type(self, type_: Type) -> bool {
    self.is_object() && self.object_type() == type_
  }
  /// Get the [Type] of a pointer to the heap
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a pointer to the heap.
  /// Use [`Value::is_object`] to check if it is a pointer to the heap.
  #[must_use]
  pub(crate) fn object_type(self) -> Type {
    debug_assert!(self.is_object());

    let raw = self.0.addr() & FROM_POINTER;
    #[expect(
      clippy::cast_possible_truncation,
      reason = "we want truncate the address info"
    )]
    Type::from((raw >> 32) as u16)
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

  /// Is the [Value] a string?
  #[must_use]
  pub fn is_string(&self) -> bool {
    self.is_constant_string() || self.is_object_type(Type::String)
  }
  /// View the [Value] as a string.
  /// It can be a constant string or a string on the heap.
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a string
  /// Use [`Value::is_string`] to check if it is a string
  #[must_use]
  pub(crate) fn as_string<'a>(&'a self, heap: &'a Heap) -> &'a str {
    if self.is_constant_string() {
      self.as_constant_string().as_str()
    } else {
      BangString::from(self.as_object()).as_str(heap)
    }
  }

  /// Is the [Value] a function?
  #[must_use]
  pub fn is_function(&self) -> bool {
    self.is_constant_function() || self.is_object_type(Type::Function)
  }
  /// View the [Value] as a function.
  /// It can be a constant function or a function on the heap.
  ///
  /// SAFETY: Undefined behaviour if [Value] is not a function
  /// Use [`Value::is_function`] to check if it is a function
  #[must_use]
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
      x if x.is_constant_string() => write!(f, "{}", x.as_constant_string()),
      x if x.is_constant_function() => f.write_str(&x.as_constant_function().display()),
      x => write!(f, "{:?}", x.as_object::<()>()),
    }
  }
}

/// Methods which work on a generic Value
impl Value {
  /// Check if two values are equal
  /// Checks if they point the same place, are equal numbers or are equal strings
  #[must_use]
  pub fn equals(self, other: Value, heap: &Heap) -> bool {
    if self == other {
      return true;
    }

    if self.is_number() && other.is_number() {
      return (self.as_number() - other.as_number()).abs() < f64::EPSILON;
    }

    if self.is_string() && other.is_string() {
      return self.as_string(heap) == other.as_string(heap);
    }

    false
  }

  /// Check if the current value is falsy
  /// It is falsy if false, 0, or an empty collection
  #[inline]
  #[must_use]
  pub fn is_falsy(&self, heap: &Heap) -> bool {
    match self {
      Self(TRUE) => false,
      Self(FALSE | NULL) => true,
      x if x.is_number() => (x.as_number() - 0.0).abs() < f64::EPSILON,
      x if x.is_string() => x.as_string(heap).is_empty(),
      _ => false, // all other objects are truthy
    }
  }

  /// Get the type of the value as a string
  #[must_use]
  pub fn get_type(&self) -> &'static str {
    match self {
      Self(TRUE | FALSE) => "boolean",
      Self(NULL) => "null",
      x if x.is_number() => "number",
      x if x.is_constant_string() => "string",
      x if x.is_constant_function() => "function",
      x => match x.object_type() {
        Type::String => "string",
        Type::Closure | Type::Function | Type::NativeFunction => "function",
        Type::Allocated | Type::UpvalueList => unreachable!("not used as value"),
      },
    }
  }

  /// Get the value as a string
  #[must_use]
  pub fn display(&self, heap: &Heap) -> String {
    match self {
      Self(TRUE) => "true".to_owned(),
      Self(FALSE) => "false".to_owned(),
      Self(NULL) => "null".to_owned(),
      x if x.is_number() => x.as_number().to_string(),
      x if x.is_string() => x.as_string(heap).to_owned(),
      x if x.is_function() => x.as_function(heap).display(),
      x if x.is_object_type(Type::NativeFunction) => {
        heap[x.as_object::<NativeFunction>()].to_string()
      }
      x if x.is_object_type(Type::Closure) => {
        format!("{}", heap[x.as_object::<Closure>()])
      }
      _ => unreachable!(),
    }
  }
}

// Wrap raw values into a Value
impl From<bool> for Value {
  fn from(value: bool) -> Self {
    if value {
      Self::TRUE
    } else {
      Self::FALSE
    }
  }
}
impl From<f64> for Value {
  fn from(value: f64) -> Self {
    Self(ptr::without_provenance(value.to_bits().try_into().unwrap()))
  }
}
impl From<*const SmartString> for Value {
  fn from(value: *const SmartString) -> Self {
    Self(
      value
        .map_addr(|ptr| ptr | TO_POINTER | CONSTANT_STRING)
        .cast(),
    )
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

// To check if a value is a number, or a tagged pointer
// Sign bit is not set, but all bits of the exponent, and 2 bits of the mantissa are set (so real NaNs can be represented)
const IS_NUMBER: usize = 0x7FFC_0000_0000_0000;

// Tagged Pointers
const TO_POINTER: usize = 0xFFFF_0000_0000_0000;
const FROM_POINTER: usize = 0x0000_FFFF_FFFF_FFF8;
const TAG: usize = 0x7;
const OBJECT: usize = 1;
const CONSTANT_STRING: usize = 2;
const CONSTANT_FUNCTION: usize = 3;
const _SINGLETONS: usize = 7;

// Singleton Constants
pub const TRUE: *const () = ptr::without_provenance(0xFFFF_0000_0000_0017);
pub const FALSE: *const () = ptr::without_provenance(0xFFFF_0000_0000_0027);
pub const NULL: *const () = ptr::without_provenance(0xFFFF_0000_0000_0037);

/// Marker to indicate the type of a garbage collected object
/// Stored inline with the [Value] for quick access
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u16)]
pub enum Type {
  String,
  Function,
  NativeFunction,
  Closure,
  Allocated,
  UpvalueList,
}
impl From<u16> for Type {
  fn from(value: u16) -> Self {
    unsafe { std::mem::transmute(value) }
  }
}
impl From<Type> for usize {
  fn from(value: Type) -> Self {
    usize::from(value as u16)
  }
}
