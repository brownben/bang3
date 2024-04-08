use crate::collections::{SmallVec, String};
use std::{cell::RefCell, fmt, mem, ptr, rc::Rc};

/// A value on the stack of the interpreter.
///
/// It can be:
/// - True
/// - False
/// - Null (only created when an error has occurred)
/// - Number (f64)
/// - String (heap allocated or constant from bytecode)
/// - Function (constant from bytecode)
/// - Object (heap allocated)
///
/// It uses NaN boxing and tagged pointers,
/// to allow us to store all these values within a single pointer.
///
/// It is only valid for the VM for which it was created,
/// and errors may occur when used outside of it.
pub struct Value(*const Object);
impl Value {
  /// Boolean true value
  pub const TRUE: Self = Self(TRUE);

  /// Boolean false value
  pub const FALSE: Self = Self(FALSE);

  /// Created when a value is missing, only accessible when an error has occurred
  pub const NULL: Self = Self(NULL);
}
impl Clone for Value {
  fn clone(&self) -> Self {
    if self.is_object() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);

      unsafe { Rc::increment_strong_count(pointer) };
    } else if self.is_allocated() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
      let pointer = pointer.cast::<RefCell<Self>>();

      unsafe { Rc::increment_strong_count(pointer) };
    }

    Self(self.0)
  }
}
impl Drop for Value {
  fn drop(&mut self) {
    if self.is_object() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);

      unsafe { Rc::decrement_strong_count(pointer) };
    } else if self.is_allocated() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
      let pointer = pointer.cast::<RefCell<Self>>();

      unsafe { Rc::decrement_strong_count(pointer) };
    }
  }
}

// Methods to access the underlying data of the Value
impl Value {
  /// Is the [Value] a number?
  #[must_use]
  pub(crate) fn is_number(&self) -> bool {
    (self.0.addr() & IS_NUMBER) != IS_NUMBER
  }
  /// View the [Value] as a number
  ///
  /// # Panics
  /// Panics if the [Value] is not a number.
  /// Use [`Value::is_number`] to check if it is a number.
  #[must_use]
  pub(crate) fn as_number(&self) -> f64 {
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
  /// # Panics
  /// Panics if the [Value] is not a constant string.
  /// Use [`Value::is_constant_string`] to check if it is a constant string.
  #[must_use]
  pub(crate) fn as_constant_string(&self) -> &String {
    debug_assert!(self.is_constant_string());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer.cast::<String>() }
  }

  /// Is the [Value] a [Function]?
  #[must_use]
  pub(crate) fn is_function(&self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == FUNCTION
  }
  /// View the [Value] as a [Function]
  ///
  /// # Panics
  /// Panics if the [Value] is not a [Function].
  /// Use [`Value::is_function`] to check if it is a function.
  #[must_use]
  pub(crate) fn as_function(&self) -> &Function {
    debug_assert!(self.is_function());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer.cast::<Function>() }
  }

  /// Is the [Value] allocated?
  #[must_use]
  pub(crate) fn is_allocated(&self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == ALLOCATED
  }
  /// View the [Value] as an allocated value
  ///
  /// # Panics
  /// Panics if the [Value] is not an allocated value.
  /// Use [`Value::is_allocated`] to check if it is allocated.
  #[must_use]
  pub(crate) fn as_allocated(&self) -> Rc<RefCell<Self>> {
    debug_assert!(self.is_allocated());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    let pointer = pointer.cast::<RefCell<Self>>();

    unsafe { Rc::increment_strong_count(pointer) };
    unsafe { Rc::from_raw(pointer) }
  }

  /// Is the [Value] an [Object]?
  #[must_use]
  pub(crate) fn is_object(&self) -> bool {
    (self.0.addr() & TO_POINTER) == TO_POINTER && self.0.addr() & TAG == OBJECT
  }
  /// View the [Value] as an [Object]
  ///
  /// # Panics
  /// Panics if the [Value] is not an [Object].
  /// Use [`Value::is_object`] to check if it is an object.
  #[must_use]
  pub(crate) fn as_object(&self) -> &Object {
    debug_assert!(self.is_object());

    let pointer = self.0.map_addr(|ptr| ptr & FROM_POINTER);
    unsafe { &*pointer }
  }

  /// Is the [Value] a string?
  ///
  /// It can either be a heap allocated string or a constant string
  #[must_use]
  pub(crate) fn is_string(&self) -> bool {
    if self.is_constant_string() {
      return true;
    }

    if self.is_object() {
      return matches!(self.as_object(), Object::String(_));
    }

    false
  }
  /// View the [Value] as a string
  ///
  /// # Panics
  /// Panics if the [Value] is not a string.
  /// Use [`Value::is_string`] to check if it is a string.
  #[must_use]
  pub(crate) fn as_string(&self) -> &String {
    debug_assert!(self.is_string());

    if self.is_constant_string() {
      self.as_constant_string()
    } else if self.is_object()
      && let Object::String(s) = self.as_object()
    {
      return s;
    } else {
      panic!()
    }
  }

  /// Is the [Value] a [Closure]?
  #[must_use]
  pub(crate) fn is_closure(&self) -> bool {
    self.is_object() && matches!(self.as_object(), Object::Closure(_))
  }
  /// View the [Value] as a [Closure]
  ///
  /// # Panics
  /// Panics if the [Value] is not a [Closure].
  /// Use [`Value::is_closure`] to check if it is a closure.
  #[must_use]
  pub(crate) fn as_closure(&self) -> &Closure {
    debug_assert!(self.is_closure());

    if let Object::Closure(c) = self.as_object() {
      c
    } else {
      panic!()
    }
  }
}

// Methods which work on a generic Value
impl Value {
  /// Check if the current value is falsy
  /// It is falsy if false, 0, or an empty collection
  #[inline]
  #[must_use]
  pub fn is_falsy(&self) -> bool {
    debug_assert!(!self.is_allocated()); // Allocated values are never used as a value

    match self {
      Self(TRUE) => false,
      Self(FALSE | NULL) => true,
      x if x.is_number() => (x.as_number() - 0.0).abs() < f64::EPSILON,
      x if x.is_constant_string() => x.as_constant_string().is_empty(),
      x if x.is_function() => false,
      x => x.as_object().is_falsy(),
    }
  }

  /// Get the type of the value
  #[must_use]
  pub fn get_type(&self) -> &'static str {
    debug_assert!(!self.is_allocated()); // Allocated values are never used as a value

    match self {
      Self(TRUE | FALSE) => "boolean",
      Self(NULL) => "null",
      x if x.is_number() => "number",
      x if x.is_string() => "string",
      x if x.is_function() => "function",
      x => x.as_object().get_type(),
    }
  }

  /// Allocate the value to the heap, so it can be accessed as a closure
  #[must_use]
  pub(crate) fn allocate(self) -> Self {
    let memory = Rc::new(RefCell::new(self));

    let pointer = Rc::into_raw(memory);
    let stored_pointer = pointer.map_addr(|ptr| ptr | TO_POINTER | ALLOCATED);

    Self(stored_pointer.cast::<Object>())
  }
}
impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    debug_assert!(!self.is_allocated()); // Allocated values are never used as a value

    if self.0 == other.0 {
      return true;
    }

    if self.is_number() && other.is_number() {
      return (self.as_number() - other.as_number()).abs() < f64::EPSILON;
    }

    if self.is_string() && other.is_string() {
      return self.as_string() == other.as_string();
    }

    if !self.is_object() || !other.is_object() {
      return false;
    }

    self.as_object() == other.as_object()
  }
}
impl fmt::Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self(TRUE) => write!(f, "true"),
      Self(FALSE) => write!(f, "false"),
      Self(NULL) => write!(f, "null"),
      a if a.is_number() => write!(f, "{}", a.as_number()),
      a if a.is_string() => write!(f, "{}", a.as_string()),
      a if a.is_function() => write!(f, "{}", a.as_function()),
      a if a.is_object() => write!(f, "{}", a.as_object()),
      a if a.is_allocated() => write!(f, "<allocated {:?}>", a.as_allocated().borrow()),
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
    #[allow(clippy::cast_possible_truncation)] // u64 == usize
    Self(ptr::without_provenance(value.to_bits() as usize))
  }
}
impl<T: Into<Object>> From<T> for Value {
  fn from(value: T) -> Self {
    let value = Rc::new(value.into());
    let pointer = Rc::into_raw(value);
    Self(pointer.map_addr(|ptr| ptr | TO_POINTER | OBJECT).cast())
  }
}
impl From<*const String> for Value {
  fn from(value: *const String) -> Self {
    Self(
      value
        .map_addr(|ptr| ptr | TO_POINTER | CONSTANT_STRING)
        .cast(),
    )
  }
}
impl From<*const Function> for Value {
  fn from(value: *const Function) -> Self {
    Self(value.map_addr(|ptr| ptr | TO_POINTER | FUNCTION).cast())
  }
}

/// An heap allocated object in the interpreter
///
/// It can be:
/// - [String]
/// - [Closure]
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
  String(String),
  Closure(Closure),
}
impl Object {
  /// Check if the current object is falsy
  #[inline]
  pub fn is_falsy(&self) -> bool {
    match self {
      Self::String(string) => string.is_empty(),
      Self::Closure(_) => false,
    }
  }

  /// Get the type of the object
  pub fn get_type(&self) -> &'static str {
    match self {
      Self::String(_) => "string",
      Self::Closure(_) => "function",
    }
  }
}
impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::String(x) => write!(f, "{x}"),
      Object::Closure(x) => x.fmt(f),
    }
  }
}
impl From<String> for Object {
  fn from(value: String) -> Self {
    Self::String(value)
  }
}
impl From<&str> for Object {
  fn from(value: &str) -> Self {
    Self::String(value.into())
  }
}
impl From<Closure> for Object {
  fn from(value: Closure) -> Self {
    Self::Closure(value)
  }
}

/// Represents a Function
///
/// Only valid for the [Chunk], which it was generated for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
  pub(crate) name: String,
  pub(crate) start: usize,
}
impl Function {
  pub fn new(name: String, start: usize) -> Self {
    Self { name, start }
  }
}
impl fmt::Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.name.is_empty() {
      write!(f, "<function>")
    } else {
      write!(f, "<function {}>", self.name)
    }
  }
}

/// Represents a Closure
///
/// A function which has captured variables from the surrounding scope.
#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
  func: *const Function,
  pub(crate) upvalues: SmallVec<[Value; 4]>,
}
impl Closure {
  pub fn new(func: *const Function, upvalues: SmallVec<[Value; 4]>) -> Self {
    Self { func, upvalues }
  }

  #[inline]
  pub fn function(&self) -> &Function {
    unsafe { &*self.func }
  }
}
impl fmt::Display for Closure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure {}>", self.function())
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
const FUNCTION: usize = 3;
const ALLOCATED: usize = 4;
const _SINGLETONS: usize = 7;

// Singleton Constants
pub const TRUE: *const Object = ptr::without_provenance(0xFFFF_0000_0000_0017);
pub const FALSE: *const Object = ptr::without_provenance(0xFFFF_0000_0000_0027);
pub const NULL: *const Object = ptr::without_provenance(0xFFFF_0000_0000_0037);

#[cfg(test)]
mod test {
  use super::*;

  // We store bits at the end of the pointer, to tag different values
  // Check that the alignment is greater than 8, so we can use the last 3 bits
  const _OBJECT_ALIGNMENT_ASSERT: () = assert!(std::mem::align_of::<Object>() >= 8);
  const _STRING_ALIGNMENT_ASSERT: () = assert!(std::mem::align_of::<String>() >= 8);
  const _FUNCTION_ALIGNMENT_ASSERT: () = assert!(std::mem::align_of::<Function>() >= 8);

  #[test]
  fn boolean() {
    let true_ = Value::TRUE;
    assert!(!true_.is_number());
    assert!(!true_.is_constant_string());
    assert!(!true_.is_function());
    assert!(!true_.is_allocated());
    assert!(!true_.is_object());
    assert!(!true_.is_string());
    assert!(!true_.is_closure());

    let false_ = Value::FALSE;
    assert!(!false_.is_number());
    assert!(!false_.is_constant_string());
    assert!(!false_.is_function());
    assert!(!false_.is_allocated());
    assert!(!false_.is_object());
    assert!(!false_.is_string());
    assert!(!false_.is_closure());

    assert_eq!(true_, Value::from(true));
    assert_eq!(false_, Value::from(false));
  }

  #[test]
  fn number() {
    for number in [0.0, 1.0, 2.0, 4.0, 8.0, 123.0, -0.0, -2.0, 123.45] {
      let num = Value::from(number);
      assert!(num.is_number());
      assert!(!num.is_constant_string());
      assert!(!num.is_function());
      assert!(!num.is_allocated());
      assert!(!num.is_object());
      assert!(!num.is_string());
      assert!(!num.is_closure());

      assert_eq!(num.as_number(), number)
    }

    let num = Value::from(f64::NAN);
    assert!(num.is_number());
    assert!(!num.is_constant_string());
    assert!(!num.is_function());
    assert!(!num.is_allocated());
    assert!(!num.is_object());
    assert!(!num.is_string());
    assert!(!num.is_closure());
    assert!(num.as_number().is_nan());

    let num = Value::from(f64::INFINITY);
    assert!(num.is_number());
    assert!(!num.is_constant_string());
    assert!(!num.is_function());
    assert!(!num.is_allocated());
    assert!(!num.is_object());
    assert!(!num.is_string());
    assert!(!num.is_closure());
    assert_eq!(num.as_number(), f64::INFINITY);

    let num = Value::from(f64::asin(55.0));
    assert!(num.is_number());
    assert!(!num.is_constant_string());
    assert!(!num.is_function());
    assert!(!num.is_allocated());
    assert!(!num.is_object());
    assert!(!num.is_string());
    assert!(!num.is_closure());
    assert!(num.as_number().is_nan());
  }

  #[test]
  fn string() {
    let string = Value::from("hello");
    assert!(!string.is_number());
    assert!(!string.is_constant_string());
    assert!(!string.is_function());
    assert!(!string.is_allocated());
    assert!(string.is_object());
    assert!(string.is_string());
    assert!(!string.is_closure());

    let raw_constant_string = String::from("hello");
    let constant_string = Value::from(ptr::from_ref(&raw_constant_string));
    assert!(!constant_string.is_number());
    assert!(constant_string.is_constant_string());
    assert!(!constant_string.is_function());
    assert!(!constant_string.is_allocated());
    assert!(!constant_string.is_object());
    assert!(constant_string.is_string());
    assert!(!constant_string.is_closure());

    // same length as max inline string length
    let raw_long_constant_string = String::from("helloworldhelloworld123");
    assert!(raw_long_constant_string.is_inline());
    let long_constant = Value::from(ptr::from_ref(&raw_long_constant_string));
    assert!(!long_constant.is_number());
    assert!(long_constant.is_constant_string());
    assert!(!long_constant.is_function());
    assert!(!long_constant.is_allocated());
    assert!(!long_constant.is_object());
    assert!(long_constant.is_string());
    assert!(!long_constant.is_closure());

    assert_eq!(string, Object::String("hello".into()).into());
    assert_eq!(string, constant_string);
  }

  #[test]
  fn null() {
    let null = Value::NULL;
    assert!(!null.is_number());
    assert!(!null.is_constant_string());
    assert!(!null.is_function());
    assert!(!null.is_allocated());
    assert!(!null.is_object());
    assert!(!null.is_closure());
    assert!(!null.is_string());
  }

  #[test]
  fn function() {
    let function = Function::new("".into(), 0);
    let function = Value::from(ptr::from_ref(&function));
    assert!(!function.is_number());
    assert!(!function.is_constant_string());
    assert!(function.is_function());
    assert!(!function.is_allocated());
    assert!(!function.is_object());
    assert!(!function.is_closure());
    assert!(!function.is_string());
  }

  #[test]
  fn is_falsy() {
    assert_eq!(Value::TRUE.is_falsy(), false);
    assert_eq!(Value::FALSE.is_falsy(), true);
    assert_eq!(Value::NULL.is_falsy(), true);

    assert_eq!(Value::from(0.0).is_falsy(), true);
    assert_eq!(Value::from(-0.0).is_falsy(), true);
    assert_eq!(Value::from(0.01).is_falsy(), false);
    assert_eq!(Value::from(123.0).is_falsy(), false);

    assert_eq!(Value::from("").is_falsy(), true);
    assert_eq!(Value::from("hello").is_falsy(), false);

    let function = Function::new("".into(), 0);
    assert_eq!(Value::from(ptr::from_ref(&function)).is_falsy(), false);
  }

  #[test]
  fn allocate() {
    let string = Value::from("hello");
    assert!(!string.is_number());
    assert!(!string.is_constant_string());
    assert!(!string.is_function());
    assert!(!string.is_allocated());
    assert!(string.is_object());
    assert!(string.is_string());
    assert!(!string.is_closure());
    assert_eq!(string, Object::String("hello".into()).into());

    let string = string.allocate();
    assert!(!string.is_number());
    assert!(!string.is_constant_string());
    assert!(!string.is_function());
    assert!(string.is_allocated());
    assert!(!string.is_object());
    assert!(!string.is_string());
    assert!(!string.is_closure());

    assert_eq!(
      *string.as_allocated().borrow(),
      Value::from(Object::String("hello".into()))
    );
    assert_eq!(string.as_allocated().clone(), string.as_allocated().clone(),);
  }
}
