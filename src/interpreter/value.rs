use crate::collections::{SmallVec, String};
use std::{cell::RefCell, fmt, mem, ptr, rc::Rc};

// Store bits at the end of the pointer, to denote as a pointer rather than NaN
// Force alignment greater than 8, so we can use the last 3 bits
const _ALIGNMENT_ASSERT: () = assert!(std::mem::align_of::<Object>() >= 8);

const TO_STORED: usize =
  0b1111_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0101;
const FROM_STORED: usize =
  0b0000_0000_0000_0000_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1010;

const IS_NUMBER: usize =
  0b0111_1111_1111_1100_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;

const TO_ALLOCATED: usize =
  0b1111_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0011;
const FROM_ALLOCATED: usize =
  0b0000_0000_0000_0000_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1100;

pub const TRUE: *const Object = ptr::without_provenance(
  0b1111_1111_1111_1101_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000,
);
pub const FALSE: *const Object = ptr::without_provenance(
  0b1111_1111_1111_1110_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000,
);
pub const NULL: *const Object = ptr::without_provenance(
  0b1111_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000,
);

/// A basic value in the interpreter
/// Either a boolean, number, or a pointer to a heap allocated [Object]
pub struct Value(*const Object);
impl Value {
  /// A boolean value of `true`
  pub const TRUE: Self = Self(TRUE);
  /// A boolean value of `false`
  pub const FALSE: Self = Self(FALSE);
  /// A null value, only used for error states in the VM
  pub const NULL: Self = Self(NULL);

  /// Check is the current value is a heap allocated [Object]
  #[inline]
  #[must_use]
  pub fn is_object(&self) -> bool {
    (self.0.addr() & TO_STORED) == TO_STORED
  }
  /// View the current value as a heap allocated [Object].
  /// SAFETY: Must check it is a pointer to an object using [`Value::is_object`]
  #[inline]
  pub(crate) fn as_object(&self) -> &Object {
    let pointer = self.0.map_addr(|ptr| ptr & FROM_STORED);
    unsafe { &*pointer }
  }

  /// Allocate a [Value] to the heap, and get a pointer to it stored as a [Value]
  #[must_use]
  pub fn allocate(self) -> Self {
    let memory = Rc::new(RefCell::new(self));

    let pointer = Rc::into_raw(memory);
    let stored_pointer = pointer.map_addr(|ptr| ptr | TO_ALLOCATED);

    Self(stored_pointer.cast::<Object>())
  }
  /// Check is the current value is a pointer to a heap allocated [Value]
  #[inline]
  #[must_use]
  pub fn is_allocated(&self) -> bool {
    (self.0.addr() & TO_ALLOCATED) == TO_ALLOCATED
  }
  /// View the current value as a pointer to a heap allocated [Value]
  /// SAFETY: Must check it is a pointer to an allocated value using [`Value::is_allocated`]
  #[inline]
  pub(crate) fn as_allocated(&self) -> Rc<RefCell<Self>> {
    let pointer = self.0.map_addr(|ptr| ptr & FROM_ALLOCATED);
    let pointer = pointer.cast::<RefCell<Self>>();

    unsafe { Rc::increment_strong_count(pointer) };
    unsafe { Rc::from_raw(pointer) }
  }

  /// Check is the current value is a number
  #[inline]
  #[must_use]
  pub fn is_number(&self) -> bool {
    (self.0.addr() & IS_NUMBER) != IS_NUMBER
  }
  /// View the current value as a number.
  /// Does not check if the value is a number
  #[inline]
  #[must_use]
  pub fn as_number(&self) -> f64 {
    // SAFETY: All u64 should be valid f64s
    unsafe { mem::transmute(self.0) }
  }
}

impl Value {
  /// Check if the current value is falsy
  /// It is falsy if false, 0, or an empty collection
  #[inline]
  #[must_use]
  pub fn is_falsy(&self) -> bool {
    match self {
      Self(TRUE) => false,
      Self(FALSE | NULL) => true,
      a if a.is_number() => (a.as_number() - 0.0).abs() < f64::EPSILON,
      b => b.as_object().is_falsy(),
    }
  }

  /// Get the type of the value
  #[must_use]
  pub fn get_type(&self) -> &'static str {
    match self {
      Self(TRUE | FALSE) => "boolean",
      Self(NULL) => "null",
      a if a.is_number() => "number",
      b => b.as_object().get_type(),
    }
  }
}

impl Clone for Value {
  fn clone(&self) -> Self {
    if self.is_object() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_STORED);

      unsafe { Rc::increment_strong_count(pointer) };
    } else if self.is_allocated() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_ALLOCATED);
      let pointer = pointer.cast::<RefCell<Self>>();

      unsafe { Rc::increment_strong_count(pointer) };
    }

    Self(self.0)
  }
}
impl Drop for Value {
  fn drop(&mut self) {
    if self.is_object() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_STORED);

      unsafe { Rc::decrement_strong_count(pointer) };
    } else if self.is_allocated() {
      let pointer = self.0.map_addr(|ptr| ptr & FROM_ALLOCATED);
      let pointer = pointer.cast::<RefCell<Self>>();

      unsafe { Rc::decrement_strong_count(pointer) };
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    if self.0 == other.0 {
      return true;
    }

    if self.is_number() && other.is_number() {
      let a = self.as_number();
      let b = other.as_number();
      return (a - b).abs() < f64::EPSILON;
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
      a if a.is_object() => write!(f, "{}", a.as_object()),
      a if a.is_allocated() => write!(f, "<allocated {:?}>", a.as_allocated().borrow()),
      _ => unreachable!(),
    }
  }
}

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
    Self(pointer.map_addr(|ptr| ptr | TO_STORED))
  }
}

/// An heap allocated object in the interpreter
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
  String(String),
  Function(Function),
  NativeFunction(fn(&Value) -> Value),
  Closure(Closure),
}
impl Object {
  /// Check if the current object is falsy
  #[inline]
  pub fn is_falsy(&self) -> bool {
    match self {
      Self::String(string) => string.is_empty(),
      Self::Function(_) | Self::NativeFunction(_) | Self::Closure(_) => false,
    }
  }

  /// Get the type of the object
  pub fn get_type(&self) -> &'static str {
    match self {
      Self::String(_) => "string",
      Self::Function(_) | Self::NativeFunction(_) | Self::Closure(_) => "function",
    }
  }
}
impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::String(x) => write!(f, "{x}"),
      Object::Function(x) => x.fmt(f),
      Object::NativeFunction(_) => write!(f, "<function>"),
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
impl From<Function> for Object {
  fn from(value: Function) -> Self {
    Self::Function(value)
  }
}
impl From<Closure> for Object {
  fn from(value: Closure) -> Self {
    Self::Closure(value)
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
  pub(crate) name: String,
  pub(crate) start: usize,
  pub(crate) upvalues: SmallVec<[(u8, ClosureStatus); 8]>,
}
impl Function {
  pub fn new(name: String, start: usize, upvalues: SmallVec<[(u8, ClosureStatus); 8]>) -> Self {
    Self {
      name,
      start,
      upvalues,
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
  pub func: Function,
  pub(crate) upvalues: SmallVec<[Value; 4]>,
}
impl Closure {
  pub fn new(func: Function, upvalues: SmallVec<[Value; 4]>) -> Self {
    Self { func, upvalues }
  }
}
impl fmt::Display for Closure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.func.fmt(f)
  }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum ClosureStatus {
  #[default]
  Open,
  Closed,
  Upvalue,
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn boolean() {
    let true_ = Value::TRUE;
    assert!(!true_.is_number());
    assert!(!true_.is_object());

    assert_eq!(true_, Value::from(true));

    let false_ = Value::FALSE;
    assert!(!false_.is_number());
    assert!(!false_.is_object());

    assert_eq!(false_, Value::from(false));
  }

  #[test]
  fn number() {
    for number in [0.0, 1.0, 2.0, 4.0, 8.0, 123.0, -0.0, -2.0, 123.45] {
      let num = Value::from(number);
      assert!(num.is_number());
      assert!(!num.is_object());
      assert!(!num.is_allocated());
      assert_eq!(num.as_number(), number)
    }

    let num = Value::from(f64::NAN);
    assert!(num.is_number());
    assert!(!num.is_object());
    assert!(!num.is_allocated());
    assert!(num.as_number().is_nan());

    let num = Value::from(f64::INFINITY);
    assert!(num.is_number());
    assert!(!num.is_object());
    assert!(!num.is_allocated());
    assert_eq!(num.as_number(), f64::INFINITY);

    let num = Value::from(f64::asin(55.0));
    assert!(num.is_number());
    assert!(!num.is_object());
    assert!(!num.is_allocated());
    assert!(num.as_number().is_nan());
  }

  #[test]
  fn string() {
    let string = Value::from("hello");
    assert!(string.is_object());
    assert!(!string.is_number());
    assert!(!string.is_allocated());
    assert_eq!(string, Object::String("hello".into()).into());
  }

  #[test]
  fn null() {
    let null = Value::NULL;
    assert!(!null.is_number());
    assert!(!null.is_object());
    assert!(!null.is_allocated());
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

    let function = Function {
      start: 0,
      name: "".into(),
      upvalues: SmallVec::new(),
    };
    assert_eq!(Value::from(function).is_falsy(), false);
  }

  #[test]
  fn allocate() {
    let string = Value::from("hello");
    assert!(string.is_object());
    assert!(!string.is_number());
    assert!(!string.is_allocated());
    assert_eq!(string, Object::String("hello".into()).into());

    let string = string.allocate();
    assert!(string.is_allocated());
    assert!(!string.is_object());
    assert!(!string.is_number());
    assert_eq!(
      *string.as_allocated().borrow(),
      Value::from(Object::String("hello".into()))
    );
    assert_eq!(string.as_allocated().clone(), string.as_allocated().clone(),);
  }
}
