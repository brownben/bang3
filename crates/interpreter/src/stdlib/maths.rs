use super::macros::module;
use crate::{
  VM, Value,
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
  vm::ErrorKind,
};

module!(maths, MATHS_ITEMS, maths_docs, {
  const /// The mathematical constant `π` (~3.141592…)
        PI: f64 = std::f64::consts::PI;
  const /// The mathematical constant `e` (Euler's number, ~2.718281…)
        E: f64 = std::f64::consts::E;
  const /// Floating point infinity (`∞`)
        INFINITY: f64 = f64::INFINITY;
  const /// Not a Number (NaN)
        NAN: f64 = f64::NAN;

  /// Returns the largest integer less than or equal to a number
  ///
  /// ## Example
  /// ```bang
  /// maths::floor(1.2) // 1
  /// maths::floor(1) // 1
  /// maths::floor(-3.4) // -4
  /// ```
  fn floor() = f64_function!(f64::floor);
  /// Returns the smallest integer greater than or equal to a number
  ///
  /// ## Example
  /// ```bang
  /// maths::ceil(1.2) // 2
  /// maths::ceil(1) // 1
  /// maths::ceil(-3.4) // -3
  /// ```
  fn ceil() = f64_function!(f64::ceil);
  /// Returns the closest integer to a number
  /// If a value is half-way between two integers, round away from 0.0.
  ///
  /// ## Example
  /// ```bang
  /// maths::round(1.2) // 1
  /// maths::round(1) // 1
  /// maths::round(-3.4) // -3
  /// maths::round(1.5) // 2
  /// maths::round(-1.5) // -2
  /// ```
  fn round() = f64_function!(f64::round);
  /// Returns the absolute value of a number
  ///
  /// ## Example
  /// ```bang
  /// maths::abs(1.2) // 1.2
  /// maths::abs(-1.2) // 1.2
  /// ```
  fn abs() = f64_function!(f64::abs);
  /// Is the value `NaN`?
  ///
  /// ## Example
  /// ```bang
  /// maths::isNan(1.2) // false
  /// maths::isNan(maths::NAN) // true
  /// ```
  fn isNan() = f64_function!(f64::is_nan);


  /// Computes the sine of a number (in radians)
  ///
  /// ## Example
  /// ```bang
  /// maths::sin(0) // 0
  /// maths::sin(maths::PI / 2) // 1
  /// ```
  fn sin() = f64_function!(f64::sin);
  /// Computes the cosine of a number (in radians)
  ///
  /// ## Example
  /// ```bang
  /// maths::cos(0) // 1
  /// maths::cos(maths::PI / 2) // 0
  /// ```
  fn cos() = f64_function!(f64::cos);
  /// Computes the tangent of a number (in radians)
  ///
  /// ## Example
  /// ```bang
  /// maths::tan(0) // 0
  /// maths::tan(maths::PI / 4) // 1
  /// ```
  fn tan() = f64_function!(f64::tan);
  /// Computes the arcsine of a number
  ///
  /// Return value is in radians in the range [-PI/2, PI/2] or NaN if the number is outside the range [-1, 1].
  ///
  /// ## Example
  /// ```bang
  /// maths::asin(0) // 0
  /// maths::asin(1) // PI / 2
  /// ```
  fn asin() = f64_function!(f64::asin);
  /// Computes the arccosine of a number
  ///
  /// Return value is in radians in the range [0, PI] or NaN if the number is outside the range [-1, 1].
  ///
  /// ## Example
  /// ```bang
  /// maths::acos(1) // 0
  /// maths::acos(0) // PI / 2
  /// ```
  fn acos() = f64_function!(f64::acos);
  /// Computes the arctangent of a number
  ///
  /// Return value is in radians in the range [-PI/2, PI/2].
  ///
  /// ## Example
  /// ```bang
  /// maths::atan(0) // 0
  /// maths::atan(maths::tan(1)) // 1
  /// ```
  fn atan() = f64_function!(f64::atan);

  /// Hyperbolic sine function
  fn sinh() = f64_function!(f64::sinh);
  /// Hyperbolic cosine function
  fn cosh() = f64_function!(f64::cosh);
  /// Hyperbolic tangent function
  fn tanh() = f64_function!(f64::tanh);
  /// Inverse hyperbolic sine function
  fn asinh() = f64_function!(f64::asinh);
  /// Inverse hyperbolic cosine function
  fn acosh() = f64_function!(f64::acosh);
  /// Inverse hyperbolic tangent function
  fn atanh() = f64_function!(f64::atanh);

  /// Returns the square root of a number
  ///
  /// Returns NaN if self is a negative number other than -0.0.
  ///
  /// ## Example
  /// ```bang
  /// maths::sqrt(9) // 3
  /// maths::sqrt(0) // 0
  /// maths::sqrt(-1) >> maths::isNan // true
  /// ```
  fn sqrt() = f64_function!(f64::sqrt);
  /// Returns the cube root of a number
  ///
  /// Returns NaN if self is a negative number other than -0.0.
  ///
  /// ## Example
  /// ```bang
  /// maths::cbrt(8) // 2
  /// maths::cbrt(0) // 0
  /// maths::cbrt(-1) >> maths::isNan // true
  /// ```
  fn cbrt() = f64_function!(f64::cbrt);
  /// Returns e raised to the power of a number
  /// (where e = 2.718281…, the base of natural logarithms)
  ///
  /// ## Example
  /// ```bang
  /// maths::exp(0) // 1
  /// maths::exp(1) // ~2.718281…
  /// ```
  fn exp() = f64_function!(f64::exp);
  /// Returns the natural logarithm of a number
  ///
  /// ## Example
  /// ```bang
  /// maths::ln(1) // 0
  /// maths::ln(maths::E) // 1
  /// ```
  fn ln() = f64_function!(f64::ln);
  /// Raises a number to the power of a number
  ///
  /// ## Example
  /// ```bang
  /// maths::pow(2)(3) // 8
  /// maths::pow(2)(4) // 16
  /// maths::pow(13)(2) // 169
  /// ```
  fn pow() = |vm, arg| {
    fn func(vm: &mut VM, number: Value, power: Value) -> Result<Value, ErrorKind> {
      let result = f64::powf(number.as_number(), get_number(power, vm)?);
      Ok(result.into())
    }

    () = is_number(arg, vm)?;

    let closure = vm.heap.allocate(NativeClosure::new("pow", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
  /// Returns the logarithm of the number with respect to an arbitrary base
  ///
  /// The first argument is the base, and the second is the number.
  ///
  /// ## Example
  /// ```bang
  /// maths::log(2)(8) // 3
  ///
  /// let log10 = maths::log(10)
  /// log10(100) // 2
  /// ```
  fn log() = |vm, arg| {
    fn func(vm: &mut VM, base: Value, number: Value) -> Result<Value, ErrorKind> {
      let result = f64::log(get_number(number, vm)?, base.as_number());
      Ok(result.into())
    }

    () = is_number(arg, vm)?;

    let closure = vm.heap.allocate(NativeClosure::new("log", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Convert an angle from radians to degrees
  ///
  /// ## Example
  /// ```bang
  /// maths::radiansToDegrees(maths::PI) // 180.0
  /// maths::radiansToDegrees(2 * maths::PI) // 360.0
  /// ```
  fn radiansToDegrees() = f64_function!(f64::to_degrees);
  /// Convert an angle from degrees to radians
  ///
  /// ## Example
  /// ```bang
  /// maths::degreesToRadians(180) // PI
  /// maths::degreesToRadians(360) // 2 * PI
  /// ```
  fn degreesToRadians() = f64_function!(f64::to_radians);
});

macro_rules! f64_function {
  ($function:expr) => {
    |vm, arg| Ok($function(get_number(arg, vm)?).into())
  };
}
use f64_function;

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
fn get_number(value: Value, vm: &VM) -> Result<f64, ErrorKind> {
  if value.is_number() {
    Ok(value.as_number())
  } else {
    Err(ErrorKind::TypeError {
      expected: "number",
      got: value.get_type(vm),
    })
  }
}

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
fn is_number(value: Value, vm: &VM) -> Result<(), ErrorKind> {
  if value.is_number() {
    Ok(())
  } else {
    Err(ErrorKind::TypeError {
      expected: "number",
      got: value.get_type(vm),
    })
  }
}
