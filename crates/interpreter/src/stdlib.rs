//! # Standard library
//! Standard library definition for Bang

use crate::{object::NativeFunction, Value, VM};
use std::fmt;

/// A context is the environment in which the VM runs.
///
/// It is responsible for defining the global functions which can be called,
/// and for importing values from modules.
pub trait Context: fmt::Debug {
  /// Defines the functions which should be registered globally for the VM
  fn global_functions(&self) -> Vec<NativeFunction> {
    Vec::new()
  }

  /// Import a value from a module
  fn import_value(&self, vm: &mut VM, module: &str, item: &str) -> ImportResult {
    let (_vm, _module, _item) = (vm, module, item);
    ImportResult::ModuleNotFound
  }
}

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Value(Value),
  /// The module was not found
  ModuleNotFound,
  /// The item was not found in the module
  ItemNotFound,
}
impl<T: Into<Value>> From<T> for ImportResult {
  fn from(value: T) -> Self {
    ImportResult::Value(value.into())
  }
}

/// An empty context, where nothing can be imported and has no global function
#[derive(Clone, Debug)]
pub struct EmptyContext;
impl Context for EmptyContext {}

/// A context which provides the standard library
#[derive(Clone, Debug)]
pub struct StandardContext;
impl Context for StandardContext {
  fn global_functions(&self) -> Vec<NativeFunction> {
    vec![
      NativeFunction::new("print", |vm, arg| {
        println!("{}", arg.display(vm));
        Ok(arg)
      }),
      NativeFunction::new("type", |vm, arg| {
        let type_string = arg.get_type(vm);
        Ok(vm.allocate_string(type_string))
      }),
    ]
  }

  fn import_value(&self, vm: &mut VM, module: &str, item: &str) -> ImportResult {
    match module {
      "maths" => maths(vm, item),
      "string" => string(vm, item),
      _ => ImportResult::ModuleNotFound,
    }
  }
}

/// The names of all the modules in the standard library
pub const MODULES: [&str; 2] = ["maths", "string"];

module!(string, STRING_ITEMS, {
  const NEW_LINE: String = "\n";
  const TAB: String = "\t";
  const CARRIAGE_RETURN: String = "\r";

  fn length(String) -> usize = |s: &str| s.chars().count();
  fn byteLength(String) -> usize = str::len;

  fn isEmpty(String) -> bool = str::is_empty;
  fn isAscii(String) -> bool = str::is_ascii;

  fn toLowercase(String) -> String = str::to_lowercase;
  fn toUppercase(String) -> String = str::to_uppercase;

  fn contains(String, String) -> bool = |pattern, string| str::contains(string, pattern);
  fn startsWith(String, String) -> bool = |pattern, string| str::starts_with(string, pattern);
  fn endsWith(String, String) -> bool = |pattern, string| str::ends_with(string, pattern);

  fn trim(String) -> StringSlice = str::trim;
  fn trimStart(String) -> StringSlice = str::trim_start;
  fn trimEnd(String) -> StringSlice = str::trim_end;
});

module!(maths, MATHS_ITEMS, {
  const PI: f64 = std::f64::consts::PI;
  const E: f64 = std::f64::consts::E;
  const INFINITY: f64 = f64::INFINITY;

  fn floor(Number) -> f64 = f64::floor;
  fn ceil(Number) -> f64 = f64::ceil;
  fn round(Number) -> f64 = f64::round;
  fn abs(Number) -> f64 = f64::abs;
  fn isNan(Number) -> f64 = f64::is_nan;

  fn sin(Number) -> f64 = f64::sin;
  fn cos(Number) -> f64 = f64::cos;
  fn tan(Number) -> f64 = f64::tan;
  fn asin(Number) -> f64 = f64::asin;
  fn acos(Number) -> f64 = f64::acos;
  fn atan(Number) -> f64 = f64::atan;
  fn sinh(Number) -> f64 = f64::sinh;
  fn cosh(Number) -> f64 = f64::cosh;
  fn tanh(Number) -> f64 = f64::tanh;
  fn asinh(Number) -> f64 = f64::asinh;
  fn acosh(Number) -> f64 = f64::acosh;
  fn atanh(Number) -> f64 = f64::atanh;


  fn sqrt(Number) -> f64 = f64::sqrt;
  fn cbrt(Number) -> f64 = f64::cbrt;
  fn exp(Number) -> f64 = f64::exp;
  fn ln(Number) -> f64 = f64::ln;
  fn pow(Number, Number) -> f64 = f64::powf;
  fn log(Number, Number) -> f64 = |base, num| f64::log(num, base);

  fn radiansToDegrees(Number) -> f64 = f64::to_degrees;
  fn degreesToRadians(Number) -> f64 = f64::to_radians;
});

mod macros {
  use super::ImportResult;
  use crate::{
    object::NativeFunction,
    object::{NativeClosure, NATIVE_CLOSURE_TYPE_ID},
    object::{StringSlice, STRING_SLICE_TYPE_ID},
    value::Value,
    vm::{ErrorKind, VM},
  };

  pub macro module($module_name:ident, $module_items_name:ident, {
    $(const $constant_name:ident : $constant_type:ident = $constant:expr;)*
    $(fn $function_name:ident($($function_type:ident$(,)?)+) -> $function_return:ident = $function:expr;)*
  }) {
    #[allow(unused_variables)]
    /// A module of Bang's Standard Library
    pub fn $module_name(vm: &mut VM, item: &str) -> ImportResult {
      match item {
        $(stringify!($constant_name) => wrap_value!($constant_type, vm, $constant),)*
        $(
          stringify!($function_name) => wrap_value!(fn, vm,
            native_function!($function_name, $($function_type)+, $function_return, $function)
          ),
        )*
        _ => ImportResult::ItemNotFound,
      }
    }
    /// All the items in the module
    pub const $module_items_name: [&str; ${count($constant_name)} + ${count($function_name)}] = [
      $(stringify!($constant_name),)*
      $(stringify!($function_name),)*
    ];
  }

  macro wrap_value {
    (fn, $vm:expr, $native_function:expr) => {
      ImportResult::Value(Value::from_object(
        $vm.heap.allocate($native_function),
        crate::object::NATIVE_FUNCTION_TYPE_ID,
      ))
    },
    (String, $vm:expr, $value:expr) => {
      $vm.allocate_string($value).into()
    },
    (f64, $vm:expr, $value:expr) => {
      $value.into()
    }
  }

  macro native_function {
    ($name:ident, Number, f64, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_number() {
          Ok($native_function(arg.as_number()).into())
        } else {
          Err(ErrorKind::TypeError { expected: "number", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, Number Number, f64, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_number() {
          fn func(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
            if b.is_number() {
              Ok($native_function(a.as_number(), b.as_number()).into())
            } else {
              Err(ErrorKind::TypeError { expected: "number", got: b.get_type(vm) })
            }
          }

          let closure = vm.heap.allocate(NativeClosure::new(stringify!($name), func, arg));
          Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
        } else {
          Err(ErrorKind::TypeError { expected: "number", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, String, usize, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          let result = $native_function(arg.as_string(&vm.heap));
          #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
          Ok((result as f64).into())
        } else {
          Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, String, bool, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          Ok($native_function(arg.as_string(&vm.heap)).into())
        } else {
          Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, String, String, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          let result = $native_function(arg.as_string(&vm.heap));
          Ok(vm.allocate_string(&result).into())
        } else {
          Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, String, StringSlice, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          let arg_string = arg.as_string(&vm.heap);
          let result = $native_function(arg_string);

          let start = result.as_ptr().addr() - arg_string.as_ptr().addr();
          let end = start + result.len();

          if start == 0 && end == arg_string.len() {
            Ok(arg)
          } else {
            let closure = vm.heap.allocate(StringSlice::new(arg, start, end));
            Ok(Value::from_object(closure, STRING_SLICE_TYPE_ID))
          }
        } else {
          Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) })
        }
      })
    },

    ($name:ident, String String, bool, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          fn func(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
            if b.is_string() {
              Ok($native_function(a.as_string(&vm.heap), b.as_string(&vm.heap)).into())
            } else {
              Err(ErrorKind::TypeError { expected: "string", got: b.get_type(vm) })
            }
          }

          let closure = vm.heap.allocate(NativeClosure::new(stringify!($name), func, arg));
          Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
        } else {
          Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) })
        }
      })
    },
  }
}
use macros::module;
