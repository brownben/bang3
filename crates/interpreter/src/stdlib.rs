//! # Standard library
//! Standard library definition for Bang

use crate::{object::NativeFunction, vm::allocate_string, Context, ImportResult};
use bang_gc::Heap;

/// A context which provides the standard library
#[derive(Clone, Debug)]
pub struct StandardContext;
impl Context for StandardContext {
  fn global_functions(&self) -> Vec<NativeFunction> {
    vec![
      NativeFunction::new("print", |vm, arg| {
        println!("{}", arg.display(vm));
        arg
      }),
      NativeFunction::new("type", |vm, arg| {
        let type_string = arg.get_type(vm);
        allocate_string(&mut vm.heap, type_string)
      }),
    ]
  }

  fn import_value(&self, heap: &mut Heap, module: &str, item: &str) -> ImportResult {
    match module {
      "maths" => maths(heap, item),
      "string" => string(heap, item),
      _ => ImportResult::ModuleNotFound,
    }
  }
}

module!(string, STRING_ITEMS, {
  const NEW_LINE: String = "\n";
  const TAB: String = "\t";
  const CARRIAGE_RETURN: String = "\r";

  fn length(String) -> Number = |s| {
    #[allow(clippy::cast_precision_loss, reason = "max string length < 2^52")]
    { str::len(s) as f64 }
  };

  fn isEmpty(String) -> Boolean = str::is_empty;
  fn isAscii(String) -> Boolean = str::is_ascii;

  fn toLowercase(String) -> String = str::to_lowercase;
  fn toUppercase(String) -> String = str::to_uppercase;
});

module!(maths, MATHS_ITEMS, {
  const PI: Number = std::f64::consts::PI;
  const E: Number = std::f64::consts::E;
  const INFINITY: Number = f64::INFINITY;

  fn floor(Number) -> Number = f64::floor;
  fn ceil(Number) -> Number = f64::ceil;
  fn round(Number) -> Number = f64::round;
  fn abs(Number) -> Number = f64::abs;
  fn sqrt(Number) -> Number = f64::sqrt;
  fn cbrt(Number) -> Number = f64::cbrt;
  fn sin(Number) -> Number = f64::sin;
  fn cos(Number) -> Number = f64::cos;
  fn tan(Number) -> Number = f64::tan;
  fn asin(Number) -> Number = f64::asin;
  fn acos(Number) -> Number = f64::acos;
  fn atan(Number) -> Number = f64::atan;
  fn sinh(Number) -> Number = f64::sinh;
  fn cosh(Number) -> Number = f64::cosh;
  fn tanh(Number) -> Number = f64::tanh;
  fn asinh(Number) -> Number = f64::asinh;
  fn acosh(Number) -> Number = f64::acosh;
  fn atanh(Number) -> Number = f64::atanh;
  fn isNan(Number) -> Number = f64::is_nan;
  fn exp(Number) -> Number = f64::exp;
  fn ln(Number) -> Number = f64::ln;
  fn radiansToDegrees(Number) -> Number = f64::to_degrees;
  fn degreesToRadians(Number) -> Number = f64::to_radians;
});

mod macros {
  use crate::{object::NativeFunction, vm::allocate_string, ImportResult, Value};
  use bang_gc::Heap;

  pub macro module($module_name:ident, $module_items_name:ident, {
    $(const $constant_name:ident : $constant_type:ident = $constant:expr;)*
    $(fn $function_name:ident($function_type:ident) -> $function_return:ident = $function:expr;)*
  }) {
    #[allow(unused_variables)]
    /// A module of Bang's Standard Library
    pub fn $module_name(heap: &mut Heap, item: &str) -> ImportResult {
      match item {
        $(stringify!($constant_name) => wrap_value!($constant_type, heap, $constant),)*
        $(
          stringify!($function_name) => wrap_value!(fn, heap,
            native_function!($function_name, $function_type, $function_return, $function)
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
    (fn, $heap:expr, $native_function:expr) => {
      ImportResult::Value(Value::from_object(
        $heap.allocate($native_function),
        crate::object::NATIVE_FUNCTION_TYPE_ID,
      ))
    },
    (String, $heap:expr, $value:expr) => {
      allocate_string($heap, $value).into()
    },
    (Number, $heap:expr, $value:expr) => {
      $value.into()
    }
  }

  macro native_function {
    ($name:ident, Number, Number, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_number() {
          $native_function(arg.as_number()).into()
        } else {
          Value::NULL
        }
      })
    },

    ($name:ident, String, Number, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          $native_function(arg.as_string(&vm.heap)).into()
        } else {
          Value::NULL
        }
      })
    },

    ($name:ident, String, Boolean, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          $native_function(arg.as_string(&vm.heap)).into()
        } else {
          Value::NULL
        }
      })
    },

    ($name:ident, String, String, $native_function:expr) => {
      NativeFunction::new(stringify!($name), |vm, arg| {
        if arg.is_string() {
          let result = $native_function(arg.as_string(&vm.heap));
          allocate_string(&mut vm.heap, &result).into()
        } else {
          Value::NULL
        }
      })
    },
  }
}
use macros::module;
