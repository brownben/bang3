use super::ImportResult;
use crate::{
  object::NativeFunction,
  object::{NativeClosure, NATIVE_CLOSURE_TYPE_ID},
  object::{StringSlice, STRING_SLICE_TYPE_ID},
  value::Value,
  vm::{ErrorKind, VM},
};

pub macro module($module_name:ident, $module_items_name:ident, $module_docs_name:ident, {
  $(
    const $(#[doc = $constant_doc_comment:literal])*
    $constant_name:ident : $constant_type:ident = $constant:expr;
  )*
  $(
    $(#[doc = $doc_comment:literal])*
    fn $function_name:ident($($function_type:ident$(,)?)+) -> $function_return:ident = $function:expr;
  )*
}) {
  #[allow(unused_variables)]
  #[doc = "The `"]
  #[doc = stringify!($module_name)]
  #[doc = "` module of Bang's Standard Library"]
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

  #[doc = "All the items in the `"]
  #[doc = stringify!($module_name)]
  #[doc = "` module"]
  pub const $module_items_name: [&str; ${count($constant_name)} + ${count($function_name)}] = [
    $(stringify!($constant_name),)*
    $(stringify!($function_name),)*
  ];

  #[doc = "Get doc comments for the items in the `"]
  #[doc = stringify!($module_name)]
  #[doc = "` module"]
  #[must_use] pub fn $module_docs_name(item: &str) -> Option<&'static str> {
    match item {
      $(
        stringify!($constant_name) => Some(concat!($($constant_doc_comment, "\n"),*)),
      )*
      $(
        stringify!($function_name) => Some(concat!($($doc_comment, "\n"),*)),
      )*
      _ => None,
    }
  }
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
  },
  (bool, $vm:expr, $value:expr) => {
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
