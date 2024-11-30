use crate::{
  object::{LIST_TYPE_ID, List},
  value::Value,
  vm::{ErrorKind, VM},
};

/// A prelude for the macros, all the items that are required to be in scope for the macros to work.
pub(crate) mod prelude {
  pub(crate) use super::super::ImportResult;
  pub(crate) use super::{count, get_list, get_number, get_string, native_function, wrap_value};
  pub(crate) use crate::{
    object::NativeFunction,
    object::{NATIVE_CLOSURE_TWO_TYPE_ID, NativeClosureTwo},
    object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
    object::{STRING_SLICE_TYPE_ID, StringSlice},
    value::Value,
    vm::{ErrorKind, VM},
  };
}

macro_rules! module {
  ($module_name:ident, $module_items_name:ident, $module_docs_name:ident, {
    $(
      const $(#[doc = $constant_doc_comment:literal])*
      $constant_name:ident : $constant_type:ident = $constant:expr;
    )*
    $(
      $(#[doc = $doc_comment:literal])*
      fn $function_name:ident($($function_type:ident$(,)?)+) -> $function_return:ident = $function:expr;
    )*
  }) => {
    #[allow(unused_variables)]
    #[doc = concat!("The `", stringify!($module_name), "` module of Bang's Standard Library")]
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

    #[doc = concat!("All the items in the `", stringify!($module_name), "` module")]
    pub const $module_items_name: [&str; count!($($constant_name)*) + count!($($function_name)*)] = [
      $(stringify!($constant_name),)*
      $(stringify!($function_name),)*
    ];

    #[doc = concat!("Get doc comments for the items in the `", stringify!($module_name), "` module")]
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
}
pub(crate) use module;

macro_rules! wrap_value {
  (fn, $vm:expr, $native_function:expr) => {
    ImportResult::Value(Value::from_object(
      $vm.heap.allocate($native_function),
      crate::object::NATIVE_FUNCTION_TYPE_ID,
    ))
  };
  (String, $vm:expr, $value:expr) => {
    $vm.allocate_string($value).into()
  };
  (f64, $vm:expr, $value:expr) => {
    $value.into()
  };
  (bool, $vm:expr, $value:expr) => {
    $value.into()
  };
}
pub(crate) use wrap_value;

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
pub(crate) fn get_number(value: Value, vm: &VM) -> Result<f64, ErrorKind> {
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
pub(crate) fn get_string<'a>(value: Value, vm: &'a VM) -> Result<&'a str, ErrorKind> {
  if value.is_string() {
    Ok(value.as_string(&vm.heap))
  } else {
    Err(ErrorKind::TypeError {
      expected: "string",
      got: value.get_type(vm),
    })
  }
}

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
pub(crate) fn get_list<'a>(value: Value, vm: &'a VM) -> Result<&'a [Value], ErrorKind> {
  if value.is_object_type(LIST_TYPE_ID) {
    Ok(List::from(value.as_object::<usize>()).items(vm))
  } else {
    Err(ErrorKind::TypeError {
      expected: "list",
      got: value.get_type(vm),
    })
  }
}

macro_rules! native_function {
  ($name:ident, Number, f64, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      Ok($native_function(get_number(arg, vm)?).into())
    })
  };

  ($name:ident, Number Number, f64, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      fn func(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
        if b.is_number() {
          Ok($native_function(a.as_number(), b.as_number()).into())
        } else {
          Err(ErrorKind::TypeError {
            expected: "number",
            got: b.get_type(vm),
          })
        }
      }

      if !arg.is_number() {
        return Err(ErrorKind::TypeError {
          expected: "number",
          got: arg.get_type(vm),
        });
      }

      let closure = vm
        .heap
        .allocate(NativeClosure::new(stringify!($name), func, arg));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
    })
  };

  ($name:ident, String, usize, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      let result = $native_function(get_string(arg, vm)?);
      #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
      Ok((result as f64).into())
    })
  };

  ($name:ident, String, bool, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      Ok($native_function(get_string(arg, vm)?).into())
    })
  };

  ($name:ident, String, String, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      let result = $native_function(get_string(arg, vm)?);
      Ok(vm.allocate_string(&result).into())
    })
  };

  ($name:ident, String, StringSlice, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      let arg_string = get_string(arg, vm)?;
      let result = $native_function(arg_string);

      let start = result.as_ptr().addr() - arg_string.as_ptr().addr();
      let end = start + result.len();

      if start == 0 && end == arg_string.len() {
        Ok(arg)
      } else {
        let closure = vm.heap.allocate(StringSlice::new(arg, start, end));
        Ok(Value::from_object(closure, STRING_SLICE_TYPE_ID))
      }
    })
  };

  ($name:ident, String String, bool, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      fn func(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
        if b.is_string() {
          Ok($native_function(a.as_string(&vm.heap), b.as_string(&vm.heap)).into())
        } else {
          Err(ErrorKind::TypeError {
            expected: "string",
            got: b.get_type(vm),
          })
        }
      }

      if !arg.is_string() {
        return Err(ErrorKind::TypeError {
          expected: "string",
          got: arg.get_type(vm),
        });
      }

      let closure = (vm.heap).allocate(NativeClosure::new(stringify!($name), func, arg));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
    })
  };

  ($name:ident, String String String, String, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      fn func_one(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
        if b.is_string() {
          let closure =
            (vm.heap).allocate(NativeClosureTwo::new(stringify!($name), func_two, a, b));
          Ok(Value::from_object(closure, NATIVE_CLOSURE_TWO_TYPE_ID))
        } else {
          Err(ErrorKind::TypeError {
            expected: "string",
            got: b.get_type(vm),
          })
        }
      }
      fn func_two(vm: &mut VM, a: Value, b: Value, c: Value) -> Result<Value, ErrorKind> {
        if c.is_string() {
          let output = $native_function(
            a.as_string(&vm.heap),
            b.as_string(&vm.heap),
            c.as_string(&vm.heap),
          );
          Ok(vm.allocate_string(&output).into())
        } else {
          Err(ErrorKind::TypeError {
            expected: "string",
            got: c.get_type(vm),
          })
        }
      }

      if !arg.is_string() {
        return Err(ErrorKind::TypeError {
          expected: "string",
          got: arg.get_type(vm),
        });
      }

      let closure = (vm.heap).allocate(NativeClosure::new(stringify!($name), func_one, arg));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
    })
  };

  ($name:ident, Any, StringOrSelf, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      if arg.is_string() {
        return Ok(arg);
      }

      let result = $native_function(vm, arg);
      Ok(vm.allocate_string(&result).into())
    })
  };

  ($name:ident, List, usize, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      let result = $native_function(get_list(arg, vm)?);
      #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
      Ok((result as f64).into())
    })
  };
  ($name:ident, List, bool, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      Ok($native_function(get_list(arg, vm)?).into())
    })
  };
  ($name:ident, Any List, bool, $native_function:expr) => {
    NativeFunction::new(stringify!($name), |vm, arg| {
      fn func(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
        Ok($native_function(vm, a, get_list(b, vm)?).into())
      }

      let closure = (vm.heap).allocate(NativeClosure::new(stringify!($name), func, arg));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
    })
  };
}
pub(crate) use native_function;

macro_rules! count {
  () => { 0 };
  ( $x:tt $($xs:tt)* ) => { 1 + count!($($xs)*) };
}
pub(crate) use count;
