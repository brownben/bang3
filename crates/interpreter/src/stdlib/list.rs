use super::macros::{module, prelude::*};
use crate::object::{LIST_TYPE_ID, List};

module!(list, LIST_ITEMS, list_docs, {
  /// The number of elements in a list
  ///
  /// ## Example
  /// ```bang
  /// [2, 3, 5, 7, 11] >> list::length // 5
  /// [2] >> list::length // 1
  /// ```
  fn length(Value) -> Value = |vm, arg| {
    let result = get_list(arg, vm)?.len();
    #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
    Ok((result as f64).into())
  };

  /// Is the list empty?
  ///
  /// ## Example
  /// ```bang
  /// [] >> list::isEmpty // true
  /// [3, 4] >> list::isEmpty // false
  /// ```
  fn isEmpty(Value) -> Value = |vm, arg| {
    let result = get_list(arg, vm)?.is_empty();
    Ok(result.into())
  };

  /// Is the given item in the list?
  ///
  /// ## Example
  /// ```bang
  /// [1, 3, 5] >> list::contains(3) // true
  /// ['a', 'big', 'dog'] >> list::contains('hello') // false
  /// ```
  fn contains(Value) -> Value = |vm, arg| {
    fn func(vm: &mut VM, search: Value, list: Value) -> Result<Value, ErrorKind> {
      let list = get_list(list, vm)?;
      let result = list.iter().any(|item| vm.equals(*item, search));

      Ok(result.into())
    }

    let closure = (vm.heap).allocate(NativeClosure::new(stringify!($name), func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
});

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
