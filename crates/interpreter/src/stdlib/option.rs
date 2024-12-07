use super::macros::module;
use crate::{
  VM, Value,
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
  object::{NONE_TYPE_ID, SOME_TYPE_ID},
  vm::ErrorKind,
};
use bang_gc::Gc;

module!(option, OPTION_ITEMS, option_types, option_docs, {
  const /// No value
        None: option = Value::from_object(Gc::NULL, NONE_TYPE_ID);

  /// Some value
  #[type(^a => option<^a>)]
  fn Some() = |vm, arg| Ok(Value::from_object(vm.heap.allocate(arg), SOME_TYPE_ID));

  /// Is the value `None`?
  ///
  /// ## Example
  /// ```bang
  /// option::isNone(option::None) // true
  /// option::isNone(option::Some(1)) // false
  /// ```
  #[type(option => boolean)]
  fn isNone() = |_, arg| Ok(arg.is_object_type(NONE_TYPE_ID).into());
  /// Is the value `Some`?
  ///
  /// ## Example
  /// ```bang
  /// option::isSome(option::Some(1)) // true
  /// option::isSome(option::None) // false
  /// ```
  #[type(option => boolean)]
  fn isSome() = |_, arg| Ok(arg.is_object_type(SOME_TYPE_ID).into());

  /// Returns the contained `Some` value, panics if the value is a `None`.
  ///
  /// ## Example
  /// ```bang
  /// option::Some(5) >> option::unwrap // 5
  /// option::None >> option::unwrap // panic, execution stopped
  /// ```
  #[type(option<^a> => ^a)]
  fn unwrap() = |vm, arg| {
    if arg.is_object_type(SOME_TYPE_ID) {
      Ok(vm.heap[arg.as_object::<Value>()])
    } else if arg.is_object_type(NONE_TYPE_ID) {
      Err(ErrorKind::Custom {
        title: "Panic",
        message: "called `option::unwrap` on a `None` value".to_string()
      })
    } else {
      Err(ErrorKind::TypeError { expected: "option", got: arg.get_type(vm) })
    }
  };
  /// Returns the contained Some value or a provided default.
  ///
  /// ## Example
  /// ```bang
  /// option::Some(5) >> option::unwrapOr(0) // 5
  /// option::None >> option::unwrapOr(0) // 0
  /// ```
  #[type(^a => option<^a> => ^a)]
  fn unwrapOr() = |vm, arg| {
    fn func(vm: &mut VM, alternative: Value, option: Value) -> Result<Value, ErrorKind> {
      if option.is_object_type(NONE_TYPE_ID) {
        Ok(alternative)
      } else if option.is_object_type(SOME_TYPE_ID) {
        Ok(vm.heap[option.as_object::<Value>()])
      } else {
        Err(ErrorKind::TypeError { expected: "option", got: option.get_type(vm) })
      }
    }

    let closure = (vm.heap).allocate(NativeClosure::new("unwrapOr", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Merges a nested Option into a single layer.
  ///
  /// Converts from `option<option<T>>` to `option<T>`
  ///
  /// ## Example
  /// ```bang
  /// option::flatten(option::Some(option::None)) // None
  /// option::flatten(option::Some(option::Some(5))) // Some(5)
  /// ```
  #[type(option<option<^a>> => option<^a>)]
  fn flatten() = |vm, arg| {
    if arg.is_object_type(NONE_TYPE_ID) {
      Ok(arg)
    } else if arg.is_object_type(SOME_TYPE_ID) {
      Ok(vm.heap[arg.as_object::<Value>()])
    } else {
      Err(ErrorKind::TypeError { expected: "option", got: arg.get_type(vm) })
    }
  };

  /// Maps an `Option<T>` to a `Option<U>` by applying a function to a contained value
  /// (if `Some`) or returns `None` (if `None`).
  ///
  /// ## Example
  /// ```bang
  /// option::Some(5) >> option::map(x => x + 1) // Some(6)
  /// option::None >> option::map(x => x + 1) // None
  ///
  /// option::Some('example') >> option::map(string::length) // Some(7)
  /// option::None >> option::map(string::length) // None
  /// ```
  #[type((^a => ^b) => option<^a> => option<^b>)]
  fn map() = |vm, arg| {
    fn func(vm: &mut VM, function: Value, option: Value) -> Result<Value, ErrorKind> {
      if option.is_object_type(NONE_TYPE_ID) {
        Ok(option)
      } else if option.is_object_type(SOME_TYPE_ID) {
        let inner_value = vm.heap[option.as_object::<Value>()];
        let result = vm.call(function, inner_value)?.unwrap_or(Value::NULL);

        Ok(Value::from_object(vm.heap.allocate(result), SOME_TYPE_ID))
      } else {
        Err(ErrorKind::TypeError { expected: "option", got: option.get_type(vm) })
      }
    }


    if !arg.is_callable(vm) {
      return Err(ErrorKind::TypeError { expected: "function", got: arg.get_type(vm) })
    }

    let closure = (vm.heap).allocate(NativeClosure::new("map", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
});