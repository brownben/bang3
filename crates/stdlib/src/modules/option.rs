use crate::StdlibModule;
use crate::macros::module;
use bang_interpreter::{
  ErrorKind, VM, Value,
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
  object::{NONE_TYPE_ID, SOME_TYPE_ID},
};

module!(option, OptionModule, {
  const /// No value
        None: option = Value::NONE;

  /// Some value
  #[type(^a => option<^a>)]
  fn Some() = |vm, arg| Ok(Value::from_object(vm.heap.allocate(arg), SOME_TYPE_ID));

  /// Is the value `None`?
  ///
  /// ## Example
  /// ```bang
  /// option::isNone(None) // true
  /// option::isNone(Some(1)) // false
  /// ```
  #[type(option => boolean)]
  fn isNone() = |_, arg| Ok(arg.is_object_type(NONE_TYPE_ID).into());
  /// Is the value `Some`?
  ///
  /// ## Example
  /// ```bang
  /// option::isSome(Some(1)) // true
  /// option::isSome(None) // false
  /// ```
  #[type(option => boolean)]
  fn isSome() = |_, arg| Ok(arg.is_object_type(SOME_TYPE_ID).into());

  /// Returns the contained `Some` value, panics if the value is a `None`.
  ///
  /// ## Example
  /// ```bang
  /// Some(5) >> option::unwrap // 5
  /// None >> option::unwrap // panic, execution stopped
  /// ```
  #[type(option<^a> => ^a)]
  fn unwrap() = |vm, arg| {
    if arg.is_object_type(SOME_TYPE_ID) {
      Ok(vm.heap[arg.as_object::<Value>()])
    } else if arg == Value::NONE {
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
  /// Some(5) >> option::unwrapOr(0) // 5
  /// None >> option::unwrapOr(0) // 0
  /// ```
  #[type(^a => option<^a> => ^a)]
  fn unwrapOr() = |vm, arg| {
    fn func(vm: &mut VM, alternative: Value, option: Value) -> Result<Value, ErrorKind> {
      if option == Value::NONE {
        Ok(alternative)
      } else if option.is_object_type(SOME_TYPE_ID) {
        Ok(vm.heap[option.as_object::<Value>()])
      } else {
        Err(ErrorKind::TypeError { expected: "option", got: option.get_type(vm) })
      }
    }

    let closure = (vm.heap).allocate(NativeClosure::new("option::unwrapOr", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Merges a nested Option into a single layer.
  ///
  /// Converts from `option<option<T>>` to `option<T>`
  ///
  /// ## Example
  /// ```bang
  /// option::flatten(Some(None)) // None
  /// option::flatten(Some(Some(5))) // Some(5)
  /// ```
  #[type(option<option<^a>> => option<^a>)]
  fn flatten() = |vm, arg| {
    if arg == Value::NONE {
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
  /// Some(5) >> option::map(x => x + 1) // Some(6)
  /// None >> option::map(x => x + 1) // None
  ///
  /// Some('example') >> option::map(string::length) // Some(7)
  /// None >> option::map(string::length) // None
  /// ```
  #[type((^a => ^b) => option<^a> => option<^b>)]
  fn map() = |vm, arg| {
    fn func(vm: &mut VM, function: Value, option: Value) -> Result<Value, ErrorKind> {
      if option == Value::NONE {
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

    let closure = (vm.heap).allocate(NativeClosure::new("option::map", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
});
