use crate::StdlibModule;
use crate::macros::module;
use bang_interpreter::{
  ErrorKind, VM, Value,
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
};

module!(assert, AssertModule, {
  /// Asserts that two values are equal
  ///
  /// If the values are not equal, it panics stopping all execution.
  /// If they are equal true is returned.
  ///
  /// ## Example
  /// ```bang
  /// assert::equal('hello')('hello')
  /// assert::equal('hello')('world') // Panics
  /// ```
  #[type(^a => ^a => boolean)]
  fn equal() = |vm, value|{
    fn func(vm: &mut VM, first: Value, second: Value) -> Result<Value, ErrorKind> {
      if vm.equals(first, second) {
        Ok(Value::TRUE)
      } else {
        Err(ErrorKind::Custom {
          title: "Assertion Error",
          message: format!(
            "expected both values to be equal\nfirst: {} [type: {}]\nsecond: {} [type: {}]",
            first.debug(vm), first.get_type(vm),
            second.debug(vm), second.get_type(vm),
          ),
        })
      }
    }

    let closure = vm.heap.allocate(NativeClosure::new("assert::equal", func, value));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Asserts that a value is truthy
  ///
  /// If the value is not truthy, it panics stopping all execution.
  /// If it is truthy the value is returned.
  ///
  /// ## Example
  /// ```bang
  /// assert::true(3.5 > 2)
  /// assert::true(1.5 < 1) // Panics
  /// ```
  #[type(^a => boolean)]
  fn true() = |vm, value|{
    if value.is_falsy(vm) {
      Err(ErrorKind::Custom {
        title: "Assertion Error",
        message: format!(
          "expected a truthy value, but got {} [type: {}]",
          value.debug(vm), value.get_type(vm),
        ),
      })
    } else {
      Ok(value)
    }
  };

  /// Asserts that a value is falsy
  ///
  /// If the value is not falsy, it panics stopping all execution.
  /// If the value is falsy the value is returned.
  ///
  /// ## Example
  /// ```bang
  /// assert::false(3.5 < 2)
  /// assert::false(1.5 > 1) // Panics
  /// ```
  #[type(^a => boolean)]
  fn false() = |vm, value|{
    if value.is_truthy(vm) {
      Err(ErrorKind::Custom {
        title: "Assertion Error",
        message: format!(
          "expected a falsy value, but got {} [type: {}]",
          value.debug(vm), value.get_type(vm),
        ),
      })
    } else {
      Ok(value)
    }
  };
});
