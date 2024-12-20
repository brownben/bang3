use crate::StdlibModule;
use crate::macros::module;
use bang_interpreter::{
  ErrorKind, VM, Value,
  object::SOME_TYPE_ID,
  object::{ITERATOR_TYPE_ID, Iterator, IteratorLength},
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
};

module!(list, ListModule, {
  /// The number of elements in a list
  ///
  /// ## Example
  /// ```bang
  /// [2, 3, 5, 7, 11] >> list::length // 5
  /// [2] >> list::length // 1
  /// ```
  #[type(list => number)]
  fn length() = |vm, arg| {
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
  #[type(list => boolean)]
  fn isEmpty() = |vm, arg| {
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
  #[type(^a => list<^a> => boolean)]
  fn contains() = |vm, arg| {
    fn func(vm: &mut VM, search: Value, list: Value) -> Result<Value, ErrorKind> {
      let list = get_list(list, vm)?;
      let result = list.iter().any(|item| vm.equals(*item, search));

      Ok(result.into())
    }

    let closure = (vm.heap).allocate(NativeClosure::new("contains", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Creates an iterator over the values of the list
  ///
  /// ## Example
  /// ```bang
  /// [] >> list::iter // <iterator>
  /// [1, 2, 3] >> list::iter // <iterator>
  /// ```
  #[type(list<^a> => iterator<^a>)]
  fn iter() = |vm, base| {
    fn next(vm: &mut VM, state: usize, base: Value) -> Option<(Value, usize)> {
      let list = base.as_list(&vm.heap);

      if state < list.len() {
        Some((list[state], state + 1))
      } else {
        None
      }
    }

    if !base.is_list() {
      return Err(ErrorKind::TypeError { expected: "list", got: base.get_type(vm) });
    }
    let length = base.as_list(&vm.heap).len();

    let iterator = vm.heap.allocate(Iterator {
      base,
      next,
      length: IteratorLength::Max(length)
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };

  /// Gets an item from the list at the given index
  ///
  /// The index is truncated to an integer, if it is negative, the index is
  /// calculated from the end of the list.
  ///
  /// ## Example
  /// ```bang
  /// [1, 2, 3] >> list::get(1) // Some(2)
  /// [1, 2, 3] >> list::get(5) // None
  /// [1, 2, 3] >> list::get(-1) // Some(3)
  /// [1, 2, 3] >> list::get(-5) // None
  /// ```
  #[type(number => list<^a> => option<^a>)]
  fn get() = |vm, index| {
    fn negative_func(vm: &mut VM, number: Value, list: Value) -> Result<Value, ErrorKind> {
      let list = get_list(list, vm)?;

      #[expect(clippy::cast_sign_loss, reason = "handled below")]
      #[expect(clippy::cast_possible_truncation, reason = "documented behaviour")]
      let index = -number.as_number() as usize;

      if index > list.len() {
        return Ok(Value::NONE);
      }

      let result = unsafe { *list.get_unchecked(list.len() - index) };
      Ok(Value::from_object(vm.heap.allocate(result), SOME_TYPE_ID))
    }
    fn positive_func(vm: &mut VM, number: Value, list: Value) -> Result<Value, ErrorKind> {
      let list = get_list(list, vm)?;

      #[expect(clippy::cast_sign_loss, reason = "handled")]
      #[expect(clippy::cast_possible_truncation, reason = "documented behaviour")]
      let index = number.as_number() as usize;

      if index >= list.len() {
        return Ok(Value::NONE);
      }

      let result = unsafe { *list.get_unchecked(index) };
      Ok(Value::from_object(vm.heap.allocate(result), SOME_TYPE_ID))
    }

    if !index.is_number() {
      return Err(ErrorKind::TypeError { expected: "number", got: index.get_type(vm) });
    }

    let closure = if index.as_number() < 0.0 {
      (vm.heap).allocate(NativeClosure::new("list::get", negative_func, index))
    } else {
      (vm.heap).allocate(NativeClosure::new("list::get", positive_func, index))
    };
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
});

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
fn get_list<'a>(value: Value, vm: &'a VM) -> Result<&'a [Value], ErrorKind> {
  if value.is_list() {
    Ok(value.as_list(&vm.heap))
  } else {
    Err(ErrorKind::TypeError {
      expected: "list",
      got: value.get_type(vm),
    })
  }
}
