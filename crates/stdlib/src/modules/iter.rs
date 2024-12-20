use crate::StdlibModule;
use crate::macros::module;
use bang_interpreter::{
  ErrorKind, VM, Value,
  object::SOME_TYPE_ID,
  object::{BangString, STRING_TYPE_ID},
  object::{ITERATOR_TRANSFORM_TYPE_ID, IteratorTransform},
  object::{ITERATOR_TYPE_ID, Iterator, IteratorLength},
  object::{LIST_TYPE_ID, List},
  object::{NATIVE_CLOSURE_TWO_TYPE_ID, NativeClosureTwo},
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
};

module!(iter, IterModule, {
  /// Creates an iterator that yields nothing
  ///
  /// ## Example
  /// ```bang
  /// iter::empty() >> iter::first // None
  /// iter::empty() >> iter::toList // []
  /// ```
  #[type(_ => iterator)]
  fn empty() = |vm, base| {
    fn next(vm: &mut VM, _: usize, _: Value) -> Option<(Value, usize)> {
      None
    }

    let iterator = vm.heap.allocate(Iterator {
      base,
      next,
      length: IteratorLength::Max(0),
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };
  /// Creates an iterator that yields an element exactly once
  ///
  /// ## Example
  /// ```bang
  /// iter::once(1) >> iter::first // Some(1)
  /// iter::once('hello') >> iter::toList // ['hello']
  /// ```
  #[type(^a => iterator<^a>)]
  fn once() = |vm, base| {
    fn next(vm: &mut VM, state: usize, value: Value) -> Option<(Value, usize)> {
      (state == 0).then_some((value, 1))
    }

    let iterator = vm.heap.allocate(Iterator {
      base,
      next,
      length: IteratorLength::Max(1),
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };
  /// Creates a new iterator that endlessly repeats a single element
  ///
  /// ## Example
  /// ```bang
  /// iter::repeat(1) >> iter::first // Some(1)
  ///
  /// iter::repeat('hello') >> iter::toList // Panics: cannot make an infinite list
  /// ```
  #[type(^a => iterator<^a>)]
  fn repeat() = |vm, base| {
    #[expect(clippy::unnecessary_wraps)]
    fn next(vm: &mut VM, state: usize, value: Value) -> Option<(Value, usize)> {
      Some((value, state))
    }

    let iterator = (vm.heap).allocate(Iterator {
      base,
      next,
      length: IteratorLength::Infinite,
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };
  /// Creates a new iterator which counts integers starting from 0
  ///
  /// ## Example
  /// ```bang
  /// iter::integers() >> iter::first // Some(0)
  /// iter::integers() >> iter::takeWhile(x => x < 3) >> iter::toList // [0, 1, 2]
  /// ```
  #[type(_ => iterator<number>)]
  fn integers() = |vm, base| {
    #[expect(clippy::unnecessary_wraps)]
    fn next(vm: &mut VM, state: usize, value: Value) -> Option<(Value, usize)> {
      #[expect(clippy::cast_precision_loss, reason = "state < 2^52")]
      Some(((state as f64).into(), state + 1))
    }

    let iterator = (vm.heap).allocate(Iterator {
      base,
      next,
      length: IteratorLength::Infinite,
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };

  /// Gets the first element from the iterator
  ///
  /// ## Example
  /// ```bang
  /// iter::repeat(false) >> iter::first // Some(false)
  /// iter::once(3) >> iter::last // Some(3)
  /// iter::empty() >> iter::last // None
  /// ```
  #[type(iterator<^a> => option<^a>)]
  fn first() = |vm, iterator| {
    let result = iter_next(vm, iterator, 0)?;

    match result {
      Some(value) => Ok(Value::from_object(vm.heap.allocate(value), SOME_TYPE_ID)),
      None => Ok(Value::NONE)
    }
  };
  /// Gets the last element from the iterator
  ///
  /// Iterates through until the iterator finishes, and then returns the last value seen.
  /// If the iterator is infinite, it will panic.
  ///
  /// ## Example
  /// ```bang
  /// iter::once(3) >> iter::last // Some(3)
  /// iter::empty() >> iter::last // None
  /// ```
  #[type(iterator<^a> => option<^a>)]
  fn last() = |vm, iterator| {
    if is_infinite(iterator, vm) {
      return Err(ErrorKind::Custom {
        title: "Infinite Computation",
        message: "cannot get the last element of an infinite list".to_owned()
      });
    };

    let mut last = None;
    let mut state = 0;
    while let Some((value, new_state)) = iter_next(vm, iterator, state)? {
      last = Some(value);
      state = new_state;
    }

    match last {
      Some(value) => Ok(Value::from_object(vm.heap.allocate(value), SOME_TYPE_ID)),
      None => Ok(Value::NONE)
    }
  };

  /// Counts the number of elements in the iterator
  ///
  /// Iterates through until the entire iterator. If the iterator is infinite,
  /// it returns the `∞` (`maths::INFINITY`).
  ///
  /// ## Example
  /// ```bang
  /// iter::once(3) >> iter::count // 1
  /// iter::empty() >> iter::count // 0
  /// iter::repeat(false) >> iter::count // ∞
  /// ```
  #[type(iterator<^a> => number)]
  fn count() = |vm, iterator| {
    if is_infinite(iterator, vm) {
      return Ok(f64::INFINITY.into());
    }

    let mut result = 0;
    let mut state = 0;
    while let Some((_, new_state)) = iter_next(vm, iterator, state)? {
      result += 1;
      state = new_state;
    };

    Ok(f64::from(result).into())
  };

  /// Are all of the values of the iterator `true`?
  ///
  /// It will stop processing as soon as it finds a `false`, given
  /// that no matter what else happens, the result will also be `false`.
  ///
  /// If the iterator is infinite and truthy, it will iterate forever.
  /// If it is empty, it returns `true`.
  ///
  /// ## Example
  /// ```bang
  /// iter::empty() >> iter::all // true
  /// [true, true] >> list::iter >> iter::all // true
  /// [true, false, true] >> list::iter >> iter::any // false
  /// ```
  #[type(iterator<boolean> => boolean)]
  fn all() = |vm, iterator| {
    let mut state = 0;
    let result = loop {
      match iter_next(vm, iterator, state)? {
        Some((value, new_state)) if value.is_truthy(vm) => state = new_state,
        Some((_, _)) => break false,
        None => break true,
      }
    };

    Ok(result.into())
  };
  /// Are any of the values of the iterator `true`?
  ///
  /// It will stop processing as soon as it finds a `true`, given
  /// that no matter what else happens, the result will also be `true`.
  ///
  /// If the iterator is infinite and the items are falsy, it will iterate forever.
  /// If it is empty, it returns `false`.
  ///
  /// ## Example
  /// ```bang
  /// iter::empty() >> iter::any // false
  /// iter::repeat(true) >> iter::any // true
  /// [false, true, false] >> list::iter >> iter::any // false
  /// ```
  #[type(iterator<boolean> => boolean)]
  fn any() = |vm, iterator| {
    let mut state = 0;
    let result = loop {
      match iter_next(vm, iterator, state)? {
        Some((value, _)) if value.is_truthy(vm) => break true,
        Some((_, new_state)) => state = new_state,
        None => break false,
      }
    };

    Ok(result.into())
  };
  /// Searches for an element of an iterator that satisfies a predicate
  ///
  /// Returns `Some(value)` if an element satisfies the predicate, or `None`
  /// if no element satisfies the predicate.
  ///
  /// ## Example
  /// ```bang
  /// 'hello world' >> string::chars >> iter::find(char => char > 't') // Some('w')
  /// 'hello world' >> string::chars >> iter::find(char => char > 'z') // None
  /// ```
  #[type((^a -> boolean) -> iterator<^a> => option<^a>)]
  fn find() =  |vm, arg| {
    fn func(vm: &mut VM, func: Value, iterator: Value) -> Result<Value, ErrorKind> {
      let mut state = 0;
      let result = loop {
        match iter_next(vm, iterator, state)? {
          Some((result, new_state)) => match vm.call(func, result)? {
            Some(value) if value.is_truthy(vm) => break Some(result),
            Some(_) | None => state = new_state,
          }
          None => break None,
        }
      };

      match result {
        Some(value) => Ok(Value::from_object(vm.heap.allocate(value), SOME_TYPE_ID)),
        None => Ok(Value::NONE)
      }
    }

    if !arg.is_callable(vm) {
      return Err(ErrorKind::TypeError {
        expected: "function",
        got: arg.get_type(vm),
      });
    }

    let closure = vm.heap.allocate(NativeClosure::new("iter::find", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
  /// Searches for an element of an iterator, and returns its index
  ///
  /// Returns `Some(index)` if an element satisfies the predicate, or `None`
  /// if no element satisfies the predicate.
  ///
  /// ## Example
  /// ```bang
  /// 'hello world' >> string::chars >> iter::find(char => char > 't') // Some(6)
  /// 'hello world' >> string::chars >> iter::find(char => char > 'z') // None
  /// ```
  #[type((^a -> boolean) -> iterator<^a> => option<number>)]
  fn position() = |vm, arg| {
    fn func(vm: &mut VM, func: Value, iterator: Value) -> Result<Value, ErrorKind> {
      let mut i = 0.0;
      let mut state = 0;
      let result: Option<Value> = loop {
        match iter_next(vm, iterator, state)? {
          Some((result, new_state)) => match vm.call(func, result)? {
            Some(value) if value.is_truthy(vm) => break Some(i.into()),
            Some(_) | None => {
              state = new_state;
              i += 1.0;
            },
          }
          None => break None,
        }
      };

      match result {
        Some(value) => Ok(Value::from_object(vm.heap.allocate(value), SOME_TYPE_ID)),
        None => Ok(Value::NONE)
      }
    }

    if !arg.is_callable(vm) {
      return Err(ErrorKind::TypeError {
        expected: "function",
        got: arg.get_type(vm),
      });
    }

    let closure = vm.heap.allocate(NativeClosure::new("iter::position", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Collects the items generated by the iterator into a list
  ///
  /// If the iterator is infinite, it panics.
  ///
  /// ## Example
  /// ```bang
  /// iter::empty() >> iter::toList // []
  /// iter::once(5) >> iter::toList // [5]
  /// [1, 2, 3] >> list::iter >> iter::toList // [1, 2, 3]
  /// ```
  #[type(iterator<^a> => list<^a>)]
  fn toList() = |vm, iterator| {
    let length = match iterator_length(iterator, vm) {
      IteratorLength::Infinite => {
        return Err(ErrorKind::Custom {
          title: "Infinite Computation",
          message: "cannot collect an infinite iterator into a list".to_owned()
        });
      },
      IteratorLength::Unknown => 8,
      IteratorLength::Max(length) => length,
    };

    // We save the list on the stack, so it can always be viewed by the GC tracer
    // We take a pointer to it, so we can update it if it needs to be resized
    let list = List::with_capacity(&mut vm.heap, length);
    let mut list_location = vm.stash_value(Value::from_object(list.as_ptr(), LIST_TYPE_ID))?;

    let mut state = 0;
    while let Some((value, new_state)) = iter_next(vm, iterator, state)? {
      let list = List::from(vm.pop_stashed_value(list_location).as_object());
      let updated_list = list.push(&mut vm.heap, value);
      list_location = vm.stash_value(Value::from_object(updated_list.as_ptr(), LIST_TYPE_ID))?;

      state = new_state;
    };

    Ok(vm.pop_stashed_value(list_location))
  };

  /// Transforms the iterator by applying a function to each element
  ///
  /// ## Example
  /// ```bang
  /// iter::integers()
  ///   >> iter::map(x => x * 2)
  ///   >> iter::takeWhile(x => x < 10)
  ///   >> iter::toList // [0, 2, 4, 6, 8]
  ///
  /// 'hello'
  ///   >> string::chars
  ///   >> iter::map(string::toUppercase)
  ///   >> iter::toList // ['H', 'E', 'L', 'L', 'O']
  /// ```
  #[type((^a => ^b) => iterator<^a> => iterator<^b>)]
  fn map() = function_transform!("map", |vm, iterator, state, func| {
    Ok(match iter_next(vm, iterator, state)? {
      Some((value, new_state)) => vm.call(func, value)?.map(|value| (value, new_state)),
      None => None
    })
  });
  /// Does something with each element of an iterator, passing the value on
  ///
  /// Can be useful for debugging, and seeing what is going on at different stages
  ///
  /// ## Example
  /// ```bang
  /// list::iter([1, 2, 3]) >> iter::inspect(x => print(`Value: {x}`)) >> iter::toList
  /// ```
  #[type((^a => ^b) => iterator<^a> => iterator<^a>)]
  fn inspect() = function_transform!("inspect", |vm, iterator, state, func| {
    match iter_next(vm, iterator, state)? {
      Some((value, new_state)) => {
        let _result = vm.call(func, value)?;

        Ok(Some((value, new_state)))
      }
      None => Ok(None)
    }
  });
  /// Filters the iterator by keeping only the elements that pass the predicate
  ///
  /// If the predicate returns true, the element is kept, otherwise it is discarded
  ///
  /// ## Example
  /// ```bang
  /// iter::integers()
  ///   >> iter::takeWhile(x => x < 15)
  ///   >> iter::filter(x => x >= 10)
  ///   >> iter::toList // [10, 11, 12, 13, 14]
  ///
  /// 'Hi 👋'
  ///   >> string::chars
  ///   >> iter::filter(string::isAscii)
  ///   >> iter::toList // ['H', 'i', ' ']
  /// ```
  #[type((^a => boolean) => iterator<^a> => iterator<^a>)]
  fn filter() = function_transform!("filter", |vm, iterator, state, func| {
    let mut state = state;
    loop {
      match iter_next(vm, iterator, state)? {
        Some((result, new_state)) => match vm.call(func, result)? {
          Some(value) if value.is_truthy(vm) => return Ok(Some((result, new_state))),
          Some(_) | None => state = new_state,
        }
        None => return Ok(None),
      }
    }
  });

  /// Creates an iterator that both filters and maps
  ///
  /// The returned iterator only has values for which the supplied functions returns `Some(value)`
  ///
  /// `filterMap` can be used to make chains of filter and map more concise.
  ///
  /// ## Example
  /// ```bang
  ///
  /// let input = '12
  /// 13
  /// 14'
  ///
  /// input
  ///   >> string::lines
  ///   >> iter::map(string::parseNumber)
  ///   >> iter::filter(option::isSome)
  ///   >> iter::map(option::unwrap)
  ///
  /// // the above chain can be simplified with `filterMap` to:
  /// input >> string::lines >> iter::filterMap(string::parseNumber)
  /// ```
  #[type((^a => option<^b>) => iterator<^a> => iterator<^b>)]
  fn filterMap() = function_transform!("filterMap", |vm, iterator, state, func| {
    let mut state = state;
    loop {
      match iter_next(vm, iterator, state)? {
        Some((result, new_state)) => {
          let value = vm.call(func, result)?.unwrap();

          if value.is_object_type(SOME_TYPE_ID) {
            return Ok(Some((vm.heap[value.as_object::<Value>()], new_state)));
          } else if value == Value::NONE {
            state = new_state;
          } else {
            return Err(ErrorKind::TypeError { expected: "option", got: value.get_type(vm) })
          }
        }
        None => return Ok(None),
      }
    }
  });

  /// Reduces the elements to a single one, by repeatedly applying a reducing operation
  ///
  /// If the iterator is empty, returns None; otherwise, returns the result of the reduction.
  ///
  /// The reducing function is called twice, first with the current accumulator, and then with
  /// the current item from the iterator.
  ///
  /// ## Example
  /// ```bang
  /// let sum = iter::reduce(acc => x => acc + x)
  /// sum(list::iter([1, 2, 3])) // 6
  ///
  /// let product = iter::reduce(acc => x => acc * x)
  /// product(list::iter([2, 4, 8])) // 64
  /// ```
  #[type((^a => ^a => ^a) => iterator<^a> => option<^a>)]
  fn reduce() = |vm, arg| {
    fn func(vm: &mut VM, func: Value, iterator: Value) -> Result<Value, ErrorKind> {
      if is_infinite(iterator, vm) {
        return Err(ErrorKind::Custom {
          title: "Infinite Computation",
          message: "cannot reduce an infinite iterator into a value".to_owned()
        });
      };

      let Some((mut accumulator, mut state)) = iter_next(vm, iterator, 0)? else {
        return Ok(Value::NONE);
      };

      loop {
        let Some((iterator_result, new_state)) = iter_next(vm, iterator, state)? else { break };
        let iterator_result = vm.stash_value(iterator_result)?;
        let Some(inner_function) = vm.call(func, accumulator)? else { break };
        let iterator_result = vm.pop_stashed_value(iterator_result);
        let Some(new_accumulator) = vm.call(inner_function, iterator_result)? else { break };

        accumulator = new_accumulator;
        state = new_state;
      };

      Ok(Value::from_object(vm.heap.allocate(accumulator), SOME_TYPE_ID))
    }

    if !arg.is_callable(vm) {
      return Err(ErrorKind::TypeError {
        expected: "function",
        got: arg.get_type(vm),
      });
    }

    let closure = vm.heap.allocate(NativeClosure::new("iter::reduce", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
  /// Folds every element into an accumulator by applying an operation, returning the final result
  ///
  /// Takes two arguments, the initial value for the accumulator, and the function to apply.
  /// The function returns the new accumulator value, and is called twice, first with the current
  /// accumulator, and then with the current item from the iterator.
  ///
  /// ## Example
  /// ```bang
  /// list::iter([1, 2, 3]) >> iter::fold(0)(acc => x => acc + x) // 6
  /// list::iter([1, 2, 3]) >> iter::fold(4)(acc => x => acc + x) // 10
  /// ```
  #[type(^a => (^a => ^b => ^a) => iterator<^b> => ^a)]
  fn fold() = |vm, arg| {
    fn func_one(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
      if !b.is_callable(vm) {
        return Err(ErrorKind::TypeError { expected: "function", got: b.get_type(vm) });
      }

      let closure = vm.heap.allocate(NativeClosureTwo::new("iter::fold", func_two, a, b));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TWO_TYPE_ID))
    }
    fn func_two(vm: &mut VM, start: Value, func: Value, iter: Value) -> Result<Value, ErrorKind> {
      if is_infinite(iter, vm) {
        return Err(ErrorKind::Custom {
          title: "Infinite Computation",
          message: "cannot fold an infinite iterator into a value".to_owned()
        });
      };

      let mut state = 0;
      let mut accumulator = start;

      loop {
        let Some((iterator_result, new_state)) = iter_next(vm, iter, state)? else { break };
        let iterator_result = vm.stash_value(iterator_result)?;
        let Some(inner_function) = vm.call(func, accumulator)? else { break };
        let iterator_result = vm.pop_stashed_value(iterator_result);
        let Some(new_accumulator) = vm.call(inner_function, iterator_result)? else { break };

        accumulator = new_accumulator;
        state = new_state;
      };

      Ok(accumulator)
    }

    let closure = vm.heap.allocate(NativeClosure::new("iter::fold", func_one, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Sums the items in an iterator of numbers
  ///
  /// Takes each element, adds them together, and returns the result
  ///
  /// An empty iterator returns zero
  ///
  /// ## Example
  /// ```bang
  /// iter::integers() >> iter::takeWhile(x => x < 5) >> iter::sum // 10
  /// iter::empty() >> iter::sum // 0
  /// ```
  #[type(iterator<number> => number)]
  fn sum() = |vm, iter| {
    if is_infinite(iter, vm) {
      return Err(ErrorKind::Custom {
        title: "Infinite Computation",
        message: "cannot sum an infinite iterator into a value".to_owned()
      });
    };

    let mut state = 0;
    let mut sum = 0.0;

    loop {
      let Some((result, new_state)) = iter_next(vm, iter, state)? else { break };

      if !result.is_number() {
        return Err(ErrorKind::TypeError { expected: "number", got: result.get_type(vm) });
      }

      sum += result.as_number();
      state = new_state;
    };

    Ok(Value::from(sum))
  };
  /// Multiplies the items in an iterator of numbers
  ///
  /// An empty iterator returns one
  ///
  /// ## Example
  /// ```bang
  /// list::iter([5, 4, 3]) >> iter::product // 60
  /// iter::empty() >> iter::product // 1
  /// ```
  #[type(iterator<number> => number)]
  fn product() = |vm, iter| {
    if is_infinite(iter, vm) {
      return Err(ErrorKind::Custom {
        title: "Infinite Computation",
        message: "cannot sum an infinite iterator into a value".to_owned()
      });
    };

    let mut state = 0;
    let mut product = 1.0;

    loop {
      let Some((result, new_state)) = iter_next(vm, iter, state)? else { break };

      if !result.is_number() {
        return Err(ErrorKind::TypeError { expected: "number", got: result.get_type(vm) });
      }

      product *= result.as_number();
      state = new_state;
    };

    Ok(Value::from(product))
  };
  /// Joins an iterator of strings, placing the separator between each item
  ///
  /// ## Example
  /// ```bang
  /// list::iter(['a', 'b', 'c']) >> iter::join('') // 'abc'
  /// list::iter(['a', 'b', 'c']) >> iter::join('-') // 'a-b-c'
  /// ```
  #[type(string => iterator<string> => string)]
  fn join() = |vm, arg| {
    fn func(vm: &mut VM, arg: Value, iterator: Value) -> Result<Value, ErrorKind> {
      let length = match iterator_length(iterator, vm) {
        IteratorLength::Infinite => {
          return Err(ErrorKind::Custom {
            title: "Infinite Computation",
            message: "cannot join an infinite string".to_owned()
          });
        },
        IteratorLength::Unknown => 8,
        IteratorLength::Max(length) => length,
      };

      let string = BangString::with_capacity(&mut vm.heap, length * 3);
      let mut string_ptr = vm.stash_value(Value::from_object(string.as_ptr(), STRING_TYPE_ID))?;

      let string_join = {
        // we can't borrow the string & result together - so we get the string without the lifetime
        let string = arg.as_string(&vm.heap);
        let slice = unsafe { std::slice::from_raw_parts(string.as_ptr(), string.len()) };
        unsafe { std::str::from_utf8_unchecked(slice) }
      };

      let mut state = 0;
      while let Some((value, new_state)) = iter_next(vm, iterator, state)? {
        if !value.is_string() {
          return Err(ErrorKind::TypeError { expected: "string", got: value.get_type(vm) });
        }
        let string_part = {
          // we can't borrow the string & result together - so we get the string without the lifetime
          let string = value.as_string(&vm.heap);
          let slice = unsafe { std::slice::from_raw_parts(string.as_ptr(), string.len()) };
          unsafe { std::str::from_utf8_unchecked(slice) }
        };

        let string = BangString::from(vm.pop_stashed_value(string_ptr).as_object::<usize>());
        let string = if state > 0 { string.push(&mut vm.heap, string_join) } else { string };
        let new_string = string.push(&mut vm.heap, string_part);
        string_ptr = vm.stash_value(Value::from_object(new_string.as_ptr(), STRING_TYPE_ID))?;

        state = new_state;
      };

      Ok(vm.pop_stashed_value(string_ptr))
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = vm.heap.allocate(NativeClosure::new("iter::join", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Takes elements of an iterator while a predicate holds
  ///
  /// If the predicate returns true, the element is taken, otherwise the iteration stops.
  ///
  /// Can be used to make an infinite iterator finite.
  ///
  /// ## Example
  /// ```bang
  /// iter::integers()
  ///   >> iter::takeWhile(x => x < 5)
  ///   >> iter::toList // [0, 1, 2, 3, 4]
  /// ```
  #[type((^a => boolean) => iterator<^a> => iterator<^a>)]
  fn takeWhile() = function_transform!("takeWhile" #unknownLength, |vm, iterator, state, func| {
    let Some((result, new_state)) = iter_next(vm, iterator, state)? else { return Ok(None) };

    let predicate = vm.call(func, result)?;
    if predicate.unwrap().is_truthy(vm) {
      return Ok(Some((result, new_state)));
    }

    Ok(None)
  });
});

fn iterator_length(value: Value, vm: &VM) -> IteratorLength {
  if value.is_object_type(ITERATOR_TYPE_ID) {
    let iterator: &Iterator = &vm.heap[value.as_object()];
    return iterator.length;
  }

  if value.is_object_type(ITERATOR_TRANSFORM_TYPE_ID) {
    let transform: &IteratorTransform = &vm.heap[value.as_object()];
    return transform.length;
  }

  IteratorLength::Unknown
}

fn is_infinite(value: Value, vm: &VM) -> bool {
  iterator_length(value, vm) == IteratorLength::Infinite
}

fn iter_next(
  vm: &mut VM,
  iterator: Value,
  state: usize,
) -> Result<Option<(Value, usize)>, ErrorKind> {
  if iterator.is_object_type(ITERATOR_TYPE_ID) {
    let iterator: &Iterator = &vm.heap[iterator.as_object()];

    return Ok((iterator.next)(vm, state, iterator.base));
  }

  if iterator.is_object_type(ITERATOR_TRANSFORM_TYPE_ID) {
    let transform: &IteratorTransform = &vm.heap[iterator.as_object()];

    return (transform.next)(vm, transform.iterator, state, transform.arg);
  }

  Err(ErrorKind::TypeError {
    expected: "iterator",
    got: iterator.get_type(vm),
  })
}

macro_rules! function_transform {
  ($name:literal $(#$infinite:ident)?, $next:expr) => {
    |vm, arg| {
      fn func(vm: &mut VM, arg: Value, iterator: Value) -> Result<Value, ErrorKind> {
        if !iterator.is_iterator() {
          return Err(ErrorKind::TypeError {
            expected: "iterator",
            got: arg.get_type(vm),
          });
        }

        let iterator = (vm.heap).allocate(IteratorTransform {
          iterator,
          arg,
          next: $next,
          length: function_transform!(infinite $($infinite)?, iterator, vm),
        });
        Ok(Value::from_object(iterator, ITERATOR_TRANSFORM_TYPE_ID))
      }

      if !arg.is_callable(vm) {
        return Err(ErrorKind::TypeError {
          expected: "function",
          got: arg.get_type(vm),
        });
      }


      let closure = vm.heap.allocate(NativeClosure::new(concat!("iter::", $name), func, arg));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
    }
  };

  (infinite infinite, $iterator:expr, $vm:expr) => {
    IteratorLength::Infinite
  };
  (infinite unknownLength, $iterator:expr, $vm:expr) => {{
    let length = iterator_length($iterator, $vm);
    if length == IteratorLength::Infinite {
      IteratorLength::Unknown
    } else {
      length
    }
  }};
  (infinite, $iterator:expr, $vm:expr) => {
    iterator_length($iterator, $vm)
  };
}
use function_transform;
