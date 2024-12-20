#![allow(clippy::cast_precision_loss, reason = "string lengths are < 2^52")]
use crate::StdlibModule;
use crate::macros::module;
use bang_interpreter::{
  ErrorKind, VM, Value,
  object::SOME_TYPE_ID,
  object::{ITERATOR_TYPE_ID, Iterator, IteratorLength},
  object::{NATIVE_CLOSURE_TWO_TYPE_ID, NativeClosureTwo},
  object::{NATIVE_CLOSURE_TYPE_ID, NativeClosure},
  object::{STRING_VIEW_TYPE_ID, StringView},
};

module!(string, StringModule, {
  const /// A newline character
        NEW_LINE: String = "\n";
  const /// A tab character
        TAB: String = "\t";
  const /// A carriage return character
        CARRIAGE_RETURN: String = "\r";

  /// Converts a value to a string
  ///
  /// ```bang
  /// from string import { toString }
  ///
  /// toString(false) // 'false'
  /// toString('hello') // 'hello'
  /// toString(x => x + 1) // '<function>'
  /// ```
  #[type(^a => string)]
  fn toString() = |vm, arg| {
    if arg.is_string() {
      return Ok(arg);
    }

    let result = (arg).display(vm);
    Ok(vm.allocate_string(&result))
  };
  /// Converts a value to a string
  ///
  /// Re-export of `string::toString` to make it more natural to use with
  /// module access expressions. (e.g. `string::from(5)`).
  ///
  /// ```bang
  /// string::from(false) // 'false'
  /// string::from('hello') // 'hello'
  /// string::from(x => x + 1) // '<function>'
  /// ```
  #[type(^a => string)]
  fn from() = |vm, arg| {
    if arg.is_string() {
      return Ok(arg);
    }

    let result = (arg).display(vm);
    Ok(vm.allocate_string(&result))
  };

  /// The number of characters in the string
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::length // 5
  /// '🏃' >> string::length // 1
  /// ```
  #[type(string => number)]
  fn length() = |vm, arg| {
    let string = get_string(arg, vm)?;
    let result = string.chars().count();

    Ok((result as f64).into())
  };
  /// The size of the string's buffer in bytes
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::byteLength // 5
  /// '🏃' >> string::byteLength // 4
  /// ```
  #[type(string => number)]
  fn byteLength() = |vm, arg| {
    let result = get_string(arg, vm)?.len();

    Ok((result as f64).into())
  };

  /// Is the string empty?
  ///
  /// ## Example
  /// ```bang
  /// '' >> string::isEmpty // true
  /// 'a' >> string::isEmpty // false
  /// ```
  #[type(string => boolean)]
  fn isEmpty() = |vm, arg| {
    let result = get_string(arg, vm)?.is_empty();

    Ok(result.into())
  };
  /// Are all the characters in the string ASCII?
  ///
  /// ## Example
  /// ```bang
  /// 'hello!' >> string::isAscii // true
  /// 'hiya 👋' >> string::isAscii // false
  /// ```
  #[type(string => boolean)]
  fn isAscii() = |vm, arg| {
    let result = get_string(arg, vm)?.is_ascii();

    Ok(result.into())
  };

  /// Returns a string with all characters converted to lowercase
  ///
  /// (`Lowercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::toLowercase // 'hello'
  /// 'HELLO' >> string::toLowercase // 'hello'
  /// ```
  #[type(string => string)]
  fn toLowercase() = |vm, arg| {
    let result = get_string(arg, vm)?.to_lowercase();

    Ok(vm.allocate_string(&result))
  };
  /// Returns a string with all characters converted to uppercase
  ///
  /// (`Uppercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::toUppercase // 'HELLO'
  /// 'HELLO' >> string::toUppercase // 'HELLO'
  /// ```
  #[type(string => string)]
  fn toUppercase() = |vm, arg| {
    let result = get_string(arg, vm)?.to_uppercase();

    Ok(vm.allocate_string(&result))
  };

  /// Is the string a substring of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'welcome to earth' >> string::contains('come') // true
  /// 'welcome to earth' >> string::contains('hello') // false
  /// ```
  #[type(string => string => boolean)]
  fn contains() = |vm, arg| {
    fn func(vm: &mut VM, pat: Value, string: Value) -> Result<Value, ErrorKind> {
      let string = get_string(string, vm)?;
      let result = string.contains(get_string(pat, vm)?);
      Ok(result.into())
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = vm.heap.allocate(NativeClosure::new("string::contains", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
  /// Is a string a prefix of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::startsWith('he') // true
  /// 'hello' >> string::startsWith('lo') // false
  /// ```
  #[type(string => string => boolean)]
  fn startsWith() = |vm, arg| {
    fn func(vm: &mut VM, pat: Value, string: Value) -> Result<Value, ErrorKind> {
      let string = get_string(string, vm)?;
      let result = string.starts_with(pat.as_string(&vm.heap));
      Ok(result.into())
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = vm.heap.allocate(NativeClosure::new("string::startsWith", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };
  /// Is a string a suffix of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::endsWith('lo') // true
  /// string::endsWith('he')('hello') // false
  /// ```
  #[type(string => string => boolean)]
  fn endsWith() = |vm, arg| {
    fn func(vm: &mut VM, pat: Value, string: Value) -> Result<Value, ErrorKind> {
      let string = get_string(string, vm)?;
      let result = string.ends_with(pat.as_string(&vm.heap));
      Ok(result.into())
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = vm.heap.allocate(NativeClosure::new("string::endsWith", func, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Returns a string with leading and trailing whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trim('  hello  ') // 'hello'
  /// ```
  #[type(string => string)]
  fn trim() = |vm, arg| {
    let string = get_string(arg, vm)?;
    let result = string.trim();

    let start = result.as_ptr().addr() - string.as_ptr().addr();
    let end = start + result.len();

    if start == 0 && end == string.len() {
      Ok(arg)
    } else {
      let view = vm.heap.allocate(StringView::new(vm, arg, start, end));
      Ok(Value::from_object(view, STRING_VIEW_TYPE_ID))
    }
  };
  /// Returns a string with leading whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trimStart('  hello') // 'hello'
  /// ```
  #[type(string => string)]
  fn trimStart() = |vm, arg| {
    let string = get_string(arg, vm)?;
    let result = string.trim_start();

    let start = result.as_ptr().addr() - string.as_ptr().addr();

    if start == 0  {
      Ok(arg)
    } else {
      let view = vm.heap.allocate(StringView::new(vm, arg, start, string.len()));
      Ok(Value::from_object(view, STRING_VIEW_TYPE_ID))
    }
  };
  /// Returns a string with trailing whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trimEnd('hello   ') // 'hello'
  /// ```
  #[type(string => string)]
  fn trimEnd() = |vm, arg| {
    let string = get_string(arg, vm)?;
    let result = string.trim_end();

    let end = result.len();

    if end == string.len() {
      Ok(arg)
    } else {
      let view = vm.heap.allocate(StringView::new(vm, arg, 0, end));
      Ok(Value::from_object(view, STRING_VIEW_TYPE_ID))
    }
  };


  /// Replace all occurrences of a pattern in a string with a replacement.
  /// Returns a new string.
  ///
  /// ## Example
  /// ```bang
  /// 'this is old' >> string::replaceAll('old')('new') // 'this is new'
  /// 'this is old' >> string::replaceAll('i')('I') // 'thIs Is new'
  /// ```
  #[type(string => string => string => string)]
  fn replaceAll() = |vm, arg| {
    fn func_one(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
      if !b.is_string() {
        return Err(ErrorKind::TypeError { expected: "string", got: b.get_type(vm) });
      }

      let closure = (vm.heap).allocate(NativeClosureTwo::new("string::replaceAll", func_two, a, b));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TWO_TYPE_ID))
    }
    fn func_two(vm: &mut VM, pat: Value, rep: Value, string: Value) -> Result<Value, ErrorKind> {
      let output = str::replace(
        get_string(string, vm)?,
        pat.as_string(&vm.heap),
        rep.as_string(&vm.heap),
      );
      Ok(vm.allocate_string(&output))
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = (vm.heap).allocate(NativeClosure::new("string::replaceAll", func_one, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };


  /// Replace the first occurrence of a pattern in a string with a replacement.
  /// Returns a new string.
  ///
  /// ## Example
  /// ```bang
  /// 'this is old' >> string::replaceOne('old')('new') // 'this is new'
  /// 'this is old' >> string::replaceOne('i')('I') // 'thIs is new'
  /// ```
  #[type(string => string => string => string)]
  fn replaceOne() = |vm, arg| {
    fn func_one(vm: &mut VM, a: Value, b: Value) -> Result<Value, ErrorKind> {
      if !b.is_string() {
        return Err(ErrorKind::TypeError { expected: "string", got: b.get_type(vm) });
      }

      let closure = vm.heap.allocate(NativeClosureTwo::new("string::replaceOne", func_two, a, b));
      Ok(Value::from_object(closure, NATIVE_CLOSURE_TWO_TYPE_ID))
    }
    fn func_two(vm: &mut VM, pat: Value, rep: Value, string: Value) -> Result<Value, ErrorKind> {
      let output = str::replacen(
        get_string(string, vm)?,
        pat.as_string(&vm.heap),
        rep.as_string(&vm.heap),
        1
      );
      Ok(vm.allocate_string(&output))
    }

    if !arg.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: arg.get_type(vm) });
    }

    let closure = vm.heap.allocate(NativeClosure::new("string::replaceOne", func_one, arg));
    Ok(Value::from_object(closure, NATIVE_CLOSURE_TYPE_ID))
  };

  /// Parses a string into a number
  ///
  /// ## Example
  /// ```bang
  /// string::parseNumber('3') // Some(3.0)
  /// string::parseNumber('3.14') // Some(3.14)
  /// string::parseNumber('3.14e-2') // Some(0.0314)
  /// string::parseNumber('three') // None
  /// string::parseNumber('Infinity') // Some(inf)
  /// ```
  #[type(string => option<number>)]
  fn parseNumber() = |vm, arg| {
    let string = get_string(arg, vm)?;
    let result = string.parse::<f64>().map(Value::from);

    match result {
      Ok(value) => Ok(Value::from_object(vm.heap.allocate(value), SOME_TYPE_ID)),
      Err(_) => Ok(Value::NONE)
    }
  };

  /// Creates an iterator over the characters in the string.
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::chars >> iter::toList // ['h', 'e', 'l', 'l', 'o']
  /// 'hi 🏃' >> string::chars >> iter::toList // ['h', 'i', ' ', '🏃']
  /// ```
  #[type(string => iterator<string>)]
  fn chars() = |vm, base| {
    fn next(vm: &mut VM, state: usize, base: Value) -> Option<(Value, usize)> {
      let string = base.as_string(&vm.heap).as_bytes();

      if state >= string.len() {
        return None;
      }

      let length = match string[state] {
        x if (x & 0b1111_0000) == 0b1111_0000 => 4,
        x if (x & 0b1110_0000) == 0b1110_0000 => 3,
        x if (x & 0b1100_0000) == 0b1100_0000 => 2,
        _ => 1,
      };

      let view = vm.heap.allocate(StringView::new(vm, base, state, state + length));
      Some((Value::from_object(view, STRING_VIEW_TYPE_ID), state + length))
    }

    if !base.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: base.get_type(vm) });
    }
    let length = base.as_string(&vm.heap).chars().count();

    let iterator = vm.heap.allocate(Iterator {
      base,
      next,
      length: IteratorLength::Max(length),
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };
  /// Creates an iterator over the lines in a string
  ///
  /// Lines are split at line endings that are either newlines (\n) or sequences of a
  /// carriage return followed by a line feed (\r\n).
  ///
  /// Any carriage return (\r) not immediately followed by a line feed (\n) does not split a line.
  /// These carriage returns are thereby included in the produced lines.
  ///
  /// The final line ending is optional. A string that ends with a final line ending will return
  /// the same lines as an otherwise identical string without a final line ending.
  ///
  /// ## Example
  /// ```bang
  /// 'hello
  /// world
  /// this has three lines'
  ///   >> string::lines
  ///   >> iter::toList // ['hello', 'world', 'this has three lines']
  /// ```
  #[type(string => iterator<string>)]
  fn lines() = |vm, base| {
    fn next(vm: &mut VM, state: usize, base: Value) -> Option<(Value, usize)> {
      let string = base.as_string(&vm.heap).as_bytes();

      if state >= string.len() {
        return None;
      }

      let start = state;
      let mut end = state;
      let next_state = loop {
        if end >= string.len() {
          break end + 1;
        }

        if string[end] == b'\n' {
          break end + 1;
        }
        if string[end] == b'\r' && end + 1 < string.len() && string[end + 1] == b'\n' {
          break end + 2;
        }

        end += match string[end] {
          x if (x & 0b1111_0000) == 0b1111_0000 => 4,
          x if (x & 0b1110_0000) == 0b1110_0000 => 3,
          x if (x & 0b1100_0000) == 0b1100_0000 => 2,
          _ => 1,
        };
      };

      let view = vm.heap.allocate(StringView::new(vm, base, start, end));
      Some((Value::from_object(view, STRING_VIEW_TYPE_ID), next_state))
    }

    if !base.is_string() {
      return Err(ErrorKind::TypeError { expected: "string", got: base.get_type(vm) });
    }
    let length = base.as_string(&vm.heap).lines().count();

    let iterator = vm.heap.allocate(Iterator {
      base,
      next,
      length: IteratorLength::Max(length),
    });
    Ok(Value::from_object(iterator, ITERATOR_TYPE_ID))
  };
});

#[allow(clippy::inline_always, reason = "function exists to simplify macros")]
#[inline(always)]
fn get_string<'a>(value: Value, vm: &'a VM) -> Result<&'a str, ErrorKind> {
  if value.is_string() {
    Ok(value.as_string(&vm.heap))
  } else {
    Err(ErrorKind::TypeError {
      expected: "string",
      got: value.get_type(vm),
    })
  }
}
