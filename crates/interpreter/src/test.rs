use crate::{compile, vm::allocate_string, HeapSize, Value, VM};
use bang_parser::{parse, Allocator};
use indoc::indoc;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Error {
  CompileError,
  RuntimeError,
}
impl From<crate::CompileError> for Error {
  fn from(_: crate::CompileError) -> Self {
    Self::CompileError
  }
}
impl From<crate::RuntimeError> for Error {
  fn from(_: crate::RuntimeError) -> Self {
    Self::RuntimeError
  }
}

fn run(source: &str) -> Result<VM, Error> {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);
  assert!(ast.is_valid());
  let chunk = compile(&ast)?;
  let mut vm = VM::new(HeapSize::Small).unwrap();
  vm.define_builtin_functions();
  vm.run(&chunk)?;
  Ok(vm)
}

macro assert_variable {
  ($vm:expr; $name:ident, string $string:literal) => {
    let vm = $vm.as_mut().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    let string = allocate_string(&mut vm.heap, $string);
    assert!(
      value.equals(string, &vm.heap),
      "{} (type {}) != {} (type {})",
      value.display(&vm.heap),
      value.get_type(),
      string.display(&vm.heap),
      string.get_type(),
    );
  },
  ($vm:expr; $name:ident, $value:expr) => {
    let vm = $vm.as_ref().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    assert_eq!(value, Value::from($value));
  }
}

#[test]
fn add_numbers() {
  let vm = run(indoc! {"
    let a = 1 + 2
    let b = 3.3 + 4.2
    let c = 1000.2 - 35.7
  "});
  assert_variable!(vm; a, 3.0);
  assert_variable!(vm; b, 7.5);
  assert_variable!(vm; c, 964.5);
}

#[test]
fn subtract_numbers() {
  let vm = run(indoc! {"
    let a = 5 - 2
    let b = 10.1 - 2.6
    let c = 964.99 - .49
  "});
  assert_variable!(vm; a, 3.0);
  assert_variable!(vm; b, 7.5);
  assert_variable!(vm; c, 964.5);
}

#[test]
fn multiply_numbers() {
  let vm = run(indoc! {"
    let a = 5 * 2
    let b = 5.0 * 2.0
    let c = 3.2 * 2
    let d = 0.5 * 16.4
  "});
  assert_variable!(vm; a, 10.0);
  assert_variable!(vm; b, 10.0);
  assert_variable!(vm; c, 6.4);
  assert_variable!(vm; d, 8.2);
}

#[test]
fn divide_numbers() {
  let vm = run(indoc! {"
    let a = 5 / 2
    let b = 5.0 / 2.0
    let c = 22 / 11
  "});
  assert_variable!(vm; a, 2.5);
  assert_variable!(vm; b, 2.5);
  assert_variable!(vm; c, 2.0);
}

#[test]
fn remainder_numbers() {
  let vm = run(indoc! {"
    let a = 5 % 3
    let b = -5 % 3
    let c = 5 % -3
    let d = -5 % -3
  "});
  assert_variable!(vm; a, 2.0);
  assert_variable!(vm; b, -2.0);
  assert_variable!(vm; c, 2.0);
  assert_variable!(vm; d, -2.0);
}

#[test]
fn bidmas() {
  let vm = run(indoc! {"
    let a = (5 - (3 - 1)) + -1
    let b = 5 + 3 * 2
  "});
  assert_variable!(vm; a, 2.0);
  assert_variable!(vm; b, 11.0);
}

#[test]
fn numeric_operators_non_numbers() {
  let cant_add_boolean_and_number = run("let a = true + 4");
  assert!(cant_add_boolean_and_number.is_err());

  let cant_add_string_and_number = run("let a = \"Hello\" + 4");
  assert!(cant_add_string_and_number.is_err());

  let cant_multiply_string = run("let a = \"Hello\" * 4");
  assert!(cant_multiply_string.is_err());

  let cant_divide_null = run("let a = null / 4");
  assert!(cant_divide_null.is_err());

  let cant_add_boolean_null = run("let a = false + null");
  assert!(cant_add_boolean_null.is_err());
}

#[test]
fn negate() {
  let vm = run(indoc! {"
    let a = - 2
    let b = - 2.6
    let c = -4525
  "});
  assert_variable!(vm; a, -2.0);
  assert_variable!(vm; b, -2.6);
  assert_variable!(vm; c, -4525.0);

  let cant_negate_strings = run("let a = -'hello'");
  assert!(cant_negate_strings.is_err());

  let lots_of_minuses = run(indoc! {"
    let a = 4---4
    let b = 4--------------------------------4
  "});
  assert_variable!(lots_of_minuses; a, 0.0);
  assert_variable!(lots_of_minuses; b, 8.0);
}

#[test]
fn concatenate_strings() {
  let mut vm = run(indoc! {"
    let a = \"Hello \" ++ \"World\"
    let b = 'Whats Up' ++ `?`
    let c = \"Merged\" ++ 'together'

    // Keep original strings
    let d = a ++ b
  "});
  assert_variable!(vm; a, string "Hello World");
  assert_variable!(vm; b, string "Whats Up?");
  assert_variable!(vm; c, string "Mergedtogether");
  assert_variable!(vm; d, string "Hello WorldWhats Up?");

  let numeric_add_for_strings = run(indoc! {"'a' + 'b'"});
  assert!(numeric_add_for_strings.is_err());

  let non_string_concat = run(indoc! {"'a' ++ 3"});
  assert!(non_string_concat.is_err());

  let non_string_concat = run(indoc! {"3 ++ 'a'"});
  assert!(non_string_concat.is_err());
}

#[test]
fn not() {
  let vm = run(indoc! {"
    let a = !!true
    let b = !\"\"
    let c = !true
    let d = !false
    let e = !\"Hello\"
    let f = !0
    let g = !-1
    let h = !3
    let i = !('' ++ '')
    let j = !(x => x)
  "});

  assert_variable!(vm; a, true);
  assert_variable!(vm; b, true);
  assert_variable!(vm; c, false);
  assert_variable!(vm; d, true);
  assert_variable!(vm; e, false);
  assert_variable!(vm; f, true);
  assert_variable!(vm; g, false);
  assert_variable!(vm; h, false);
  assert_variable!(vm; i, true);
  assert_variable!(vm; j, false);
}

#[test]
fn equals() {
  let numbers = run(indoc! {"
    let a = 3 == 4
    let b = 3 == 3
    let c = -5 == 5
    let d = 0 == -0
    let e = 0.3 == (0.1 + 0.2)
  "});
  assert_variable!(numbers; a, false);
  assert_variable!(numbers; b, true);
  assert_variable!(numbers; c, false);
  assert_variable!(numbers; d, true);
  assert_variable!(numbers; e, true);

  let strings = run(indoc! {"
    let a = 'hello' == \"hello\"
    let b = 'hello' == \"goodbye\"
    let c = 'hello' == 'hello!'
    let d = \"hello\" == \"hello\"
    let e = \"hello\" == \"hel\" ++ \"lo\"
  "});
  assert_variable!(strings; a, true);
  assert_variable!(strings; b, false);
  assert_variable!(strings; c, false);
  assert_variable!(strings; d, true);
  assert_variable!(strings; e, true);

  let built_strings = run(indoc! {"
    let a = 'hel' ++ 'lo' == 'hello'
    let b = 'hell' ++ 'o' == 'he' ++ 'llo'
  "});
  assert_variable!(built_strings; a, true);
  assert_variable!(built_strings; b, true);

  let booleans = run(indoc! {"
    let a = true == true
    let b = true == false
    let c = false == false
  "});
  assert_variable!(booleans;a, true);
  assert_variable!(booleans;b, false);
  assert_variable!(booleans;c, true);

  let mixed_types = run(indoc! {"
    let a = '' == false
    let b = 0 == false
    let c = 'null' == 0
    let d = '' == false
    let e = '0' == 0
  "});
  assert_variable!(mixed_types; a, false);
  assert_variable!(mixed_types; b, false);
  assert_variable!(mixed_types; c, false);
  assert_variable!(mixed_types; d, false);
  assert_variable!(mixed_types; e, false);

  let functions = run(indoc! {"
    let a = x => 7
    let b = _ => 7

    let c = a == a
    let d = a == b
    let e = b == b
  "});
  assert_variable!(functions; c, true);
  assert_variable!(functions; d, false);
  assert_variable!(functions; e, true);

  let closures = run(indoc! {"
    let x = v => _ => v
    let y = x(7)
    let z = _ => 7

    let c = y == z
    let d = y == y
    let e = z == z
  "});
  assert_variable!(closures; c, false);
  assert_variable!(closures; d, true);
  assert_variable!(closures; e, true);
}

#[test]
fn not_equals() {
  let numbers = run(indoc! {"
    let a = 3 != 4
    let b = 3 != 3
    let c = -5 != 5
  "});
  assert_variable!(numbers; a, true);
  assert_variable!(numbers; b, false);
  assert_variable!(numbers; c, true);

  let strings = run(indoc! {"
    let a = 'hello' != \"hello\"
    let b = 'hello' != \"goodbye\"
    let c = 'hello' != 'hello!'
    let d = \"hello\" != \"hello\"
    let e = \"hello\" ++ 'x' != 7
  "});
  assert_variable!(strings; a, false);
  assert_variable!(strings; b, true);
  assert_variable!(strings; c, true);
  assert_variable!(strings; d, false);

  let booleans = run(indoc! {"
    let a = true != true
    let b = true != false
    let c = false != false
  "});
  assert_variable!(booleans; a, false);
  assert_variable!(booleans; b, true);
  assert_variable!(booleans; c, false);

  let mixed_types = run(indoc! {"
    let a = '' != false
    let b = 0 != false
    let c = 'null' != 0
    let d = '' != false
    let e = '0' != 0
  "});
  assert_variable!(mixed_types; a, true);
  assert_variable!(mixed_types; b, true);
  assert_variable!(mixed_types; c, true);
  assert_variable!(mixed_types; d, true);
  assert_variable!(mixed_types; e, true);
}

#[test]
fn less_than() {
  let numbers = run(indoc! {"
    let a = 3 < 4
    let b = 3 < 3
    let c = -5 < 5
    let d = 10 < -10
  "});
  assert_variable!(numbers; a, true);
  assert_variable!(numbers; b, false);
  assert_variable!(numbers; c, true);
  assert_variable!(numbers; d, false);

  let strings = run(indoc! {"
    let a = 'b' < 'c'
    let b = 'b' < 'b'
    let c = 'H' < 'h'
    let d = 'hello' < 'Hello'
  "});
  assert_variable!(strings; a, true);
  assert_variable!(strings; b, false);
  assert_variable!(strings; c, true);
  assert_variable!(strings; d, false);

  let mixed_types = run("3 < ''");
  assert!(mixed_types.is_err());

  let or_equal_to = run(indoc! {"
    let a = 3 <= 4
    let b = 3 <= 3
    let c = -5 <= 5
    let d = 10 <= -10
  "});
  assert_variable!(or_equal_to; a, true);
  assert_variable!(or_equal_to; b, true);
  assert_variable!(or_equal_to; c, true);
  assert_variable!(or_equal_to; d, false);
}

#[test]
fn greater_than() {
  let numbers = run(indoc! {"
    let a = 3 > 4
    let b = 3 > 3
    let c = -5 > 5
    let d = 10 > -10
  "});
  assert_variable!(numbers; a, false);
  assert_variable!(numbers; b, false);
  assert_variable!(numbers; c, false);
  assert_variable!(numbers; d, true);

  let strings = run(indoc! {"
    let a = 'b' > 'c'
    let b = 'b' > 'b'
    let c = 'H' > 'h'
    let d = 'hello' > 'Hello'
  "});
  assert_variable!(strings; a, false);
  assert_variable!(strings; b, false);
  assert_variable!(strings; c, false);
  assert_variable!(strings; d, true);

  let mixed_types = run("3 > ''");
  assert!(mixed_types.is_err());

  let or_equal_to = run(indoc! {"
    let a = 3 >= 4
    let b = 3 >= 3
    let c = -5 >= 5
    let d = 10 >= -10
  "});
  assert_variable!(or_equal_to; a, false);
  assert_variable!(or_equal_to; b, true);
  assert_variable!(or_equal_to; c, false);
  assert_variable!(or_equal_to; d, true);
}

#[test]
fn and() {
  let mut vm = run(indoc! {"
    let a = false and true
    let b = false and false
    let c = true and true
    let d = true and false
    let e = true and 'Hello'
  "});
  assert_variable!(vm; a, false);
  assert_variable!(vm; b, false);
  assert_variable!(vm; c, true);
  assert_variable!(vm; d, false);
  assert_variable!(vm; e, string "Hello");
}

#[test]
fn or() {
  let vm = run(indoc! {"
    let a = false or true
    let b = false or false
    let c = true or true
    let d = true or false
    let e = true or \"Hello\"
  "});
  assert_variable!(vm; a, true);
  assert_variable!(vm; b, false);
  assert_variable!(vm; c, true);
  assert_variable!(vm; d, true);
  assert_variable!(vm; e, true);
}

#[test]
fn block() {
  let single_expression = run(indoc! {"
    let a = { false }
    let b = {
      7
    }
  "});
  assert_variable!(single_expression; a, false);
  assert_variable!(single_expression; b, 7.0);

  let multiple_statements = run(indoc! {"
    let a = {
      true
      false
    }
    let b = {
      4 + 55 // this is a comment
      7
    }
    let c = {
      // This is a comment
      33
    }
    let d = {
      // This is a comment
      let _var = 3
      44 / 22
    }
  "});
  assert_variable!(multiple_statements; a, false);
  assert_variable!(multiple_statements; b, 7.0);
  assert_variable!(multiple_statements; c, 33.0);
  assert_variable!(multiple_statements; d, 2.0);
}

#[test]
fn call() {
  let returns_literal = run(indoc! {"
    let a = _ => 7
    let b = a()
  "});
  assert_variable!(returns_literal; b, 7.0);

  let returns_calculation = run(indoc! {"
    let a = _ => 7 + 5
    let b = a(false)
  "});
  assert_variable!(returns_calculation; b, 12.0);

  let uses_argument = run(indoc! {"
    let a = x => x + 1
    let b = a(0)
    let c = a(2)
    let d = a(-3)
  "});
  assert_variable!(uses_argument; b, 1.0);
  assert_variable!(uses_argument; c, 3.0);
  assert_variable!(uses_argument; d, -2.0);

  let call_number = run(indoc! {"3(0)"});
  assert!(call_number.is_err());

  let call_string = run(indoc! {"'hello world'(0)"});
  assert!(call_string.is_err());

  let call_boolean = run(indoc! {"false(0)"});
  assert!(call_boolean.is_err());
}

#[test]
fn pipeline() {
  let pipeline = run(indoc! {"
    let addOne = x => x + 1
    let multiplyByThree = x => x * 3

    let a = 3 >> addOne >> multiplyByThree
    let b = 3 >> addOne >> multiplyByThree >> addOne
    let c = 3 >> addOne
    let d = 66 >> multiplyByThree
  "});
  assert_variable!(pipeline; a, 12.0);
  assert_variable!(pipeline; b, 13.0);
  assert_variable!(pipeline; c, 4.0);
  assert_variable!(pipeline; d, 198.0);
}

#[test]
fn if_() {
  let vm = run(indoc! {"
    let a = if (true) 1 else 2
    let b = if (false) 3 else 4
    let c = if (3 < 4) 5 else 6
  "});

  assert_variable!(vm; a, 1.0);
  assert_variable!(vm; b, 4.0);

  let no_else = run(indoc! {"
    let a = if (false) 4
    let b = if (true) 1.1
  "});
  assert_variable!(no_else; a, ());
  assert_variable!(no_else; b, 1.1);
}

#[test]
fn access_variables_in_higher_scopes() {
  let vm = run(indoc! {"
    let a = 5
    let b = {
      let c = 3
      a + c
    }
  "});

  assert_variable!(vm; a, 5.0);
  assert_variable!(vm; b, 8.0);
}

#[test]
fn closures() {
  let single_closure = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => a
      x(0)
    }
  "});
  assert_variable!(single_closure; value, 5.0);

  let use_value = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => a + 4
      x(0)
    }
  "});
  assert_variable!(use_value; value, 9.0);

  let close_over_twice = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => a + 4
      let y = _ => a
      x(0) + y(0)
    }
  "});
  assert_variable!(close_over_twice; value, 14.0);

  let use_after_closure = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => a
      let b = a
      x(0) + b
    }
  "});
  assert_variable!(use_after_closure; value, 10.0);

  let nested_closure_intermediate = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => {
        let b = a + 1
        _ => b
      }
      x(0)(0)
    }
  "});
  assert_variable!(nested_closure_intermediate; value, 6.0);

  let nested_closure = run(indoc! {"
    let value = {
      let a = 5
      let x = _ => _ => a
      x(0)(0)
    }
  "});
  assert_variable!(nested_closure; value, 5.0);
}

#[test]
fn closure_twice() {
  let two_parameters = run(indoc! {"
    let add = a => b => a + b
    let x = add(2)(3)
    let y = add(3.5)(0.25)
  "});
  assert_variable!(two_parameters; x, 5.0);
  assert_variable!(two_parameters; y, 3.75);
}

#[test]
fn match_() {
  let single_catch_all = run("let a = match 5 | x -> x + 1");
  assert_variable!(single_catch_all; a, 6.0);

  let multiple_catch_all = run("let a = match 0 | x -> x + 1 | y -> y + 2");
  assert_variable!(multiple_catch_all; a, 1.0);

  let literal_no_catch_all = run("let a = match true | true -> 1");
  assert_variable!(literal_no_catch_all; a, 1.0);
  let literal_no_catch_all = run("let a = (match false | true -> 1) + 1");
  assert!(literal_no_catch_all.is_err());

  let literal = run("let a = match true | true -> 1 | _ -> 2");
  assert_variable!(literal; a, 1.0);

  let literal_multi_case = run(indoc! {"
    let a = match true | false -> 1 | true -> 2 | _ -> 3
    let b = match false | false -> 1 | true -> 2 | _ -> 3
    let c = match 4 | false -> 1 | true -> 2 | _ -> 3
  "});
  assert_variable!(literal_multi_case; a, 2.0);
  assert_variable!(literal_multi_case; b, 1.0);
  assert_variable!(literal_multi_case; c, 3.0);

  let max_range = run(indoc! {"
    let a = match 5 | ..10 -> 1 | _ -> 2
    let b = match 10 | ..10 -> 1 | _ -> 2
    let c = match 15 | ..10 -> 1 | _ -> 2
  "});
  assert_variable!(max_range; a, 1.0);
  assert_variable!(max_range; b, 1.0);
  assert_variable!(max_range; c, 2.0);

  let min_range = run(indoc! {"
    let a = match 5 | 10.. -> 1 | _ -> 2
    let b = match 10 | 10.. -> 1 | _ -> 2
    let c = match 15 | 10.. -> 1 | _ -> 2
  "});
  assert_variable!(min_range; a, 2.0);
  assert_variable!(min_range; b, 1.0);
  assert_variable!(min_range; c, 1.0);

  let both_range = run(indoc! {"
    let a = match 5 | 10..15 -> 1 | _ -> 2
    let b = match 10 | 10..15 -> 1 | _ -> 2
    let c = match 12 | 10..15 -> 1 | _ -> 2
    let d = match 15 | 10..15 -> 1 | _ -> 2
    let e = match 20 | 10..15 -> 1 | _ -> 2
  "});
  assert_variable!(both_range; a, 2.0);
  assert_variable!(both_range; b, 1.0);
  assert_variable!(both_range; c, 1.0);
  assert_variable!(both_range; d, 1.0);
  assert_variable!(both_range; e, 2.0);

  let literals_cleanedup = run(indoc! {"
    let a = match false
      | true -> 1
      | false -> 2

    let b = {
      let c = 7
      c
    }
  "});
  assert_variable!(literals_cleanedup; b, 7.0);
}

#[test]
fn match_guards() {
  let vm = run(indoc! {"
    let a = match true | true if false -> 1 | false -> 2 | _ -> 3
    let b = match true | true if true -> 1 | false -> 2 | _ -> 3
    let c = match 5 | ..6 if false -> 1 | 2..8 if true -> 2 | _ -> 3
    let d = match 5 | ..6 if true -> 1 | 2..8 if true -> 2 | _ -> 3
    let e = match 12 | c if c > 3 -> 1 | _ -> 2
    let f = match 12 | c if c > 15 -> 1 | c if c > 10 -> 2 | _ -> 3
    let g = match 12 | c if c > 10 -> c | _ -> 3
    let h = match 12 | c if false -> c | _ -> 3
  "});
  assert_variable!(vm; a, 3.0);
  assert_variable!(vm; b, 1.0);
  assert_variable!(vm; c, 2.0);
  assert_variable!(vm; d, 1.0);
  assert_variable!(vm; e, 1.0);
  assert_variable!(vm; f, 2.0);
  assert_variable!(vm; g, 12.0);
  assert_variable!(vm; h, 3.0);
}

#[test]
fn fibonacci_match() {
  let fibonnacci_match = run(indoc! {"
    let fibonnacciMatch = n => match n
      | ..2 -> 1
      | n -> fibonnacciMatch(n - 1) + fibonnacciMatch(n - 2)
    let a = fibonnacciMatch(0)
    let b = fibonnacciMatch(6)
  "});
  assert_variable!(fibonnacci_match; a, 1.0);
  assert_variable!(fibonnacci_match; b, 8.0);
}

#[test]
fn format_string() {
  let mut vm = run(indoc! {"
    let a = `hello {55}`
    let b = `there were {33 + 25} people`
    let c = `i can quote {true} or {false}`
    let d = `nested stuff: {{55} + 1 }`
    let e = `{`{55}`}`
  "});
  assert_variable!(vm; a, string "hello 55");
  assert_variable!(vm; b, string "there were 58 people");
  assert_variable!(vm; c, string "i can quote true or false");
  assert_variable!(vm; d, string "nested stuff: 56");
  assert_variable!(vm; e, string "55");

  let mut vm = run(indoc! {"
    let a = `{'hello'}`
    let b = `{55.2}`
    let c = `{false}`
    let d = `{true}`
    let e = `{print}`

    let identity = x => x
    let f = `{identity}`
    let g = `{x => x}`
    let h = `{(x => _ => x)()}`
  "});
  assert_variable!(vm; a, string "hello");
  assert_variable!(vm; b, string "55.2");
  assert_variable!(vm; c, string "false");
  assert_variable!(vm; d, string "true");
  assert_variable!(vm; e, string "<function print>");
  assert_variable!(vm; f, string "<function identity>");
  assert_variable!(vm; g, string "<function>");
  assert_variable!(vm; h, string "<closure <function>>");
}

#[test]
fn return_statement() {
  let redundant = run(indoc! {"
    let a = (_ => { return 5 })()
    let b = (_ => ({ return 5 }))()
    let c = (_ => { return 5 + 5 })()
  "});
  assert_variable!(redundant; a, 5.0);
  assert_variable!(redundant; b, 5.0);
  assert_variable!(redundant; c, 10.0);

  let partial_early_return = run(indoc! {"
    let a = (_ => if (true) { return 5 } else { 6 })()
    let b = (_ => if (false) { 5 } else { return 6 })()
    let c = (_ => if (false) { 5 } else { return 6 })()
  "});
  assert_variable!(partial_early_return; a, 5.0);
  assert_variable!(partial_early_return; b, 6.0);
  assert_variable!(partial_early_return; c, 6.0);

  let early_return = run(indoc! {"
    let a = (_ => {
      let x = if (true) { return 5 } else { 6 }
      x + 4
    })()

    let b = (_ => {
      let x = if (false) { return 5 } else { 6 }
      x + 4
    })()
  "});
  assert_variable!(early_return; a, 5.0);
  assert_variable!(early_return; b, 10.0);

  let code_after = run(indoc! {"
    let a = (_ => { return 5\n 4 })()
    let b = (_ => ({ return 5 }))()
    let c = (_ => { return 5 + 5 })()
  "});
  assert_variable!(code_after; a, 5.0);
}

#[test]
fn native_function() {
  let mut vm = run(indoc! {"
    let a = {
      let x = type(55)
      x
    }
  "});
  assert_variable!(vm; a, string "number");
}

mod builtin_function {
  use super::{assert_variable, run};

  #[test]
  fn type_of() {
    let mut string = run("let a = type('hello')");
    assert_variable!(string; a, string "string");

    let mut allocated_string = run("let a = type('hello' ++ ' world')");
    assert_variable!(allocated_string; a, string "string");

    let mut number = run("let a = type(55.2)");
    assert_variable!(number; a, string "number");

    let mut r#false = run("let a = type(false)");
    assert_variable!(r#false; a, string "boolean");

    let mut r#true = run("let a = type(true)");
    assert_variable!(r#true; a, string "boolean");

    let mut builtin_function = run("let a = type(print)");
    assert_variable!(builtin_function; a, string "function");

    let mut named_function = run("let identity = x => x\nlet a = type(identity)");
    assert_variable!(named_function; a, string "function");

    let mut anonymous_function = run("let a = type(x => x)");
    assert_variable!(anonymous_function; a, string "function");

    let mut closure = run("let a = type((x => _ => x)())");
    assert_variable!(closure; a, string "function");
  }
}
