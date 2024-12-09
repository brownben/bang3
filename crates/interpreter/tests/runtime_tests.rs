//! # Runtime Tests
//!
//! Check that the output of the interpreted code is correct.

use bang_gc::HeapSize;
use bang_interpreter::{EmptyContext, StandardContext, VM, Value, compile};
use bang_syntax::parse;
use indoc::indoc;

fn run(source: &str) -> Result<VM, ()> {
  let ast = parse(source.to_owned());
  assert!(ast.is_valid());
  let chunk = compile(&ast).unwrap();
  let mut vm = VM::new(HeapSize::Small, &StandardContext).unwrap();
  vm.run(&chunk).map_err(|_| ())?;
  Ok(vm)
}

macro_rules! assert_variable {
  ($vm:expr; $name:ident, string $string:literal) => {
    let vm = $vm.as_mut().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    let string = vm.allocate_string($string);

    assert!(
      vm.equals(value, string),
      "{} (type {}) != {} (type {})",
      value.display(&vm),
      value.get_type(&vm),
      string.display(&vm),
      string.get_type(&vm),
    );
  };
  ($vm:expr; $name:ident, $value:expr) => {
    let vm = $vm.as_ref().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    assert_eq!(value, Value::from($value));
  };
}
use assert_variable;

#[test]
fn numeric_operators() {
  let add = run(indoc! {"
    let a = 1 + 2
    let b = 3.3 + 4.2
    let c = 1000.2 - 35.7
  "});
  assert_variable!(add; a, 3.0);
  assert_variable!(add; b, 7.5);
  assert_variable!(add; c, 964.5);

  let subtract = run(indoc! {"
    let a = 5 - 2
    let b = 10.1 - 2.6
    let c = 964.99 - .49
  "});
  assert_variable!(subtract; a, 3.0);
  assert_variable!(subtract; b, 7.5);
  assert_variable!(subtract; c, 964.5);

  let multiply = run(indoc! {"
    let a = 5 * 2
    let b = 5.0 * 2.0
    let c = 3.2 * 2
    let d = 0.5 * 16.4
  "});
  assert_variable!(multiply; a, 10.0);
  assert_variable!(multiply; b, 10.0);
  assert_variable!(multiply; c, 6.4);
  assert_variable!(multiply; d, 8.2);

  let divide = run(indoc! {"
    let a = 5 / 2
    let b = 5.0 / 2.0
    let c = 22 / 11
  "});
  assert_variable!(divide; a, 2.5);
  assert_variable!(divide; b, 2.5);
  assert_variable!(divide; c, 2.0);

  let remainder = run(indoc! {"
    let a = 5 % 3
    let b = -5 % 3
    let c = 5 % -3
    let d = -5 % -3
  "});
  assert_variable!(remainder; a, 2.0);
  assert_variable!(remainder; b, -2.0);
  assert_variable!(remainder; c, 2.0);
  assert_variable!(remainder; d, -2.0);

  let bidmas = run(indoc! {"
    let a = (5 - (3 - 1)) + -1
    let b = 5 + 3 * 2
  "});
  assert_variable!(bidmas; a, 2.0);
  assert_variable!(bidmas; b, 11.0);

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
  let numbers = run(indoc! {"
    let a = - 2
    let b = - 2.6
    let c = -4525
  "});
  assert_variable!(numbers; a, -2.0);
  assert_variable!(numbers; b, -2.6);
  assert_variable!(numbers; c, -4525.0);

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
fn equals() {
  let numbers = run(indoc! {"
    let a = 3 == 4
    let b = 3 == 3
    let c = -5 == 5
    let d = 0 == -0
    let e = 0.3 == (0.1 + 0.2)
    let f = (0 / 0) == maths::NAN
    let g = maths::NAN == maths::NAN
  "});
  assert_variable!(numbers; a, false);
  assert_variable!(numbers; b, true);
  assert_variable!(numbers; c, false);
  assert_variable!(numbers; d, true);
  assert_variable!(numbers; e, true);
  assert_variable!(numbers; f, false);
  assert_variable!(numbers; g, false);

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
    let a = v => _ => v

    let c = y == z
    let d = y == y
    let e = z == z
    let f = a == x
  "});
  assert_variable!(closures; c, false);
  assert_variable!(closures; d, true);
  assert_variable!(closures; e, true);
  assert_variable!(closures; f, false);
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
fn logical_operators() {
  let mut and = run(indoc! {"
    let a = false and true
    let b = false and false
    let c = true and true
    let d = true and false
    let e = true and 'Hello'
  "});
  assert_variable!(and; a, false);
  assert_variable!(and; b, false);
  assert_variable!(and; c, true);
  assert_variable!(and; d, false);
  assert_variable!(and; e, string "Hello");

  let or = run(indoc! {"
    let a = false or true
    let b = false or false
    let c = true or true
    let d = true or false
    let e = true or \"Hello\"
  "});
  assert_variable!(or; a, true);
  assert_variable!(or; b, false);
  assert_variable!(or; c, true);
  assert_variable!(or; d, true);
  assert_variable!(or; e, true);
}

#[test]
fn blocks() {
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

  let access_higher_scopes = run(indoc! {"
    let a = 5
    let b = {
      let c = 3
      a + c
    }
  "});
  assert_variable!(access_higher_scopes; a, 5.0);
  assert_variable!(access_higher_scopes; b, 8.0);

  let shadow_higher_scopes = run(indoc! {"
    let a = 5
    let b = {
      let a = 4
      let c = 3
      a + c
    }
  "});
  assert_variable!(shadow_higher_scopes; a, 5.0);
  assert_variable!(shadow_higher_scopes; b, 7.0);
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
  let basic_conditions = run(indoc! {"
    let a = if (true) 1 else 2
    let b = if (false) 3 else 4
    let c = if (3 < 4) 5 else 6
    let d = if (_ => _ => 7) 7 else 8
  "});
  assert_variable!(basic_conditions; a, 1.0);
  assert_variable!(basic_conditions; b, 4.0);
  assert_variable!(basic_conditions; c, 5.0);
  assert_variable!(basic_conditions; d, 7.0);

  let no_else = run(indoc! {"
    let a = if (false) 4
    let b = if (true) 1.1
  "});
  assert_variable!(no_else; a, ());
  assert_variable!(no_else; b, 1.1);
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

  let two_parameters = run(indoc! {"
    let add = a => b => a + b
    let x = add(2)(3)
    let y = add(3.5)(0.25)
  "});
  assert_variable!(two_parameters; x, 5.0);
  assert_variable!(two_parameters; y, 3.75);

  let two_parameters_curried = run(indoc! {"
    let add = a => b => a + b
    let a = add(2)
    let x = a(3)
    let b = 3.5 >> add
    let y = 0.25 >> b
  "});
  assert_variable!(two_parameters_curried; x, 5.0);
  assert_variable!(two_parameters_curried; y, 3.75);
}

#[test]
fn match_expression() {
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
    let d = match 4 | 4 -> 1 | 4 -> 2 | _ -> 3
  "});
  assert_variable!(literal_multi_case; a, 2.0);
  assert_variable!(literal_multi_case; b, 1.0);
  assert_variable!(literal_multi_case; c, 3.0);
  assert_variable!(literal_multi_case; d, 1.0);

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

  let temporaries_cleaned_up = run(indoc! {"
    let a = match false
      | true -> 1
      | false -> 2

    let b = {
      let x = 7
      x
    }

    let c = match 55
      | ..44 -> 1
      | 44.. -> 2

    let d = {
      let x = c
      x
    }

    let matchFunction = listy => {
      let a = 1
      let b = match listy
        | 44 -> 5
        | 900 if false -> 11
        | x if x -> 3
        | _ -> 9
      let c = 2
      a + b + c
    }
    let e = matchFunction(44)
    let f = matchFunction(1)
    let g = matchFunction(0)
    let h = matchFunction(900)
  "});
  assert_variable!(temporaries_cleaned_up; b, 7.0);
  assert_variable!(temporaries_cleaned_up; d, 2.0);
  assert_variable!(temporaries_cleaned_up; e, 8.0);
  assert_variable!(temporaries_cleaned_up; f, 6.0);
  assert_variable!(temporaries_cleaned_up; g, 12.0);
  assert_variable!(temporaries_cleaned_up; h, 6.0);

  let guards = run(indoc! {"
    let a = match true | true if false -> 1 | false -> 2 | _ -> 3
    let b = match true | true if true -> 1 | false -> 2 | _ -> 3
    let c = match 5 | ..6 if false -> 1 | 2..8 if true -> 2 | _ -> 3
    let d = match 5 | ..6 if true -> 1 | 2..8 if true -> 2 | _ -> 3
    let e = match 12 | c if c > 3 -> 1 | _ -> 2
    let f = match 12 | c if c > 15 -> 1 | c if c > 10 -> 2 | _ -> 3
    let g = match 12 | c if c > 10 -> c | _ -> 3
    let h = match 12 | c if false -> c | _ -> 3
  "});
  assert_variable!(guards; a, 3.0);
  assert_variable!(guards; b, 1.0);
  assert_variable!(guards; c, 2.0);
  assert_variable!(guards; d, 1.0);
  assert_variable!(guards; e, 1.0);
  assert_variable!(guards; f, 2.0);
  assert_variable!(guards; g, 12.0);
  assert_variable!(guards; h, 3.0);

  let mut list_patterns = run(indoc! {"
    // checks that basic lists match properly, non-lists are regected,
    // and that the stack is maintained properly
    let matchFunction = listy => {
      let a = 1
      let b = match listy
        | [] -> 3
        | [x] -> 5
        | [x, ..y] -> 7
        | _ -> 9

      let c = 2
      a + b + c
    }
    let a = matchFunction([])
    let b = matchFunction([1])
    let c = matchFunction([1, 2])
    let d = matchFunction([1, 2, 3, 4, 5, 6, 7, 8])
    let e = matchFunction(false)
    let f = matchFunction(-55)
    let g = matchFunction('a string')

    // rest pattern can match single item list
    let minimal = listy => match listy
      | [] -> 'empty'
      | [item, ..rest] -> `{item} {rest}`
    let h = minimal([])
    let i = minimal([1])
    let j = minimal([1, 2])

    // catch all rest pattern - conflicts with standard rest pattern
    let rest = listy => match listy
      | [..rest] -> string::from(rest)
    let k = rest([])
    let l = rest([1])
    let m = rest([1, 2])
    let n = rest(false)
  "});
  assert_variable!(list_patterns; a, 6.0);
  assert_variable!(list_patterns; b, 8.0);
  assert_variable!(list_patterns; c, 10.0);
  assert_variable!(list_patterns; d, 10.0);
  assert_variable!(list_patterns; e, 12.0);
  assert_variable!(list_patterns; f, 12.0);
  assert_variable!(list_patterns; g, 12.0);
  assert_variable!(list_patterns; h, string "empty");
  assert_variable!(list_patterns; i, string "1 []");
  assert_variable!(list_patterns; j, string "1 [2]");
  assert_variable!(list_patterns; k, string "[]");
  assert_variable!(list_patterns; l, string "[1]");
  assert_variable!(list_patterns; m, string "[1, 2]");
  assert_variable!(list_patterns; n, ());

  let list_pattern_guards = run(indoc! {"
    let func = x => y => {
      let a = 1
      let b = match x
        | [] if y -> 3
        | [x] if y -> 5
        | [x, ..xs] if y -> 7
        | [.._rest] if y -> 13
        | _ -> 9
      let c = 2
      a + b + c
    }
    let a = func([])(true)
    let b = func([])(false)
    let c = func([1])(true)
    let d = func([1])(false)
    let e = func([1, 2])(true)
    let f = func([1, 2, 3])(false)
    let g = func(1)(true)
    let h = func(1)(false)

    let func = x => {
      let a = 1
      let b = match x
        | [x, ..y] if false -> 2
        | _ -> 3
      let c = 5
      a + b + c
    }
    let i = func([])
    let j = func([1])
    let k = func(1)
  "});
  assert_variable!(list_pattern_guards; a, 6.0);
  assert_variable!(list_pattern_guards; b, 12.0);
  assert_variable!(list_pattern_guards; c, 8.0);
  assert_variable!(list_pattern_guards; d, 12.0);
  assert_variable!(list_pattern_guards; e, 10.0);
  assert_variable!(list_pattern_guards; f, 12.0);
  assert_variable!(list_pattern_guards; g, 12.0);
  assert_variable!(list_pattern_guards; h, 12.0);
  assert_variable!(list_pattern_guards; i, 9.0);
  assert_variable!(list_pattern_guards; j, 9.0);
  assert_variable!(list_pattern_guards; k, 9.0);

  let option_patterns = run(indoc! {"
    from option import { None, Some }

    /// matches options properly and clears up properly
    let matchFunction = x => {
      let a = 1
      let b = match x
        | Some(_) -> 5
        | None -> 10
        | _ -> 100
      let c = 2
      a + b + c
    }
    let a = matchFunction(None)
    let b = matchFunction(Some(5))
    let c = matchFunction(false)
    let d = matchFunction(-55)
    let e = matchFunction('a string')

    /// some variable is matched properly
    let matchFunction = x => {
      let a = 1
      let b = match x
        | Some(x) -> x
        | _ -> 100
      let c = 2
      a + b + c
    }
    let f = matchFunction(Some(0))
    let g = matchFunction(Some(1))
    let h = matchFunction(Some(7))
  "});
  assert_variable!(option_patterns; a, 13.0);
  assert_variable!(option_patterns; b, 8.0);
  assert_variable!(option_patterns; c, 103.0);
  assert_variable!(option_patterns; d, 103.0);
  assert_variable!(option_patterns; e, 103.0);
  assert_variable!(option_patterns; f, 3.0);
  assert_variable!(option_patterns; g, 4.0);
  assert_variable!(option_patterns; h, 10.0);

  let option_pattern_guard = run(indoc! {"
    from option import { None, Some }

    let matchFunction = x => {
      let a = 1
      let b = match x
        | Some(_) if false -> 5
        | None if false -> 10
        | _ -> 100
      let c = 2
      a + b + c
    }
    let a = matchFunction(None)
    let b = matchFunction(Some(5))
    let c = matchFunction(false)
    let d = matchFunction(-55)
  "});
  assert_variable!(option_pattern_guard; a, 103.0);
  assert_variable!(option_pattern_guard; b, 103.0);
  assert_variable!(option_pattern_guard; c, 103.0);
  assert_variable!(option_pattern_guard; d, 103.0);

  let fibonnacci = run(indoc! {"
    let fibonnacciMatch = n => match n
      | ..2 -> 1
      | n -> fibonnacciMatch(n - 1) + fibonnacciMatch(n - 2)
    let a = fibonnacciMatch(0)
    let b = fibonnacciMatch(6)
  "});
  assert_variable!(fibonnacci; a, 1.0);
  assert_variable!(fibonnacci; b, 8.0);

  let sum = run(indoc! {"
    let sum = list => match list
      | [] -> 0
      | [x, ..y] -> x + sum(y)
    let a = [] >> sum
    let b = [1] >> sum
    let c = [1, 2, 3] >> sum
    let d = [256, 128, 128, 1] >> sum
  "});
  assert_variable!(sum; a, 0.0);
  assert_variable!(sum; b, 1.0);
  assert_variable!(sum; c, 6.0);
  assert_variable!(sum; d, 513.0);

  let length = run(indoc! {"
    let length = list => match list
      | [] -> 0
      | [_, ..xs] -> 1 + length(xs)
    let a = [] >> length
    let b = [1] >> length
    let c = [1, 2, 3] >> length
    let d = [256, false, 128, 1, '', 4] >> length
  "});
  assert_variable!(length; a, 0.0);
  assert_variable!(length; b, 1.0);
  assert_variable!(length; c, 3.0);
  assert_variable!(length; d, 6.0);
}

#[test]
fn format_string() {
  let mut basic = run(indoc! {"
    let a = `hello {55}`
    let b = `there were {33 + 25} people`
    let c = `i can quote {true} or {false}`
    let d = `nested stuff: {{55} + 1 }`
    let e = `{`{55}`}`
  "});
  assert_variable!(basic; a, string "hello 55");
  assert_variable!(basic; b, string "there were 58 people");
  assert_variable!(basic; c, string "i can quote true or false");
  assert_variable!(basic; d, string "nested stuff: 56");
  assert_variable!(basic; e, string "55");

  let mut objects_display = run(indoc! {"
    let a = `{'hello'}`
    let b = `{55.2}`
    let c = `{false}`
    let d = `{true}`
    let e = `{print}`

    let identity = x => x
    let f = `{identity}`
    let g = `{x => x}`
    let h = `{(x => _ => x)()}`

    from maths import { pow }
    let i = `{pow(2)}`
  "});
  assert_variable!(objects_display; a, string "hello");
  assert_variable!(objects_display; b, string "55.2");
  assert_variable!(objects_display; c, string "false");
  assert_variable!(objects_display; d, string "true");
  assert_variable!(objects_display; e, string "<function print>");
  assert_variable!(objects_display; f, string "<function identity>");
  assert_variable!(objects_display; g, string "<function>");
  assert_variable!(objects_display; h, string "<closure <function>>");
  assert_variable!(objects_display; i, string "<closure <function pow>>");
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
fn imports() {
  let unknown_module = run("from unknown_goo_goo_gaa_gaa import { abs }");
  assert!(unknown_module.is_err());

  let unknown_item = run("from maths import { unknown_goo_goo_gaa_gaa }");
  assert!(unknown_item.is_err());

  let unknown_item = run("from maths import { abs, unknown_goo_goo_gaa_gaa }");
  assert!(unknown_item.is_err());

  let same_item_imported_but_renamed = run("from maths import { abs, abs as absolute }");
  assert!(same_item_imported_but_renamed.is_ok());

  let same_item_imported_but_renamed = run("from maths import { PI, PI as pi }");
  assert_variable!(same_item_imported_but_renamed; pi, std::f64::consts::PI);
  assert_variable!(same_item_imported_but_renamed; PI, std::f64::consts::PI);

  let unknown_module_access = run("unknown_module_aaa::abs");
  assert!(unknown_module_access.is_err());

  let unknown_item_access = run("maths::unknown_goo_goo_gaa_gaa");
  assert!(unknown_item_access.is_err());

  let same_item_imported_and_module_access = run(indoc! {"
    from maths import { PI as a }
    let b = maths::PI
  "});
  assert_variable!(same_item_imported_and_module_access; a, std::f64::consts::PI);
  assert_variable!(same_item_imported_and_module_access; b, std::f64::consts::PI);
}

#[test]
fn cant_import_anything_with_empty_context() {
  let ast = parse("from maths import { abs }".to_owned());
  assert!(ast.is_valid());
  let chunk = compile(&ast).unwrap();
  let mut vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();
  assert!(vm.run(&chunk).is_err());

  let ast = parse("maths::abs".to_owned());
  assert!(ast.is_valid());
  let chunk = compile(&ast).unwrap();
  let mut vm = VM::new(HeapSize::Small, &EmptyContext).unwrap();
  assert!(vm.run(&chunk).is_err());
}

#[test]
fn lists() {
  let mut empty_list = run(indoc! {"
    let a = type([])
    let b = if ([]) 1 else 2
    let c = string::from([])
    let d = [] == []
  "});
  assert_variable!(empty_list; a, string "list");
  assert_variable!(empty_list; b, 2.0);
  assert_variable!(empty_list; c, string "[]");
  assert_variable!(empty_list; d, true);

  let mut one_item = run(indoc! {"
    let a = type(['hello'])
    let b = if (['hello']) 1 else 2
    let c = string::from(['hello'])
    let d = ['hello'] == []
    let e = ['hello'] == ['hello']
  "});
  assert_variable!(one_item; a, string "list");
  assert_variable!(one_item; b, 1.0);
  assert_variable!(one_item; c, string "['hello']");
  assert_variable!(one_item; d, false);
  assert_variable!(one_item; e, true);

  let mut multiple_items = run(indoc! {"
    let a = type(['hello', 2, 5])
    let b = if (['hello', 2, 5]) 1 else 2
    let c = string::from(['hello', 2, 5])
    let d = ['hello', 2, 5] == []
    let e = ['hello', 2, 5] == ['hello']
    let f = ['hello', 2, 5] == ['hello', 2, 5]
  "});
  assert_variable!(multiple_items; a, string "list");
  assert_variable!(multiple_items; b, 1.0);
  assert_variable!(multiple_items; c, string "['hello', 2, 5]");
  assert_variable!(multiple_items; d, false);
  assert_variable!(multiple_items; e, false);
  assert_variable!(multiple_items; f, true);

  let mut nested = run(indoc! {"
    let stringListNested = ['hello', ['another', ['list']]]
    let a = string::from(stringListNested)
    let b = stringListNested != ['hello', 'another', 'list']

    let c = string::from([true, false, x => x, ])
  "});
  assert_variable!(nested; a, string "['hello', ['another', ['list']]]");
  assert_variable!(nested; b, true);
  assert_variable!(nested; c, string "[true, false, <function>]");
}
