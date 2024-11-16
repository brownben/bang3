use crate::{typecheck, TypeChecker};

use bang_syntax::parse;
use indoc::indoc;

fn synthesize(source: &str) -> String {
  let ast = parse(source);
  assert!(ast.errors.is_empty());

  let mut checker = TypeChecker::new();
  let result = checker.check_ast(&ast);
  assert!(checker.problems.is_empty());

  let generalized_type = checker.types.generalize(result).type_;
  checker.types.type_to_string(generalized_type)
}

fn synthesize_has_error(source: &str) -> String {
  let ast = parse(source);
  assert!(ast.errors.is_empty());

  let mut checker = TypeChecker::new();
  let result = checker.check_ast(&ast);
  assert!(!checker.problems.is_empty());

  let generalized_type = checker.types.generalize(result).type_;
  checker.types.type_to_string(generalized_type)
}

fn has_type_error(source: &str) -> bool {
  let ast = parse(source);
  assert!(ast.errors.is_empty());

  !typecheck(&ast).is_empty()
}

#[test]
fn binary_operators() {
  assert_eq!(synthesize("1 + 2"), "number");
  assert_eq!(synthesize("1 - 2"), "number");
  assert!(has_type_error("1 * false"));
  assert!(has_type_error("'' / false"));
  assert!(has_type_error("-false / false"));
  assert!(has_type_error("false / -false"));

  assert_eq!(synthesize("'str' ++ \"str\""), "string");
  assert!(has_type_error("7 ++ \"str\""));
}

#[test]
fn binary_comparators() {
  assert_eq!(synthesize("1 == 2"), "boolean");
  assert_eq!(synthesize("1 != 2"), "boolean");
  assert_eq!(synthesize("false != true"), "boolean");
  assert!(has_type_error("1 == false"));

  assert_eq!(synthesize("1 < 2"), "boolean");
  assert_eq!(synthesize("1 <= 2"), "boolean");
  assert_eq!(synthesize("'string' > 'str'"), "boolean");
  assert!(has_type_error("1 < false"));
  assert!(has_type_error("1 < 'str'"));
  assert!(has_type_error("'str' > false"));
}

#[test]
fn binary_logical_operators() {
  assert_eq!(synthesize("1 and 2"), "number");
  assert_eq!(synthesize("false and true"), "boolean");
  assert_eq!(synthesize("1 or 2"), "number");
  assert_eq!(synthesize("false or true"), "boolean");
  assert_eq!(synthesize("x => x or 1"), "number => number");

  assert!(has_type_error("1 and false"));
  assert!(has_type_error("1 or false"));
}

#[test]
fn binary_pipeline() {
  let source = indoc! {"
    let addSeven = x => x + 7
    let multiplyByTwo = x => x * 2

    5 >> addSeven >> multiplyByTwo
  "};
  assert_eq!(synthesize(source), "number");

  let incorrect_arg = indoc! {"
    let addSeven = x => x + 7
    let multiplyByTwo = x => x * 2

    'hello' >> addSeven >> multiplyByTwo
  "};
  assert!(has_type_error(incorrect_arg));

  let not_callable = "4 >> 5";
  assert!(has_type_error(not_callable));
}

#[test]
fn block() {
  assert_eq!(synthesize("{\n1\ntrue\n}"), "boolean");
  assert_eq!(synthesize("{\n1\ntrue\n//hello\n}"), "boolean");
  assert_eq!(synthesize("{\nlet a = 5\ntrue\na\n}"), "number");
}

#[test]
fn call() {
  let standard_function = indoc! {"
    let a = x => x + 7
    a(5)
  "};
  assert_eq!(synthesize(standard_function), "number");

  let no_parameter_when_required = indoc! {"
    let a = x => x + 1
    a()
  "};
  assert!(has_type_error(no_parameter_when_required));

  let no_parameter_no_argument = indoc! {"
    let a = _ => 7
    a()
  "};
  assert_eq!(synthesize(no_parameter_no_argument), "number");

  let no_parameter_with_argument = indoc! {"
    let a = _ => 7
    a(55)
  "};
  assert_eq!(synthesize(no_parameter_with_argument), "number");

  assert!(has_type_error("false()"));
  assert!(has_type_error("4.5()"));
  assert!(has_type_error("(-false)()"));

  let incorrect_argument = indoc! {"
    let a = x => x + 1
    a(false)
  "};
  assert!(has_type_error(incorrect_argument));
}

#[test]
fn functions() {
  let source = indoc! {"
    let a = x => x + 1
    let b = x => x + 2

    a == b
  "};
  assert_eq!(synthesize(source), "boolean");

  let source = indoc! {"
    let a = x => x + 1
    let b = x => x ++ '\n'

    a == b
  "};
  assert!(has_type_error(source));
}

#[test]
fn groups_and_comments() {
  assert_eq!(synthesize("(true)"), "boolean");
  assert_eq!(synthesize("(false)"), "boolean");
  assert_eq!(synthesize("0 // comment"), "number");
}

#[test]
fn if_() {
  assert_eq!(synthesize("if (1 > 4) false else true"), "boolean");

  assert!(has_type_error("if (1) false else 7"));
  assert!(has_type_error("if (1) 5 else 'str'"));
  assert!(has_type_error("if (-false) 5 else 7"));
  assert!(has_type_error("if (false) -false else 7"));
  assert!(has_type_error("if (false) false else -true"));
  assert!(has_type_error("if (true) { 5 } else { false }"));
  assert!(has_type_error("a => if (a) { 5 } else { false }"));
}

#[test]
fn literals() {
  assert_eq!(synthesize("true"), "boolean");
  assert_eq!(synthesize("false"), "boolean");

  assert_eq!(synthesize("0"), "number");
  assert_eq!(synthesize("0.02"), "number");
  assert_eq!(synthesize("2"), "number");
  assert_eq!(synthesize("2.4"), "number");
  assert_eq!(synthesize("2455.12"), "number");

  assert_eq!(synthesize("\"hello\""), "string");
  assert_eq!(synthesize("'world'"), "string");
}

#[test]
fn match_() {
  let source = "match 1 + 5 | 1 -> true | _ -> false";
  assert_eq!(synthesize(source), "boolean");

  let source = "match 2 > 4 | true -> 1 | false -> 3";
  assert_eq!(synthesize(source), "number");

  let source = "match 1 | true -> true | _ -> false";
  assert!(has_type_error(source));

  let source = "match 1 ++ 5 | 1 -> true | 2 -> false";
  assert!(has_type_error(source));

  let source = "match 2 | ..1 -> true | 1.. -> false | _ -> false";
  assert_eq!(synthesize(source), "boolean");

  let source = "match 2 | ..0 -> 2 | 0..1 -> 1 | 1.. -> 0 | _ -> 5";
  assert_eq!(synthesize(source), "number");

  let source = "match 'string' | ..0 -> 2 | 0..1 -> 1 | 1.. -> 0";
  assert!(has_type_error(source));

  let source = "match 5 | ..0 -> 2 | 'str'..1 -> 1 | 1.. -> 0";
  assert!(has_type_error(source));

  let source = "match 5 | ..0 -> 2 | 0..'str' -> 1 | 1.. -> 0";
  assert!(has_type_error(source));

  let source = "match 5 | ..0 -> 2 | 0..4 -> -false | 4.. -> 0";
  assert!(has_type_error(source));

  let source = "match 5 | ..0 -> -false | 0..4 -> -5 | 4.. -> 0";
  assert!(has_type_error(source));

  let source = "match 5 | ..0 -> false | 0.. -> 5";
  assert!(has_type_error(source));
}

#[test]
fn unary() {
  assert_eq!(synthesize("!true"), "boolean");
  assert_eq!(synthesize("!false"), "boolean");
  assert_eq!(synthesize("!'string'"), "boolean");
  assert_eq!(synthesize("!0"), "boolean");

  assert_eq!(synthesize("-0"), "number");
  assert_eq!(synthesize("-2.4"), "number");
  assert!(has_type_error("-true"));
  assert!(has_type_error("-'hello'"));
  assert!(has_type_error("!-'hello'"));
}

#[test]
fn variables() {
  let source = indoc! {"
    let a = 1
    a
  "};
  assert_eq!(synthesize(source), "number");

  let source = indoc! {"
    let a = 1
    let b = a
    b
  "};
  assert_eq!(synthesize(source), "number");

  let source = indoc! {"
    let a = 'string'
    let b = {
      let a = 1
      // comment
      a
    }
    a
    b
  "};
  assert_eq!(synthesize(source), "number");

  assert!(has_type_error("unknownVariable"));
  assert!(has_type_error("let a = 7\nunknownVariable"));
}

#[test]
fn recursive() {
  let fibonacci_match = indoc! {"
    let fibonacciMatch = n => match n
      | ..2 -> 1
      | n -> fibonacciMatch(n - 1) + fibonacciMatch(n - 2)

    fibonacciMatch(4)
  "};
  assert_eq!(synthesize(fibonacci_match), "number");

  let fibonacci_if = indoc! {"
    let fibonacciIf = n => if (n < 2) { 1 } else {
      fibonacciIf(n - 1) + fibonacciIf(n - 2)
    }

    fibonacciIf(4)
  "};
  assert_eq!(synthesize(fibonacci_if), "number");
}

#[test]
fn no_unused_variables() {
  assert!(has_type_error("let a = 5"));
  assert!(!has_type_error("let _a = 5"));
  assert!(!has_type_error("let a = 5\na"));

  let source = indoc! {"
    let a = 5
    {
      let b = a
      b + 5
    }"
  };
  assert!(!has_type_error(source));

  let source = indoc! {"
    let a = 5
    {
      let b = 4
      b + 5
    }"
  };
  assert!(has_type_error(source));

  let source = indoc! {"
    let _a = 5
    {
      let a = 4
      a + 5
    }"
  };
  assert!(!has_type_error(source));
}

#[test]
fn identity_function() {
  let source = indoc! {"
    let identity = x => x

    identity(5) == 5
  "};
  assert_eq!(synthesize(source), "boolean");

  let source = indoc! {"
    let identity = x => x

    let a = identity(5)
    let b = identity(false)

    a == b
  "};
  assert!(has_type_error(source));

  let source = indoc! {"
    let identity = x => x

    let _a = identity(5)

    identity(false)
  "};
  assert_eq!(synthesize(source), "boolean");
}

#[test]
fn builtin_functions() {
  let source = indoc! {"
    let identity = x => x
    let emptyString = _ => ''

    identity == print
    emptyString == type
  "};
  assert_eq!(synthesize(source), "boolean");
}

#[test]
fn empty_ast() {
  assert_eq!(synthesize(""), "never");
}

#[test]
fn expression_has_error() {
  assert_eq!(synthesize_has_error("let a = -false\na"), "number");
  assert_eq!(synthesize_has_error("_ => -false"), "^a => number");

  assert_eq!(
    synthesize_has_error("let a = _ => -false\na"),
    "^a => number"
  );
}

#[test]
fn infer_function() {
  assert_eq!(synthesize("a => b => a + b"), "number => number => number");
  assert_eq!(
    synthesize("a => b => if (a) b else b + 1"),
    "^a => number => number"
  );
  assert_eq!(
    synthesize("a => b => c => if (a) b else c"),
    "^a => ^b => ^b => ^b"
  );
  assert_eq!(
    synthesize("let x = (a => b => c => if (a) b() else c + 1)\nx"),
    "^a => (_ => number) => number => number"
  );
  assert_eq!(
    synthesize("let x = (a => b => c => if (a == 1) b(a) else c ++ '')\nx"),
    "number => (number => string) => string => string"
  );
}

#[test]
fn format_string() {
  let different_fields = "`{5} {false} {'string'}`";
  assert_eq!(synthesize(different_fields), "string");

  assert!(has_type_error("`{5 + false}`"));
}

#[test]
fn return_statement() {
  let simple_return = "a => { return a + 1 }";
  assert_eq!(synthesize(simple_return), "number => number");

  let returns_in_if = "a => if (a == true) { return 5 } else { return 7 }";
  assert_eq!(synthesize(returns_in_if), "boolean => number");

  let early_return = "a => if (a == true) { return 5 } else 7";
  assert_eq!(synthesize(early_return), "boolean => number");

  // returns don't match in an if
  assert!(has_type_error(
    "a => if (a == true) { return 5 } else { return false }"
  ));
  assert!(has_type_error(
    "a => if (a == true) { return '' } else { return false }"
  ));
  assert!(has_type_error(
    "a => if (a == true) { '' } else { return false }"
  ));
  assert!(has_type_error(
    "a => if (a == true) { return '' } else false"
  ));
}

#[test]
fn early_returns() {
  let source = indoc! {"
    let function = a => {
      let _a = if (a == true) {
        return 5
      } else {
        false
      }
      4
    }
    function
  "};
  assert_eq!(synthesize(source), "boolean => number");

  let source = indoc! {"
    let function = a => {
      let _a = if (a == true) {
        true
      } else {
        return 22
      }
      4
    }
    function
  "};
  assert_eq!(synthesize(source), "boolean => number");

  let source = indoc! {"
    let function = a => {
      let _a = if (a == true) {
        return 5
      } else {
        return 4
      }

      false
    }
  "};
  assert!(has_type_error(source));

  let source = indoc! {"
    let function = a => {
      let _a = if (a == true) {
        return 5
      } else {
        return 4
      }

      a
    }
  "};
  assert!(has_type_error(source));

  let source = indoc! {"
    let function = a => {
      let x = match a
        | true -> { return false }
        | false -> 4
      x == 4
    }
    function
  "};
  assert_eq!(synthesize(source), "boolean => boolean");
}

#[test]
fn if_no_else() {
  let with_early_return = indoc! {"
    let function = a => {
      if (a == true) { return 5 }

      4
    }
    function
  "};
  assert_eq!(synthesize(with_early_return), "boolean => number");

  let assign_to_variable = "let a = if (a) 5";
  assert!(has_type_error(assign_to_variable));

  let as_return = "_ => if (a) 5";
  assert!(has_type_error(as_return));

  let with_possible_side_effect = indoc! {"
    let function = a => {
      if (a == false) print(a)

      4
    }
    function
  "};
  assert_eq!(synthesize(with_possible_side_effect), "boolean => number");
}

#[test]
fn unknown_imports() {
  assert!(has_type_error(
    "from unknown_goo_goo import { unknown_aaa }"
  ));
  assert!(has_type_error("from maths import { unknown_aaa }"));
  assert!(has_type_error("from maths import { abs, unknown_aaa }"));

  assert!(has_type_error(
    "{from unknown_goo import { unknown_aaa }\n 5}"
  ));

  assert_eq!(
    synthesize_has_error("from unknown_goo import { unknown_aaa }\n unknown_aaa"),
    "unknown"
  );

  // unknown module acccess
  assert!(has_type_error("maths::unknown_aaa"));
  assert!(has_type_error("unknown_module__aaa::unknown_aaa"));
}

#[test]
fn annotations() {
  assert!(!has_type_error("let _x: number = 5"));
  assert!(has_type_error("let _x: number = 'string'"));
  assert!(has_type_error("let _x: number = false"));

  assert!(!has_type_error("let _x: number => number = x => x + 1"));
  assert!(has_type_error("let _x: number => string = x => x + 1"));
  assert!(has_type_error("let _x: string => number = x => x + 1"));

  assert!(!has_type_error(indoc! {"
    let identity = x => x
    let _x: number => number = identity
    let _y: string => string = identity
    let _z: ^s => ^s = identity
  "}));
  assert!(has_type_error(indoc! {"
    let identity: number => number = x => x
    let _y: string => string = identity
  "}));

  assert!(!has_type_error(indoc! {"
    let _x: ^a => ^b => ^b => ^b = a => b => c => if (a) b else c
    let _y: ^a => (_ => ^b) => ^b => ^b = a => b => c => if (a) b() else c
  "}));

  assert!(has_type_error(indoc! {"
    let addOne: number => number = x => x + 1
    let _y: ^a => ^a = addOne
    let _z: string => string = _y
  "}));
  assert!(has_type_error(indoc! {"
    let addOne: number => number = x => x + 1
    let _y: ^a => ^a = addOne
  "}));

  // Unknown Type Annotation
  assert!(has_type_error("let _x: unknown = 5"));
}

mod exhaustive {
  use super::*;

  #[test]
  fn boolean() {
    let source = "match 4 > 5 | true -> 1 | false -> 3";
    assert_eq!(synthesize(source), "number");

    let source = "match 4 > 5 | true -> 1 | _ -> 3";
    assert_eq!(synthesize(source), "number");

    let no_false = "match 4 > 5 | true -> 1";
    assert!(has_type_error(no_false));
    let no_true = "match 4 > 5 | false -> 1";
    assert!(has_type_error(no_true));
    let no_true_or_false = "match 4 > 5 | 1 -> 2 | 4 -> 7";
    assert!(has_type_error(no_true_or_false));

    let extra_catch_all = "match 4 > 5 | true -> 1 | false -> 3 | _ -> 5";
    assert!(has_type_error(extra_catch_all));
    let arms_after_catch_all = "match 4 > 5 | _ -> 1 | false -> 3 | true -> 5";
    assert!(has_type_error(arms_after_catch_all));
  }

  #[test]
  fn function() {
    let function_needs_catch_all = "match (_ => 7) | _ -> 3";
    assert_eq!(synthesize(function_needs_catch_all), "number");

    let function_doesnt_match_number = "match (_ => 7) | 7 -> 7 ";
    assert!(has_type_error(function_doesnt_match_number));

    let arms_after_catch_all = "match (_ => 7) | _ -> 1 | a -> 3";
    assert!(has_type_error(arms_after_catch_all));
  }

  #[test]
  fn string() {
    let string_with_catch_all = "match 'hello' | _ -> 'world'";
    assert_eq!(synthesize(string_with_catch_all), "string");

    let string_doesnt_match_number = "match 'hello' | 7 -> 'world' ";
    assert!(has_type_error(string_doesnt_match_number));

    let arms_after_catch_all = "match 'hello' | a -> 'world' | b -> 'earth'";
    assert!(has_type_error(arms_after_catch_all));
  }

  #[test]
  fn number() {
    let number_covered = "match 5 | 0 -> 'zero' | ..0 -> 'negative' | 0.. -> 'positive'";
    assert_eq!(synthesize(number_covered), "string");

    let missing_start = "match 5 | -100..0 -> 'zero' | 0.. -> 'positive'";
    assert!(has_type_error(missing_start));

    let missing_end = "match 5 | 0 -> 'zero' | ..0 -> 'negative' | 0..1000 -> 'positive'";
    assert!(has_type_error(missing_end));

    let missing_some = "match 5 | 0 -> 'zero' | 1 -> 'one'";
    assert!(has_type_error(missing_some));
    let missing_many =
      "match 5 | 0 -> 'zero' | 1 -> 'one' | 2 -> 'two' | 3 -> 'three' | 4 -> 'four'";
    assert!(has_type_error(missing_many));

    let missing_some_with_catch_all = "match 5 | 0 -> 'zero' | 1 -> 'one' | _ -> 'many'";
    assert_eq!(synthesize(missing_some_with_catch_all), "string");
    let missing_many =
      "match 5 | 0 -> 'zero' | 1 -> 'one' | 2 -> 'two' | 3 -> 'three' | 4 -> 'four' | _ -> 'many'";
    assert_eq!(synthesize(missing_many), "string");

    let arms_after_catch_all = "match 4 | _ -> 1 | 1 -> 3 | 2 -> 5";
    assert!(has_type_error(arms_after_catch_all));
  }

  #[test]
  fn with_guard() {
    let missing_possible_true = "match 4 > 5 | true if 3 > 4 -> 1 | false -> 3";
    assert!(has_type_error(missing_possible_true));

    let missing_possible_false = "match 4 > 5 | true -> 1 | false if 3 > 4 -> 3";
    assert!(has_type_error(missing_possible_false));

    let with_true_catch_guard = "match 4 > 5 | true if 3 > 4 -> 1 | false -> 3 | _ -> 5";
    assert_eq!(synthesize(with_true_catch_guard), "number");

    let needs_catch_all = "match 'stuff' | _ if 4 > 5 -> 1";
    assert!(has_type_error(needs_catch_all));
  }
}

mod stdlib {
  use super::*;

  #[test]
  fn module_access() {
    assert_eq!(synthesize("string::NEW_LINE"), "string");
    assert_eq!(synthesize("maths::pow"), "number => number => number");
  }

  #[test]
  fn maths() {
    let source = indoc! {"
      from maths import { PI, E, INFINITY }
      PI + E + INFINITY
    "};
    assert_eq!(synthesize(source), "number");

    let source = indoc! {"
      from maths import { abs, sin, cos, PI }

      let x = sin(PI) + cos(PI)
      abs(x / 44)
    "};
    assert_eq!(synthesize(source), "number");

    let source = indoc! {"
      from maths import { pow }

      let x = pow(2)(3)
      let y = pow(2)

      (4 >> y) + x
    "};
    assert_eq!(synthesize(source), "number");
  }

  #[test]
  fn string() {
    let source = indoc! {"
      from string import { NEW_LINE, TAB, CARRIAGE_RETURN }
      NEW_LINE ++ TAB ++ CARRIAGE_RETURN
    "};
    assert_eq!(synthesize(source), "string");

    let source = indoc! {"
      from string import { length }
      'this is a string' >> length >> x => x * 2
    "};
    assert_eq!(synthesize(source), "number");

    let source = indoc! {"
      from string import { isEmpty }
      'this is a string' >> isEmpty
    "};
    assert_eq!(synthesize(source), "boolean");

    let source = indoc! {"
      from string import { toUppercase }
      toUppercase('this is a string')
    "};
    assert_eq!(synthesize(source), "string");

    let source = indoc! {"
      from string import { contains }
      'this is a string' >> contains('is')
    "};
    assert_eq!(synthesize(source), "boolean");
  }
}
