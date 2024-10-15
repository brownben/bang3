use crate::{typecheck, Typechecker};

use bang_parser::{parse, Allocator};
use indoc::indoc;

fn synthesize(source: &str) -> String {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);

  let mut checker = Typechecker::new();
  let result = checker.check_ast(&ast);
  assert!(checker.problems.is_empty());

  let generalized_type = checker.types.generalize(result, 0).type_;
  checker.types.type_to_string(generalized_type)
}

fn synthesize_has_error(source: &str) -> String {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);

  let mut checker = Typechecker::new();
  let result = checker.check_ast(&ast);

  let generalized_type = checker.types.generalize(result, 0).type_;
  checker.types.type_to_string(generalized_type)
}

fn has_type_error(source: &str) -> bool {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);

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
fn match_exhaustiveness() {
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
  let arms_after_catch_all = "match 5 | _ -> 1 | 1 -> 3 | 2 -> 5";
  assert!(has_type_error(arms_after_catch_all));

  let function_needs_catch_all = "match (_ => 7) | _ -> 3";
  assert_eq!(synthesize(function_needs_catch_all), "number");

  let extra_catch_all = "match 4 > 5 | _ -> 5 | true -> 1 | false -> 3";
  assert!(has_type_error(extra_catch_all));

  let needs_catch_all = "match 555 | 1 -> 1";
  assert!(has_type_error(needs_catch_all));
}

#[test]
fn match_exhaustiveness_with_guard() {
  let missing_possible_true = "match 4 > 5 | true if 3 > 4 -> 1 | false -> 3";
  assert!(has_type_error(missing_possible_true));

  let missing_possible_false = "match 4 > 5 | true -> 1 | false if 3 > 4 -> 3";
  assert!(has_type_error(missing_possible_false));

  let with_true_catch_guard = "match 4 > 5 | true if 3 > 4 -> 1 | false -> 3 | _ -> 5";
  assert_eq!(synthesize(with_true_catch_guard), "number");

  let needs_catch_all = "match 555 | _ if 4 > 5 -> 1";
  assert!(has_type_error(needs_catch_all));
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
  assert_eq!(synthesize_has_error("_ => -false"), "a' => number");

  assert_eq!(
    synthesize_has_error("let a = _ => -false\na"),
    "a' => number"
  );
}

#[test]
fn infer_function() {
  assert_eq!(synthesize("a => b => a + b"), "number => number => number");
  assert_eq!(
    synthesize("a => b => if (a) b else b + 1"),
    "a' => number => number"
  );
  assert_eq!(
    synthesize("a => b => c => if (a) b else c"),
    "a' => d' => d' => d'"
  );
  assert_eq!(
    synthesize("let x = (a => b => c => if (a) b() else c + 1)\nx"),
    "a' => (never => number) => number => number"
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
