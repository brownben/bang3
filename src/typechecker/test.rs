use super::{FunctionType, Primitive, Type, Typecheck, Typechecker};
use crate::{parse, typecheck, Allocator};
use indoc::indoc;

fn synthesize(source: &str) -> Type {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator).unwrap();
  let mut typechecker = Typechecker::new();
  ast.typecheck(&mut typechecker)
}

fn has_type_error(source: &str) -> bool {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator).unwrap();

  !typecheck(&ast).is_empty()
}

#[test]
fn binary_operators() {
  assert_eq!(synthesize("1 + 2"), Primitive::Number.into());
  assert_eq!(synthesize("1 - 2"), Primitive::Number.into());
  assert!(has_type_error("1 * false"));
  assert!(has_type_error("'' / false"));
  assert!(has_type_error("-false / false"));
  assert!(has_type_error("false / -false"));

  assert_eq!(synthesize("'str' ++ \"str\""), Primitive::String.into());
  assert!(has_type_error("7 ++ \"str\""));
}

#[test]
fn binary_comparators() {
  assert_eq!(synthesize("1 == 2"), Primitive::Boolean.into());
  assert_eq!(synthesize("1 != 2"), Primitive::Boolean.into());
  assert_eq!(synthesize("false != true"), Primitive::Boolean.into());
  assert!(has_type_error("1 == false"));

  assert_eq!(synthesize("1 < 2"), Primitive::Boolean.into());
  assert_eq!(synthesize("1 <= 2"), Primitive::Boolean.into());
  assert_eq!(synthesize("'string' > 'str'"), Primitive::Boolean.into());
  assert!(has_type_error("1 < false"));
  assert!(has_type_error("1 < 'str'"));
  assert!(has_type_error("'str' > false"));
}

#[test]
fn binary_logical_operators() {
  assert_eq!(synthesize("1 and 2"), Primitive::Number.into());
  assert_eq!(synthesize("false and true"), Primitive::False.into());
  assert_eq!(synthesize("false and 1"), Primitive::False.into());
  assert_eq!(synthesize("true and 1"), Primitive::Number.into());
  assert_eq!(synthesize("(_ => 0) and 1"), Primitive::Number.into());
  assert!(!has_type_error("x => x and 1"));
  assert!(has_type_error("1 and false"));

  assert_eq!(synthesize("1 or 2"), Primitive::Number.into());
  assert_eq!(synthesize("false or true"), Primitive::True.into());
  assert_eq!(synthesize("false or 1"), Primitive::Number.into());
  assert_eq!(synthesize("true or 1"), Primitive::True.into());
  assert!(!has_type_error("x => x or 1"));
  assert!(has_type_error("1 or false"));
}

#[test]
fn binary_pipeline() {
  let source = indoc! {"
    let addSeven = x => x + 7
    let multiplyByTwo = x => x * 2

    5 >> addSeven >> multiplyByTwo
  "};
  assert_eq!(synthesize(source), Primitive::Number.into());
}

#[test]
fn block() {
  assert_eq!(synthesize("{\n1\ntrue\n}"), Primitive::True.into());
}

#[test]
fn call() {
  let standard_function = indoc! {"
    let a = x => x + 7
    a(5)
  "};
  assert_eq!(synthesize(standard_function), Primitive::Number.into());

  let no_parameter_when_required = indoc! {"
    let a = x => x + 1
    a()
  "};
  assert!(has_type_error(no_parameter_when_required));

  let no_parameter_no_argument = indoc! {"
    let a = _ => 7
    a()
  "};
  assert_eq!(
    synthesize(no_parameter_no_argument),
    Primitive::Number.into()
  );

  let no_parameter_with_argument = indoc! {"
    let a = _ => 7
    a(55)
  "};
  assert_eq!(
    synthesize(no_parameter_with_argument),
    Primitive::Number.into()
  );

  assert!(has_type_error("let a = x => x + 1\na(-false)"));
  assert!(has_type_error("false()"));
  assert!(has_type_error("4.5()"));
  assert!(has_type_error("(-false)()"))
}

#[test]
fn functions() {
  assert_eq!(
    synthesize("a => a"),
    FunctionType {
      parameter: Type::Existential(0),
      return_type: Type::Existential(0)
    }
    .into()
  );

  let source = indoc! {"
    let a = x => x + 1
    let b = x => x + 2

    a == b
  "};
  assert_eq!(synthesize(source), Primitive::Boolean.into());

  let source = indoc! {"
    let a = x => x + 1
    let b = x => x ++ '\n'

    a == b
  "};
  assert!(has_type_error(source));
}

#[test]
fn groups_and_comments() {
  assert_eq!(synthesize("(true)"), Primitive::True.into());
  assert_eq!(synthesize("(false)"), Primitive::False.into());
  assert_eq!(synthesize("0 // comment"), Primitive::Number.into());
}

#[test]
fn if_() {
  assert_eq!(
    synthesize("if (1 > 4) false else true"),
    Primitive::Boolean.into()
  );

  assert!(has_type_error("if (1) false else 7"));
  assert!(has_type_error("if (1) 5 else 'str'"));
  assert!(has_type_error("if (-false) 5 else 7"));
  assert!(has_type_error("if (false) -false else 7"));
  assert!(has_type_error("if (false) false else -true"));
}

#[test]
fn literals() {
  assert_eq!(synthesize("true").to_string(), "true");
  assert_eq!(synthesize("false"), Primitive::False.into());

  assert_eq!(synthesize("0"), Primitive::Number.into());
  assert_eq!(synthesize("0.02"), Primitive::Number.into());
  assert_eq!(synthesize("2"), Primitive::Number.into());
  assert_eq!(synthesize("2.4"), Primitive::Number.into());
  assert_eq!(synthesize("2455.12"), Primitive::Number.into());

  assert_eq!(synthesize("\"hello\""), Primitive::String.into());
  assert_eq!(synthesize("'world'"), Primitive::String.into());
}

#[test]
fn match_() {
  let source = "match 1 + 5 | 1 -> true | _ -> false";
  assert_eq!(synthesize(source), Primitive::Boolean.into());

  let source = "match 2 > 4 | true -> 1 | false -> 3";
  assert_eq!(synthesize(source), Primitive::Number.into());

  let source = "match 1 | true -> true | _ -> false";
  assert!(has_type_error(source));

  let source = "match 1 ++ 5 | 1 -> true | 2 -> false";
  assert!(has_type_error(source));

  let source = "match 2 | ..1 -> true | 1.. -> false";
  assert_eq!(synthesize(source), Primitive::Boolean.into());

  let source = "match 2 | ..0 -> 2 | 0..1 -> 1 | 1.. -> 0";
  assert_eq!(synthesize(source), Primitive::Number.into());

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
  assert_eq!(synthesize(source), Primitive::Number.into());

  let source = "match 4 > 5 | true -> 1 | _ -> 3";
  assert_eq!(synthesize(source), Primitive::Number.into());

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
  assert_eq!(
    synthesize(function_needs_catch_all),
    Primitive::Number.into()
  );

  let extra_catch_all = "match 4 > 5 | _ -> 5 | true -> 1 | false -> 3";
  assert!(has_type_error(extra_catch_all));
}

#[test]
fn unary() {
  assert_eq!(synthesize("!true"), Primitive::False.into());
  assert_eq!(synthesize("!false").to_string(), "true");
  assert_eq!(synthesize("!'string'"), Primitive::Boolean.into());
  assert_eq!(synthesize("!0"), Primitive::Boolean.into());

  assert_eq!(synthesize("-0"), Primitive::Number.into());
  assert_eq!(synthesize("-2.4"), Primitive::Number.into());
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
  assert_eq!(synthesize(source), Primitive::Number.into());

  let source = indoc! {"
    let a = 1
    let b = a
    b
  "};
  assert_eq!(synthesize(source), Primitive::Number.into());

  let source = indoc! {"
    let a = 'string'
    let b = {
      let a = 1
      // comment
      a
    }
    b
  "};
  assert_eq!(synthesize(source), Primitive::Number.into());

  assert!(has_type_error("unknownVariable"));
  assert!(has_type_error("let a = 7\nunknownVariable"));
}

#[test]
fn recursive() {
  let fibonacci_match = indoc! {"
    let fibonacciMatch = n => match n
      | ..2 -> 1
      | n -> fibonacciMatch(n - 1) + fibonacciMatch(n - 2)

    fibonacciMatch
  "};
  assert_eq!(
    synthesize(fibonacci_match),
    FunctionType {
      parameter: Primitive::Number.into(),
      return_type: Primitive::Number.into()
    }
    .into()
  );

  let fibonacci_if = indoc! {"
    let fibonacciIf = n => if (n < 2) { 1 } else {
      fibonacciIf(n - 1) + fibonacciIf(n - 2)
    }

    fibonacciIf
  "};
  assert_eq!(
    synthesize(fibonacci_if),
    FunctionType {
      parameter: Primitive::Number.into(),
      return_type: Primitive::Number.into()
    }
    .into()
  );
}
