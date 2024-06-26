use crate::{lint as lint_ast, parse, Allocator};

fn lint(source: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);
  let diagnostics = lint_ast(&ast);

  if diagnostics.is_empty() {
    Ok(())
  } else {
    Err(())
  }
}

#[test]
fn constant_conditions() {
  assert!(lint("if (true) pass else other").is_err());
  assert!(lint("if (4 > 5) pass else other").is_err());
  assert!(lint("match false | _ -> false").is_err());

  assert!(lint("if (x > 6) pass else other").is_ok());
  assert!(lint("match x | 1 -> false | _ -> true").is_ok());
  assert!(lint("5 >> 4").is_ok());
}

#[test]
fn no_negative_zero() {
  assert!(lint("-0").is_err());
  assert!(lint("-0.0").is_err());
  assert!(lint("x == -0.0").is_err());

  assert!(lint("0").is_ok());
  assert!(lint("5 - 0").is_ok());
  assert!(lint("!0").is_ok());
  assert!(lint("function(-5)").is_ok());
}

#[test]
fn no_self_assign() {
  assert!(lint("let x = x").is_err());
  assert!(lint("let var = (var)").is_err());
  assert!(lint("let bla = bla // bla").is_err());
  assert!(lint("let a = { a }").is_err());

  assert!(lint("let x = 7").is_ok());
  assert!(lint("let y = x").is_ok());
}

#[test]
fn no_self_comparison() {
  assert!(lint("x == x").is_err());
  assert!(lint("var >= (var)").is_err());
  assert!(lint("bla <= bla // bla").is_err());
  assert!(lint("a < a").is_err());
  assert!(lint("a > a").is_err());
  assert!(lint("a > { a }").is_err());

  assert!(lint("x == y").is_ok());
  assert!(lint("(variable) >= var").is_ok());
  assert!(lint("bla <= blabber // bla").is_ok());
  assert!(lint("a < b").is_ok());
  assert!(lint("a > d").is_ok());
  assert!(lint("a > { d \n b }").is_ok());
}

#[test]
fn no_todo_comment() {
  assert!(lint("7 != x // TODO: stuff").is_err());
  assert!(lint("7 // TODO: stuff").is_err());
  assert!(lint("// TODO: jkfal").is_err());

  assert!(lint("// still todo").is_ok());
  assert!(lint("// ToDo: ").is_ok());
  assert!(lint("// Yet TODO: ").is_ok());
  assert!(lint("// TOD0: 2").is_ok());
}

#[test]
fn no_underscore_variable_use() {
  assert!(lint("match n | _ -> _").is_err());
  assert!(lint("let _ = 3\n_ + 2").is_err());

  assert!(lint("match n | 1 -> 2 | _ -> 3").is_ok());
  assert!(lint("let _ = 3").is_ok());
}

#[test]
fn no_useless_match() {
  assert!(lint("match n | _ -> 3").is_err());
  assert!(lint("match x\n | x -> 3").is_err());
  assert!(lint("match x\n | pattern -> 3").is_err());

  assert!(lint("match n | 3 -> 3").is_ok());
  assert!(lint("match n | true -> 3 | false -> 4").is_ok());
}

#[test]
fn no_yoda_comparison() {
  assert!(lint("7 != x").is_err());
  assert!(lint("7 == x").is_err());
  assert!(lint("(7) != x").is_err());
  assert!(lint("7 == (x)").is_err());

  assert!(lint("x == y").is_ok());
  assert!(lint("x == 7").is_ok());
  assert!(lint("x != y").is_ok());
  assert!(lint("x != 7").is_ok());
  assert!(lint("7 < x").is_ok());
  assert!(lint("x > 7").is_ok());
}
