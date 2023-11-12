use crate::{lint as lint_ast, parse, Allocator};

fn lint(source: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator).unwrap();
  let diagnostics = lint_ast(&ast);

  if diagnostics.is_empty() {
    Ok(())
  } else {
    Err(())
  }
}

#[test]
fn constant_conditions() {
  assert!(lint("if (true) pass").is_err());
  assert!(lint("if (4 > 5) pass").is_err());

  assert!(lint("if (x > 6) pass").is_ok());
}

#[test]
fn no_negative_zero() {
  assert!(lint("-0").is_err());
  assert!(lint("-0.0").is_err());
  assert!(lint("x == -0.0").is_err());

  assert!(lint("0").is_ok());
  assert!(lint("5 - 0").is_ok());
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

  assert!(lint("x == y").is_ok());
  assert!(lint("(variable) >= var").is_ok());
  assert!(lint("bla <= blabber // bla").is_ok());
  assert!(lint("a < b").is_ok());
  assert!(lint("a > d").is_ok());
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
