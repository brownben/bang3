//! # Linter Tests
//!
//! Check that the linter finds problems where it is supposed to.

use bang_linter::lint as lint_ast;
use bang_syntax::parse;

fn lint(source: &str) -> Result<(), ()> {
  let diagnostics = lint_ast(&parse(source));

  if diagnostics.is_empty() {
    Ok(())
  } else {
    Err(())
  }
}

#[test]
fn constant_condition() {
  assert!(lint("if (true) pass else other").is_err());
  assert!(lint("if (4 > 5) pass else other").is_err());
  assert!(lint("match false | _ -> false").is_err());
  assert!(lint("match x | 1 if false -> false | _ -> true").is_err());

  assert!(lint("if (x > 6) pass else other").is_ok());
  assert!(lint("match x | 1 -> false | _ -> true").is_ok());
  assert!(lint("match x | 1 if a > 4 -> false | _ -> true").is_ok());
  assert!(lint("5 >> 4").is_ok());
}

#[test]
fn negative_zero() {
  assert!(lint("-0").is_err());
  assert!(lint("-0.0").is_err());
  assert!(lint("x == -0.0").is_err());

  assert!(lint("0").is_ok());
  assert!(lint("5 - 0").is_ok());
  assert!(lint("!0").is_ok());
  assert!(lint("function(-5)").is_ok());
}

#[test]
fn self_assign() {
  assert!(lint("let x = x\n x").is_err());
  assert!(lint("let var = (var)\n var").is_err());
  assert!(lint("let bla = bla // bla\n bla").is_err());
  assert!(lint("let a = { a }\na").is_err());

  assert!(lint("let x = 7\n x").is_ok());
  assert!(lint("let y = x\n y").is_ok());
}

#[test]
fn self_comparison() {
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
fn underscore_variable_use() {
  assert!(lint("match n | _ -> _").is_err());
  assert!(lint("let _ = 3\n_ + 2").is_err());

  assert!(lint("match n | 1 -> 2 | _ -> 3").is_ok());
  assert!(lint("let _ = 3").is_ok());
}

#[test]
fn useless_match() {
  assert!(lint("match n | _ -> 3").is_err());
  assert!(lint("match x\n | x -> 3").is_err());
  assert!(lint("match x\n | pattern -> 3").is_err());
  assert!(lint("match n | _ if x -> 3 | _ -> 4").is_ok());

  assert!(lint("match n | 3 -> 3").is_ok());
  assert!(lint("match n | true -> 3 | false -> 4").is_ok());
}

#[test]
fn yoda_equality() {
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

#[test]
fn unnecessary_closures() {
  assert!(lint("x => y(x)").is_err());
  assert!(lint("x => { y(x) }").is_err());
  assert!(lint("x => y((x))").is_err());
  assert!(lint("add => y(add)").is_err());
  assert!(lint("add => another(add)").is_err());
  assert!(lint("_ => another()").is_err());

  assert!(lint("x => y(x + 1)").is_ok());
  assert!(lint("x => y(x) + 1").is_ok());
  assert!(lint("a => another() + a").is_ok());
}

#[test]
fn identical_branches() {
  assert!(lint("if (a) pass else pass").is_err());
  assert!(lint("if (b) 5 else { 5 }").is_err());

  assert!(lint("if (c) pass else other").is_ok());
}

#[test]
fn erasing_operations() {
  assert!(lint("5 * 0").is_err());
  assert!(lint("0 * 555").is_err());
  assert!(lint("0 / a").is_err());
  assert!(lint("{ 0 } * 555").is_err());
  assert!(lint("4 * (0)").is_err());

  assert!(lint("4 * 5").is_ok());
  assert!(lint("4 / 5").is_ok());
  assert!(lint("1 / 0").is_ok());
  assert!(lint("1 / ''").is_ok());
  assert!(lint("1 * (1 - 1)").is_ok());
}

#[test]
fn constant_string_in_format_string() {
  assert!(lint("`hello {'world'}`").is_err());
  assert!(lint("`{'world'} hello`").is_err());
  assert!(lint("`{'world'}`").is_err());
  assert!(lint("`hello {'world'}!`").is_err());

  assert!(lint("hello {5}!").is_ok());
  assert!(lint("hello {'x' ++ a}!").is_ok());
}

#[test]
fn unnecessary_return() {
  assert!(lint("_ => { { return 5 } }").is_err());
  assert!(lint("_ => { return 5 }").is_err());
  assert!(lint("_ => { 5 }").is_ok());

  assert!(lint("_ => ({ return 5 })").is_err());
  assert!(lint("_ => ({ 5 })").is_ok());

  assert!(lint("_ => if (a) { return 4 } else { return 8 }").is_err());
  assert!(lint("_ => if (a) { 4 } else { return 8 }").is_err());
  assert!(lint("_ => if (a) { return 4 } else { 8 }").is_err());
  assert!(lint("_ => if (a) { 4 } else { 8 }").is_ok());

  assert!(lint("_ => { return 5\n// some comment  }").is_err());
  assert!(lint("_ => { 5\n// some comment  }").is_ok());

  assert!(lint("_ => match a | 1 -> { return 5 } | 2 -> 3").is_err());
  assert!(lint("_ => match a | 1 -> { 5 } | 2 -> 3").is_ok());

  assert!(lint("_ => if (a) { return 5 }").is_ok());

  assert!(lint("_ => ({ return 5 })").is_err());
  assert!(lint("_ => ({ 5 })").is_ok());
}

#[test]
fn unreachable_code() {
  assert!(lint(
    "
    let function = _ => {
      if (a) {
        return 4

        4
      } else 4
    }
    "
  )
  .is_err());

  assert!(lint(
    "
    let function = _ => {
      if (a) {
        return 4

        4
      } else 4
    }
    "
  )
  .is_err());

  assert!(lint(
    "
    let function = _ => {
      if (a) { return 4 } else { return 4 }
      5
    }
    "
  )
  .is_err());
}

#[test]
fn double_comparison_chain() {
  assert!(lint("a == b or a > b").is_err());
  assert!(lint("a == b || a < b").is_err());
  assert!(lint("a < b || a == b").is_err());
  assert!(lint("a > b or a == b").is_err());

  assert!(lint("a < b and a == b").is_ok());

  assert!(lint("a <= b").is_ok());
  assert!(lint("a >= b").is_ok());
}

#[test]
fn empty_import() {
  assert!(lint("from maths import {}").is_err());
  assert!(lint("from string import {   }").is_err());
  assert!(lint("from string import { ,,, }").is_err());

  assert!(lint("from string import { length }").is_ok());
}

#[test]
fn loss_of_precision() {
  assert!(lint("1234567890123456789").is_err());
  assert!(lint("9007199254740993").is_err());
  assert!(lint("5123000000000000000000000000001").is_err());
  assert!(lint("0.1234567890123456789").is_err());
  assert!(lint(".1234567890123456789").is_err());

  assert!(lint("0").is_ok());
  assert!(lint("0.0").is_ok());
  assert!(lint("1.0").is_ok());
  assert!(lint("12345").is_ok());
  assert!(lint("123.456").is_ok());
  assert!(lint("3.14159").is_ok());
  assert!(lint("12300000000000000000000000").is_ok());
  assert!(lint("9007199254740991").is_ok());
  assert!(lint("9007_1992547409_91").is_ok());

  // various leading and trailing zeros, to ensure number cleaning works properly
  assert!(lint("0").is_ok());
  assert!(lint(".0").is_ok());
  assert!(lint("0.0").is_ok());
  assert!(lint("000.0").is_ok());
  assert!(lint("000.000").is_ok());
  assert!(lint("0.000").is_ok());
  assert!(lint("0.01400").is_ok());
  assert!(lint("0.14").is_ok());
  assert!(lint(".14").is_ok());
  assert!(lint("003.14").is_ok());
  assert!(lint("003.140000").is_ok());
  assert!(lint("0001_000").is_ok());
  assert!(lint("0001_000.000").is_ok());
  assert!(lint("1_000.000_000").is_ok());
  assert!(lint("000_1_000.000_000").is_ok());
}
