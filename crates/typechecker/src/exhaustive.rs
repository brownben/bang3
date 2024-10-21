use super::{Type, TypeArena};
use bang_syntax::{
  ast::expression::{LiteralValue, MatchArm, Pattern},
  Span, AST,
};
use std::fmt;

/// Checks if a match expressions arms are exhaustive
pub fn check<'a>(
  value: &Type,
  arms: impl ExactSizeIterator<Item = &'a MatchArm>,
  ast: &AST,
) -> Result {
  if let Type::Primitive(TypeArena::BOOLEAN) = value {
    return boolean(arms, ast);
  }

  // TODO: handle all numbers/ range case coverage
  has_catch_all(arms, ast)
}

/// Simple check that
fn has_catch_all<'a>(mut arms: impl ExactSizeIterator<Item = &'a MatchArm>, ast: &AST) -> Result {
  let position = arms
    .position(|case| case.guard(ast).is_none() && matches!(case.pattern, Pattern::Identifier(_)));

  if position.is_some() && arms.len() == 0 {
    Result::Ok
  } else if position.is_some() {
    Result::UnreachableCases(arms.map(|arm| arm.span(ast)).collect())
  } else {
    Result::MissingCases(vec![MissingPattern::CatchAll])
  }
}

fn boolean<'a>(cases: impl Iterator<Item = &'a MatchArm>, ast: &AST) -> Result {
  let (mut has_true, mut has_false) = (false, false);
  let mut unused_arms = vec![];

  for arm in cases {
    match &arm.pattern {
      Pattern::Identifier(_) if !has_true || !has_false => {
        if arm.guard(ast).is_none() {
          has_true = true;
          has_false = true;
        }
      }
      Pattern::Literal(literal) => match literal.value(ast) {
        LiteralValue::Boolean(true) if !has_true && arm.guard(ast).is_none() => has_true = true,
        LiteralValue::Boolean(false) if !has_false && arm.guard(ast).is_none() => has_false = true,
        LiteralValue::Boolean(true) if !has_true => {}
        LiteralValue::Boolean(false) if !has_false => {}
        _ => unused_arms.push(arm.span(ast)),
      },
      _ => unused_arms.push(arm.span(ast)),
    }
  }

  match (has_true, has_false) {
    (true, true) if unused_arms.is_empty() => Result::Ok,
    (true, true) => Result::UnreachableCases(unused_arms),
    (false, true) => Result::MissingCases(vec![MissingPattern::Boolean(true)]),
    (true, false) => Result::MissingCases(vec![MissingPattern::Boolean(false)]),
    (false, false) => Result::MissingCases(vec![
      MissingPattern::Boolean(true),
      MissingPattern::Boolean(false),
    ]),
  }
}

/// Details about that values are uncovered by match arm
#[derive(Debug, Clone)]
pub enum MissingPattern {
  Boolean(bool),
  CatchAll,
}
impl fmt::Display for MissingPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MissingPattern::Boolean(true) => write!(f, "match doesn't cover `true` value"),
      MissingPattern::Boolean(false) => write!(f, "match doesn't cover `false` value"),
      MissingPattern::CatchAll => write!(f, "match doesn't have required catch all arm"),
    }
  }
}

/// Details what arms are unnecessary, or what arms are missing
#[derive(Debug)]
pub enum Result {
  MissingCases(Vec<MissingPattern>),
  UnreachableCases(Vec<Span>),
  Ok,
}
