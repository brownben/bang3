use super::{Primitive, Type};
use bang_parser::{
  ast::expression::{LiteralKind, MatchCase, Pattern},
  Span,
};
use std::fmt;

/// Checks if a match expressions arms are exhaustive
pub fn check(value: &Type, cases: &[MatchCase]) -> Result {
  match value {
    Type::Function(_) | Type::Unknown | Type::Existential(_) => has_catch_all(cases),
    Type::Primitive(primitive) => match primitive {
      Primitive::Boolean | Primitive::True | Primitive::False => boolean(cases),
      Primitive::Number | Primitive::String => {
        // TODO: handle all numbers/ range case coverage
        has_catch_all(cases)
      }
    },
  }
}

/// Simple check that
fn has_catch_all(cases: &[MatchCase]) -> Result {
  let position = cases
    .iter()
    .position(|case| matches!(case.pattern, Pattern::Identifier(_)));

  if let Some(position) = position
    && position == cases.len() - 1
  {
    Result::Ok
  } else if let Some(position) = position {
    Result::UnreachableCases(cases[position + 1..].iter().map(|case| case.span).collect())
  } else {
    Result::MissingCases(vec![MissingPattern::CatchAll])
  }
}

fn boolean(cases: &[MatchCase]) -> Result {
  let (mut has_true, mut has_false) = (false, false);
  let mut unused_cases = vec![];

  for case in cases {
    match &case.pattern {
      Pattern::Identifier(_) if !has_true || !has_false => {
        has_true = true;
        has_false = true;
      }
      Pattern::Literal(literal) => match literal.kind {
        LiteralKind::Boolean(true) if !has_true => has_true = true,
        LiteralKind::Boolean(false) if !has_false => has_false = true,
        _ => unused_cases.push(case.span),
      },
      _ => unused_cases.push(case.span),
    }
  }

  match (has_true, has_false) {
    (true, true) if unused_cases.is_empty() => Result::Ok,
    (true, true) => Result::UnreachableCases(unused_cases),
    (false, true) => Result::MissingCases(vec![MissingPattern::Boolean(true)]),
    (true, false) => Result::MissingCases(vec![MissingPattern::Boolean(false)]),
    (false, false) => unreachable!("must be single arm, or pattern will have errored"),
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
