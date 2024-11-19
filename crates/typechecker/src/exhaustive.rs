use crate::types::{PrimitiveType, Type};
use bang_syntax::{
  ast::expression::{Literal, LiteralValue, MatchArm, Pattern},
  Span, AST,
};
use std::{cmp, fmt};

/// Details what arms are unnecessary, or what arms are missing
#[derive(Debug)]
pub enum Result {
  MissingCases(Vec<MissingPattern>),
  UnreachableCases(Vec<Span>),
  Ok,
}

/// Details about that values are uncovered by match arm
#[derive(Debug, Clone)]
pub enum MissingPattern {
  Boolean(bool),
  Number(f64, f64),
  CatchAll,
}
impl fmt::Display for MissingPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MissingPattern::CatchAll => write!(f, "a catch all arm is missing"),
      MissingPattern::Boolean(x) => write!(f, "the arms don't cover `{x}`"),
      MissingPattern::Number(f64::NEG_INFINITY, end) => {
        write!(f, "the arms don't cover numbers less than `{end}`")
      }
      MissingPattern::Number(start, f64::INFINITY) => {
        write!(f, "the arms don't cover numbers greater than `{start}`")
      }
      MissingPattern::Number(start, end) => {
        write!(f, "the arms don't cover between `{start}` and `{end}`")
      }
    }
  }
}

/// Checks if a match expressions arms are exhaustive
pub fn check<'a>(
  value: &Type,
  arms: impl ExactSizeIterator<Item = &'a MatchArm>,
  ast: &AST,
) -> Result {
  match value {
    Type::Primitive(PrimitiveType::Boolean) => boolean(arms, ast),
    Type::Primitive(PrimitiveType::Number) => number(arms, ast),
    _ => has_catch_all(arms, ast),
  }
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

fn number<'a>(cases: impl Iterator<Item = &'a MatchArm>, ast: &AST) -> Result {
  let mut has_catch_all = false;
  let mut number_ranges = vec![];
  let mut unused_arms = vec![];

  for arm in cases {
    match &arm.pattern {
      _ if has_catch_all => unused_arms.push(arm.span(ast)),
      Pattern::Identifier(_) if arm.guard(ast).is_none() => has_catch_all = true,

      Pattern::Literal(literal) => match literal.value(ast) {
        LiteralValue::Number(number) => number_ranges.push(NumberRange::from(number)),
        LiteralValue::Boolean(_) | LiteralValue::String(_) => {}
      },
      Pattern::Range(start, end) => {
        if let Ok(range) = NumberRange::from_range_pattern(start.as_ref(), end.as_ref(), ast) {
          number_ranges.push(range);
        }
      }
      _ => {}
    }
  }

  if has_catch_all {
    return if unused_arms.is_empty() {
      Result::Ok
    } else {
      Result::UnreachableCases(unused_arms)
    };
  }

  number_ranges.sort_unstable();
  let mut current_number = f64::NEG_INFINITY;
  let mut missing_cases = vec![];

  for NumberRange(start, end) in number_ranges {
    if start > current_number {
      missing_cases.push(MissingPattern::Number(current_number, start));
    }

    if end > current_number {
      current_number = end;
    }
  }
  if current_number < f64::INFINITY {
    missing_cases.push(MissingPattern::Number(current_number, f64::INFINITY));
  }

  match (missing_cases.len(), unused_arms.is_empty()) {
    (0, true) => Result::Ok,
    (0, false) => Result::UnreachableCases(unused_arms),
    (missing_cases_count, _) if missing_cases_count <= 5 => Result::MissingCases(missing_cases),
    (_, _) => Result::MissingCases(vec![MissingPattern::CatchAll]),
  }
}

#[derive(Debug, PartialEq)]
struct NumberRange(f64, f64);
impl NumberRange {
  fn from_range_pattern(
    start: Option<&Literal>,
    end: Option<&Literal>,
    ast: &AST,
  ) -> std::result::Result<Self, ()> {
    let start = match start.map(|start| start.value(ast)) {
      Some(LiteralValue::Number(number)) => number,
      None => f64::NEG_INFINITY,
      Some(LiteralValue::Boolean(_) | LiteralValue::String(_)) => return Err(()),
    };
    let end = match end.map(|end| end.value(ast)) {
      Some(LiteralValue::Number(number)) => number,
      None => f64::INFINITY,
      Some(LiteralValue::Boolean(_) | LiteralValue::String(_)) => return Err(()),
    };

    Ok(Self(start, end))
  }
}
impl From<f64> for NumberRange {
  fn from(value: f64) -> Self {
    NumberRange(value, value)
  }
}
impl PartialOrd for NumberRange {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    Some(self.cmp(other))
  }
}
impl Ord for NumberRange {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    let order = self.0.total_cmp(&other.0);

    if order == cmp::Ordering::Equal {
      self.1.total_cmp(&other.1)
    } else {
      order
    }
  }
}
impl Eq for NumberRange {}
