use crate::{
  TypeError,
  types::{PrimitiveType, Structure, Type},
};
use bang_syntax::{
  AST, Span,
  ast::expression::{Literal, LiteralValue, MatchArm, Pattern},
};
use std::{cmp, fmt};

fn unused_arms_errors(unused_arms: impl IntoIterator<Item = Span>) -> Vec<TypeError> {
  unused_arms
    .into_iter()
    .map(|span| TypeError::UnreachableCase { span })
    .collect()
}
fn missing_cases_errors(
  missing_cases: impl IntoIterator<Item = impl ToString>,
  match_span: Span,
) -> Vec<TypeError> {
  missing_cases
    .into_iter()
    .map(|case| TypeError::MissingPattern {
      message: case.to_string(),
      span: match_span,
    })
    .collect()
}

/// Checks if a match expressions arms are exhaustive
pub fn check<'a>(
  value: &Type,
  arms: impl ExactSizeIterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
  match value {
    Type::Primitive(PrimitiveType::Boolean) => boolean(arms, ast, match_span),
    Type::Primitive(PrimitiveType::Number) => number(arms, ast, match_span),
    Type::Primitive(PrimitiveType::Unknown) => Vec::new(),
    Type::Structure(Structure::List, _) => list(arms, ast, match_span),
    _ => has_catch_all(arms, ast, match_span),
  }
}

/// Simple check that
fn has_catch_all<'a>(
  mut arms: impl ExactSizeIterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
  let position = arms
    .position(|case| case.guard(ast).is_none() && matches!(case.pattern, Pattern::Identifier(_)));

  if position.is_some() && arms.len() == 0 {
    Vec::new()
  } else if position.is_some() {
    unused_arms_errors(arms.map(|arm| arm.span(ast)))
  } else {
    missing_cases_errors([MissingCatchAllPattern], match_span)
  }
}
#[derive(Debug, Clone)]
pub struct MissingCatchAllPattern;
impl fmt::Display for MissingCatchAllPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "a catch all arm is missing")
  }
}

fn boolean<'a>(
  cases: impl Iterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
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
    (true, true) if unused_arms.is_empty() => Vec::new(),
    (true, true) => unused_arms_errors(unused_arms),
    (false, true) => missing_cases_errors([MissingBooleanPattern(true)], match_span),
    (true, false) => missing_cases_errors([MissingBooleanPattern(false)], match_span),
    (false, false) => missing_cases_errors(
      [MissingBooleanPattern(true), MissingBooleanPattern(false)],
      match_span,
    ),
  }
}
#[derive(Debug, Clone)]
pub struct MissingBooleanPattern(bool);
impl fmt::Display for MissingBooleanPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "the arms don't cover `{}`", self.0)
  }
}

fn number<'a>(
  cases: impl Iterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
  let mut has_catch_all = false;
  let mut number_ranges = vec![];
  let mut unused_arms = vec![];

  for arm in cases {
    match &arm.pattern {
      _ if has_catch_all => unused_arms.push(arm.span(ast)),
      Pattern::Identifier(_) if arm.guard(ast).is_none() => has_catch_all = true,

      Pattern::Literal(literal) => match literal.value(ast) {
        LiteralValue::Number(number) if arm.guard(ast).is_none() => {
          number_ranges.push(NumberRange::from(number));
        }
        LiteralValue::Number(_) | LiteralValue::Boolean(_) | LiteralValue::String(_) => {}
      },
      Pattern::Range(range) if arm.guard(ast).is_none() => {
        if let Ok(range) =
          NumberRange::from_range_pattern(range.start.as_ref(), range.end.as_ref(), ast)
        {
          number_ranges.push(range);
        }
      }
      _ => {}
    }
  }

  if has_catch_all {
    return if unused_arms.is_empty() {
      Vec::new()
    } else {
      unused_arms_errors(unused_arms)
    };
  }

  number_ranges.sort_unstable();
  let mut current_number = f64::NEG_INFINITY;
  let mut missing_cases = vec![];

  for NumberRange(start, end) in number_ranges {
    if start > current_number {
      missing_cases.push(NumberRange(current_number, start));
    }

    if end > current_number {
      current_number = end;
    }
  }
  if current_number < f64::INFINITY {
    missing_cases.push(NumberRange(current_number, f64::INFINITY));
  }

  match (missing_cases.is_empty(), unused_arms.is_empty()) {
    (true, true) => Vec::new(),
    (true, false) => unused_arms_errors(unused_arms),
    (false, _) => missing_cases_errors(missing_cases, match_span),
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
impl fmt::Display for NumberRange {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self(f64::NEG_INFINITY, end) => {
        write!(f, "the arms don't cover numbers less than `{end}`")
      }
      Self(start, f64::INFINITY) => {
        write!(f, "the arms don't cover numbers greater than `{start}`")
      }
      Self(start, end) => {
        write!(f, "the arms don't cover between `{start}` and `{end}`")
      }
    }
  }
}

fn list<'a>(
  cases: impl Iterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
  let (mut empty, mut one, mut infinite) = (false, false, false);
  let mut unused_arms = vec![];

  for arm in cases {
    match &arm.pattern {
      Pattern::Identifier(_) if !empty || !infinite || !one => {
        if arm.guard(ast).is_none() {
          empty = true;
          one = true;
          infinite = true;
        }
      }
      Pattern::List(list) => match (list.first(ast), list.rest(ast)) {
        (None, None) if !empty => {
          if arm.guard(ast).is_none() {
            empty = true;
          }
        }
        (None, Some(_)) if !empty || !infinite || !one => {
          if arm.guard(ast).is_none() {
            empty = true;
            one = true;
            infinite = true;
          }
        }
        (Some(_), None) if !one => {
          if arm.guard(ast).is_none() {
            one = true;
          }
        }
        (Some(_), Some(_)) if !infinite => {
          if arm.guard(ast).is_none() {
            one = true;
            infinite = true;
          }
        }
        _ => unused_arms.push(arm.span(ast)),
      },
      _ => unused_arms.push(arm.span(ast)),
    }
  }

  if empty && one && infinite {
    return if unused_arms.is_empty() {
      Vec::new()
    } else {
      unused_arms_errors(unused_arms)
    };
  }

  let mut missing_cases = Vec::new();

  if !empty {
    missing_cases.push(MissingListPattern::Empty.into_error(match_span));
  }
  if one && !infinite {
    missing_cases.push(MissingListPattern::LongerThanOne.into_error(match_span));
  } else if !infinite {
    missing_cases.push(MissingListPattern::NonEmpty.into_error(match_span));
  }

  missing_cases
}

#[derive(Debug, Clone)]
pub enum MissingListPattern {
  Empty,
  LongerThanOne,
  NonEmpty,
}
impl fmt::Display for MissingListPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Empty => {
        write!(f, "the arms don't cover an empty list")
      }
      Self::NonEmpty => {
        write!(f, "the arms don't cover a list with elements")
      }
      Self::LongerThanOne => {
        write!(f, "the arms don't cover a list longer than one item")
      }
    }
  }
}
impl MissingListPattern {
  fn into_error(self, match_span: Span) -> TypeError {
    TypeError::MissingPattern {
      message: self.to_string(),
      span: match_span,
    }
  }
}
