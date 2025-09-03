use crate::{
  TypeError,
  types::{PrimitiveType, Structure, Type},
};
use bang_syntax::{
  AST, Span,
  ast::expression::{Literal, LiteralValue, MatchArm, Pattern},
};
use std::{cmp, collections::BTreeSet, fmt};

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
    Type::Structure(Structure::Option, _) => option(arms, ast, match_span),
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
    Self(value, value)
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
  let mut unused_arms = vec![];

  let mut min_length = usize::MAX;
  let mut lengths_covered = BTreeSet::new();

  for arm in cases {
    match &arm.pattern {
      Pattern::Identifier(_) if min_length > 0 => {
        if arm.guard(ast).is_none() {
          min_length = 0;
        }
      }
      Pattern::List(list) => match (list.variables(ast).len(), list.rest(ast)) {
        (num, None) if !lengths_covered.contains(&num) && num < min_length => {
          if arm.guard(ast).is_none() {
            lengths_covered.insert(num);
          }
        }
        (num, Some(_)) if num < min_length => {
          if arm.guard(ast).is_none() {
            min_length = num;
          }
        }
        _ => unused_arms.push(arm.span(ast)),
      },
      _ => unused_arms.push(arm.span(ast)),
    }
  }

  let mut missing_cases = Vec::new();
  if min_length == usize::MAX {
    missing_cases.push(MissingListPattern::Infinite.into_error(match_span));
  } else {
    for size in 0..min_length {
      if !lengths_covered.contains(&size) {
        missing_cases.push(MissingListPattern::FixedSize(size).into_error(match_span));
      }
    }
  }

  if unused_arms.is_empty() && missing_cases.is_empty() {
    Vec::new()
  } else if missing_cases.is_empty() {
    unused_arms_errors(unused_arms)
  } else {
    missing_cases
  }
}
#[derive(Debug, Clone)]
pub enum MissingListPattern {
  FixedSize(usize),
  Infinite,
}
impl fmt::Display for MissingListPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::FixedSize(0) => {
        write!(f, "the arms don't cover an empty list")
      }
      Self::FixedSize(size) => {
        write!(f, "the arms don't cover a list with {size} elements")
      }
      Self::Infinite => {
        write!(f, "the arms don't cover an infinite list")
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

fn option<'a>(
  cases: impl Iterator<Item = &'a MatchArm>,
  ast: &AST,
  match_span: Span,
) -> Vec<TypeError> {
  let (mut has_some, mut has_none) = (false, false);
  let mut unused_arms = vec![];

  for arm in cases {
    match &arm.pattern {
      Pattern::Identifier(_) if !has_some || !has_none => {
        if arm.guard(ast).is_none() {
          has_some = true;
          has_none = true;
        }
      }
      Pattern::Option(option) => match option.kind {
        None if !has_none && arm.guard(ast).is_none() => has_none = true,
        None if !has_none => {}
        Some(()) if !has_some && arm.guard(ast).is_none() => has_some = true,
        Some(()) if !has_some => {}
        _ => unused_arms.push(arm.span(ast)),
      },
      _ => unused_arms.push(arm.span(ast)),
    }
  }

  match (has_some, has_none) {
    (true, true) if unused_arms.is_empty() => Vec::new(),
    (true, true) => unused_arms_errors(unused_arms),
    (false, true) => missing_cases_errors([MissingOptionPattern(Some(()))], match_span),
    (true, false) => missing_cases_errors([MissingOptionPattern(None)], match_span),
    (false, false) => missing_cases_errors(
      [MissingOptionPattern(Some(())), MissingOptionPattern(None)],
      match_span,
    ),
  }
}
#[derive(Debug, Clone)]
pub struct MissingOptionPattern(Option<()>);
impl fmt::Display for MissingOptionPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0 {
      Some(()) => write!(f, "the arms don't cover `Some`"),
      None => write!(f, "the arms don't cover `None`"),
    }
  }
}
