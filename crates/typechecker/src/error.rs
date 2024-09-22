use bang_parser::{GetSpan, Span};
use std::{error, fmt};

/// A error found by the Typechecker
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Problem {
  /// A variable which was not defined was used
  UndefinedVariable {
    /// The identifier of the variable
    identifier: String,
    /// The location of the variable
    span: Span,
  },
  /// Expected a type, but recieved a different one
  ExpectedDifferentType {
    /// The type that was expected
    expected: String,
    /// The type that was recieved
    given: String,
    /// The location of the error
    span: Span,
  },
  /// A variable was defined, but was never used
  UnusedVariable {
    /// The variable which was defined, but never used
    identifier: String,
    /// The location of the variable
    span: Span,
  },
  /// Match statement arms are not exhaustive
  MissingPattern {
    /// A message describing what patterns are missing
    message: String,
    /// The location of the match statement
    span: Span,
  },
  /// Match statement arm is not reachable
  UnreachableCase {
    /// The location of the match statement arm
    span: Span,
  },
}
impl Problem {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::UndefinedVariable { .. } => "Undefined Variable",
      Self::ExpectedDifferentType { .. } => "Expected Different Type",
      Self::UnusedVariable { .. } => "Unused Variable",
      Self::MissingPattern { .. } => "Match Not Exhaustive",
      Self::UnreachableCase { .. } => "Unreachable Case",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    match self {
      Self::UndefinedVariable { identifier, .. } => {
        format!("undefined variable `{identifier}`")
      }
      Self::ExpectedDifferentType {
        expected, given, ..
      } => {
        format!("expected type `{expected}`, but recieved `{given}`")
      }
      Self::UnusedVariable { identifier, .. } => {
        format!("variable `{identifier}` is declared but never used. if this is intentional prefix with a underscore")
      }
      Self::MissingPattern { message, .. } => message.clone(),
      Self::UnreachableCase { .. } => "case is already covered, so is unreachable".to_string(),
    }
  }

  /// The title and message of the lint in a combined string
  #[must_use]
  pub fn full_message(&self) -> String {
    let mut message = self.title().to_owned();
    message.push('\n');
    message.push_str(&self.message());
    message
  }

  /// Is this an error or a warning?
  #[must_use]
  pub fn is_warning(&self) -> bool {
    match self {
      Self::UndefinedVariable { .. }
      | Self::ExpectedDifferentType { .. }
      | Self::MissingPattern { .. } => false,
      Self::UnusedVariable { .. } | Self::UnreachableCase { .. } => true,
    }
  }
}
impl GetSpan for Problem {
  fn span(&self) -> Span {
    match self {
      Self::UndefinedVariable { span, .. }
      | Self::ExpectedDifferentType { span, .. }
      | Self::UnusedVariable { span, .. }
      | Self::MissingPattern { span, .. }
      | Self::UnreachableCase { span } => *span,
    }
  }
}
impl fmt::Display for Problem {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for Problem {}
