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
  /// Expected a function, but recieved a non callable type
  NotCallable {
    /// The type that was recieved
    type_: String,
    /// The location of the error
    span: Span,
  },
  /// Expected function to be called with different argument
  IncorrectArgument {
    /// The type that was expected
    expected: String,
    /// The type that was recieved
    given: String,
    /// The location of the error
    span: Span,
  },
  /// Expected function to be called with an argument
  MissingArgument {
    /// The type that was expected
    expected: String,
    /// The location of the error
    span: Span,
  },
  /// Expected both arms to return the same type, but they don't
  BranchesDontMatch {
    /// The type of the first arm
    then: String,
    /// The type of the later arm
    otherwise: String,
    /// The location of the later arm
    span: Span,
  },
  /// Expected pattern to match value
  PatternNeverMatches {
    /// The type of the pattern
    pattern: String,
    /// The value of the match
    value: String,
    /// The location of the error
    span: Span,
  },
  /// Function tries to return a `never` value
  FunctionReturnsNever {
    /// The location of the error
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
      Self::NotCallable { .. } => "Type Not Callable",
      Self::IncorrectArgument { .. } => "Incorrect Argument",
      Self::MissingArgument { .. } => "Missing Argument",
      Self::BranchesDontMatch { .. } => "If Else Branch Type Mismatch",
      Self::PatternNeverMatches { .. } => "Pattern Doesn't Match Value",
      Self::FunctionReturnsNever { .. } => "Function Returns `Never`",
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
      Self::NotCallable { type_, .. } => {
        format!("expected a function, `{type_}` is not callable")
      }
      Self::IncorrectArgument {
        expected, given, ..
      } => {
        format!("expected function argument to be type `{expected}`, but recieved type `{given}`")
      }
      Self::MissingArgument { expected, .. } => {
        format!("expected to be called with type `{expected}`, but no argument was given")
      }
      Self::BranchesDontMatch {
        then, otherwise, ..
      } => {
        format!("expected both arms to have the same type. The first branch has type `{then}`, but the later branch has type `{otherwise}`")
      }
      Self::PatternNeverMatches { pattern, value, .. } => {
        format!("match value is `{value}` which doesn't match pattern of type `{pattern}``")
      }
      Self::FunctionReturnsNever { .. } => {
        "function returns `never` which indicates a lack of value, functions must return a value"
          .to_string()
      }
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
    matches!(
      self,
      Self::UnusedVariable { .. } | Self::UnreachableCase { .. }
    )
  }
}
impl GetSpan for Problem {
  fn span(&self) -> Span {
    match self {
      Self::UndefinedVariable { span, .. }
      | Self::ExpectedDifferentType { span, .. }
      | Self::UnusedVariable { span, .. }
      | Self::MissingPattern { span, .. }
      | Self::UnreachableCase { span }
      | Self::NotCallable { span, .. }
      | Self::IncorrectArgument { span, .. }
      | Self::MissingArgument { span, .. }
      | Self::BranchesDontMatch { span, .. }
      | Self::PatternNeverMatches { span, .. }
      | Self::FunctionReturnsNever { span } => *span,
    }
  }
}
impl fmt::Display for Problem {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for Problem {}
