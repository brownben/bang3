use bang_syntax::Span;
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
    /// Suggestion for which variable could be used instead
    did_you_mean: Option<String>,
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
  /// A variable was imported, but was never used
  UnusedImport {
    /// The variable which was imported, but never used
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
  /// Extra argument
  ExtraArgument {
    /// The type that was recieved
    given: String,
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
  /// Module not found
  ModuleNotFound {
    /// The name of the module which was not found
    module: String,
    /// The location of the error
    span: Span,
    /// Suggestion for which module could be meant instead
    did_you_mean: Option<String>,
  },
  /// Item not found in module
  ItemNotFound {
    /// The name of the module
    module: String,
    /// The name of the item which was not found
    item: String,
    /// The location of the error
    span: Span,
    /// Suggestion for which item could be meant instead
    did_you_mean: Option<String>,
  },
  /// Use of unknown type annotation
  UnknownTypeAnnotation {
    /// The location of the error
    span: Span,
    /// Suggestion for which type could be meant instead
    did_you_mean: Option<String>,
  },
  /// Type doesn't accept a parameter
  UnexpectedParameter {
    /// The type that was recieved
    type_: String,
    /// The location of the error
    span: Span,
  },
  /// No return from match guard
  NoReturnFromMatchGuard {
    /// The location of the error
    span: Span,
  },
  /// Module Access to access imported item
  ModuleAccessAlreadyImported {
    /// The path which is being imported e.g. `module::item`
    path: String,
    /// What it is imported as
    defined_as: String,
    /// The location of the existing import
    definition_location: Span,
    /// The location of the error
    span: Span,
  },
  /// Functions and iterators only have referential equality not structural equality
  /// This can give unexpected results
  ReferentialEquality {
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
      Self::UnusedImport { .. } => "Unused Import",
      Self::MissingPattern { .. } => "Match Not Exhaustive",
      Self::UnreachableCase { .. } => "Unreachable Case",
      Self::NotCallable { .. } => "Type Not Callable",
      Self::IncorrectArgument { .. } => "Incorrect Argument",
      Self::MissingArgument { .. } => "Missing Argument",
      Self::ExtraArgument { .. } => "Extra Argument",
      Self::BranchesDontMatch { .. } => "If Else Branch Type Mismatch",
      Self::PatternNeverMatches { .. } => "Pattern Doesn't Match Value",
      Self::FunctionReturnsNever { .. } => "Function Returns `Never`",
      Self::ModuleNotFound { .. } => "Module Not Found",
      Self::ItemNotFound { .. } => "Item Not Found",
      Self::UnknownTypeAnnotation { .. } => "Unknown Type Annotation",
      Self::UnexpectedParameter { .. } => "Unexpected Parameter",
      Self::NoReturnFromMatchGuard { .. } => "No Return from Match Guard",
      Self::ModuleAccessAlreadyImported { .. } => "Module Access Item Already Imported",
      Self::ReferentialEquality { .. } => "Referential Equality",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    match self {
      Self::UndefinedVariable { identifier, .. } => {
        format!("no variable defined with the name `{identifier}`")
      }
      Self::ExpectedDifferentType {
        expected, given, ..
      } => {
        format!("expected type `{expected}`, but recieved `{given}`")
      }
      Self::UnusedVariable { identifier, .. } => {
        format!("variable `{identifier}` is declared but never used")
      }
      Self::UnusedImport { identifier, .. } => format!("`{identifier}` is imported but never used"),
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
      Self::ExtraArgument { given, .. } => {
        format!("function does not take any arguments, but recieved type `{given}`")
      }
      Self::BranchesDontMatch {
        then, otherwise, ..
      } => {
        format!(
          "expected both arms to have the same type. The first branch has type `{then}`, but the later branch has type `{otherwise}`"
        )
      }
      Self::PatternNeverMatches { pattern, value, .. } => {
        format!("match value is `{value}` which doesn't match pattern of type `{pattern}``")
      }
      Self::FunctionReturnsNever { .. } => {
        "function returns `never` which indicates a lack of value, functions must return a value"
          .to_owned()
      }
      Self::ModuleNotFound { module, .. } => {
        format!("could not find module `{module}`")
      }
      Self::ItemNotFound { module, item, .. } => {
        format!("could not find `{item}` in `{module}`")
      }
      Self::UnknownTypeAnnotation { .. } => "type annotation is not a valid type".to_owned(),
      Self::UnexpectedParameter { type_, .. } => {
        format!("type `{type_}` does not accept a parameter")
      }
      Self::NoReturnFromMatchGuard { .. } => {
        "early returns from match guards can cause unexpected execution behaviour".to_owned()
      }
      Self::ModuleAccessAlreadyImported {
        path, defined_as, ..
      } => {
        format!("`{path}` has been imported as `{defined_as}`")
      }
      Self::ReferentialEquality { .. } => {
        "functions and iterators only have referential equality defined\nthis can give unexpected behaviour".to_owned()
      }
    }
  }

  /// A suggestion for how to fix the error
  #[must_use]
  pub fn suggestion(&self) -> Option<String> {
    match self {
      Self::UnusedVariable { .. } => {
        Some("if this is intentional prefix with a underscore".to_owned())
      }
      Self::UndefinedVariable {
        did_you_mean: Some(did_you_mean),
        ..
      } => Some(format!(
        "a variable with a similar name exists, did you mean `{did_you_mean}`?",
      )),
      Self::ModuleNotFound {
        did_you_mean: Some(did_you_mean),
        ..
      } => Some(format!(
        "a module with a similar name exists, did you mean `{did_you_mean}`?",
      )),
      Self::UnknownTypeAnnotation {
        did_you_mean: Some(did_you_mean),
        ..
      } => Some(format!(
        "a type with a similar name exists, did you mean `{did_you_mean}`?",
      )),
      Self::ItemNotFound {
        module,
        did_you_mean: Some(did_you_mean),
        ..
      } => Some(format!(
        "an item with a similar name exists in `{module}`, did you mean `{did_you_mean}`?",
      )),
      _ => None,
    }
  }

  /// The title and message of the lint in a combined string
  #[must_use]
  pub fn full_message(&self) -> String {
    let mut message = self.title().to_owned();
    message.push('\n');
    message.push_str(&self.message());

    if let Some(suggestion) = self.suggestion() {
      message.push('\n');
      message.push_str("hint: ");
      message.push_str(&suggestion);
    }

    message
  }

  /// Is this an error or a warning?
  #[must_use]
  pub fn is_warning(&self) -> bool {
    matches!(
      self,
      Self::UnusedVariable { .. }
        | Self::UnusedImport { .. }
        | Self::UnreachableCase { .. }
        | Self::ModuleAccessAlreadyImported { .. }
        | Self::ReferentialEquality { .. }
    )
  }

  /// Does this error indicate unused code?
  #[must_use]
  pub fn is_unused(&self) -> bool {
    matches!(
      self,
      Self::UnusedVariable { .. } | Self::UnusedImport { .. } | Self::UnreachableCase { .. }
    )
  }

  /// Related information which can be displayed for the error
  #[must_use]
  pub fn related_info(&self) -> Option<(Span, String)> {
    match self {
      Self::ModuleAccessAlreadyImported {
        defined_as,
        definition_location: span,
        ..
      } => Some((*span, format!("`{defined_as}` is imported here"))),
      _ => None,
    }
  }

  /// The location of the error in the source code
  pub fn span(&self) -> Span {
    match self {
      Self::UndefinedVariable { span, .. }
      | Self::ExpectedDifferentType { span, .. }
      | Self::UnusedVariable { span, .. }
      | Self::UnusedImport { span, .. }
      | Self::MissingPattern { span, .. }
      | Self::UnreachableCase { span }
      | Self::NotCallable { span, .. }
      | Self::IncorrectArgument { span, .. }
      | Self::MissingArgument { span, .. }
      | Self::ExtraArgument { span, .. }
      | Self::BranchesDontMatch { span, .. }
      | Self::PatternNeverMatches { span, .. }
      | Self::FunctionReturnsNever { span }
      | Self::ModuleNotFound { span, .. }
      | Self::ItemNotFound { span, .. }
      | Self::UnknownTypeAnnotation { span, .. }
      | Self::UnexpectedParameter { span, .. }
      | Self::NoReturnFromMatchGuard { span, .. }
      | Self::ModuleAccessAlreadyImported { span, .. }
      | Self::ReferentialEquality { span, .. } => *span,
    }
  }
}
impl fmt::Display for Problem {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for Problem {}
