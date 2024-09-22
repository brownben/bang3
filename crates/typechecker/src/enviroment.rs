//! Tracks variables, their types, and where they are defined and used

use crate::{TypeRef, TypeScheme};
use bang_parser::{ast::expression, GetSpan, Span};

/// Holds variables and where they are defined and used
#[derive(Debug)]
pub struct Enviroment {
  variables: Vec<Variable>,
  finished_variables: Vec<Variable>,
  depth: u32,
}
impl Enviroment {
  pub(crate) fn new() -> Self {
    Self {
      variables: Vec::new(),
      finished_variables: Vec::new(),
      depth: 0,
    }
  }

  pub(crate) fn enter_scope(&mut self) {
    self.depth += 1;
  }
  pub(crate) fn exit_scope(&mut self, end_span: Span) {
    while let Some(Variable { depth, .. }) = self.variables.last()
      && *depth == self.depth
    {
      let mut finished_var = self.variables.pop().unwrap();
      finished_var.active = finished_var.active.merge(end_span);
      self.finished_variables.push(finished_var);
    }

    self.depth -= 1;
  }

  pub(crate) fn define_variable(&mut self, variable: &expression::Variable, type_: TypeScheme) {
    self.variables.push(Variable {
      name: variable.name.to_owned(),
      defined: variable.span(),
      used: Vec::new(),
      active: variable.span(),

      depth: self.depth,
      type_,
    });
  }

  pub(crate) fn update_variable(&mut self, identifier: &str, type_: TypeScheme) {
    let variable = self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name == identifier)
      .unwrap();

    variable.type_ = type_;
  }

  pub(crate) fn get_variable(&mut self, identifier: &str) -> Option<TypeScheme> {
    self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name == identifier)
      .map(|variable| variable.type_)
  }

  pub(crate) fn mark_variable_use(&mut self, identifier: &str, span: Span) {
    let mut variable = self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name == identifier);

    if let Some(variable) = &mut variable {
      variable.used.push(span);
    }
  }

  pub(crate) fn depth(&self) -> u32 {
    self.depth
  }

  /// An iterator over all the variables that are defined
  pub fn defined_variables(&self) -> impl Iterator<Item = &Variable> {
    self.finished_variables.iter()
  }
}

/// A variable which is defined and all the times it is used
#[derive(Debug)]
pub struct Variable {
  /// the identifier of the variable
  pub name: String,
  /// the span where the variable was defined
  defined: Span,
  /// the spans where the variable was used
  pub used: Vec<Span>,
  /// the span where the variable is active and can be used
  active: Span,

  /// the depth of the variable, used to track the scope of the variable
  depth: u32,
  /// the type of the variable
  type_: TypeScheme,
}
impl Variable {
  /// Checks if the variable is used
  pub(crate) fn is_used(&self) -> bool {
    !self.used.is_empty()
  }

  /// Is the variable active at the given position
  #[must_use]
  pub fn is_active(&self, position: Span) -> bool {
    self.active.contains(position)
  }

  pub(crate) fn type_(&self) -> TypeRef {
    self.type_.type_
  }
}
impl GetSpan for Variable {
  fn span(&self) -> Span {
    self.defined
  }
}
