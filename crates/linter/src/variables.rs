use super::visitor::Visitor;
use bang_parser::{
  ast::{expression, Expression, Statement},
  GetSpan, Span, AST,
};

/// Tracks where variables are defined and used
#[derive(Debug, Default)]
pub struct Variables {
  scope: Vec<Vec<Variable>>,
  finished: Vec<Variable>,
}
impl Variables {
  fn start_scope(&mut self) {
    self.scope.push(Vec::new());
  }
  fn end_scope(&mut self, span: Span) {
    let current_scope = self.scope.pop().unwrap();

    for mut variable in current_scope {
      variable.active = variable.active.merge(span);
      self.finished.push(variable);
    }
  }

  fn add_variable(&mut self, variable: &expression::Variable, definition_span: Span) {
    let current_scope = self.scope.last_mut().unwrap();

    current_scope.push(Variable {
      name: variable.name.to_owned(),
      defined: variable.span(),
      used: Vec::new(),
      active: definition_span,
    });
  }

  fn get_variable(&mut self, name: &str) -> Option<&mut Variable> {
    self
      .scope
      .iter_mut()
      .rev()
      .find_map(|scope| scope.iter_mut().find(|var| var.name == name))
  }

  /// Returns an iterator over all the variables that are defined
  pub fn defined(&self) -> impl Iterator<Item = &Variable> {
    self.finished.iter()
  }
}
impl Visitor for Variables {
  fn enter_expression(&mut self, expression: &bang_parser::ast::Expression) {
    if let Expression::Block(_) = expression {
      self.start_scope();
    }

    if let Expression::Function(function) = expression {
      self.start_scope();
      self.add_variable(&function.parameter, function.span());
    }

    if let Expression::Variable(variable) = expression {
      if let Some(defined_variable) = self.get_variable(variable.name) {
        defined_variable.used.push(variable.span());
      }
    }
  }

  fn visit_match_case(&mut self, case: &expression::MatchCase) {
    // Match case is not an expression or statement, but we still need to visit it as it has its own scope

    if let expression::Pattern::Identifier(identifier) = &case.pattern {
      self.start_scope();
      self.add_variable(identifier, case.span());
      self.visit_expression(&case.expression);
      self.end_scope(case.span());
    } else {
      self.visit_expression(&case.expression);
    }
  }

  fn enter_statement(&mut self, stmt: &bang_parser::ast::Statement) {
    if let Statement::Let(let_) = stmt {
      self.add_variable(&let_.identifier, let_.span());
    }
  }

  fn exit_expression(&mut self, expression: &bang_parser::ast::Expression) {
    if let Expression::Block(_) | Expression::Function(_) = expression {
      self.end_scope(expression.span());
    }
  }
}
impl From<&AST<'_, '_>> for Variables {
  fn from(ast: &AST) -> Self {
    let mut variables = Self::default();

    variables.start_scope();
    for statement in &ast.statements {
      variables.visit_statement(statement);
    }
    variables.end_scope(ast.statements.last().map(GetSpan::span).unwrap_or_default());

    variables
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
}
impl Variable {
  pub fn is_used(&self) -> bool {
    !self.used.is_empty()
  }

  pub fn is_active(&self, position: Span) -> bool {
    self.active.contains(position)
  }
}
impl GetSpan for Variable {
  fn span(&self) -> Span {
    self.defined
  }
}
