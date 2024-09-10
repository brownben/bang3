//! # Linter
//! Static analysis to find possible problems in an AST

mod helpers;
mod rules;
mod variables;
mod visitor;

#[cfg(test)]
mod test;

use crate::{
  ast::{Expression, GetSpan, Span, Statement},
  parser::AST,
};
use std::{error, fmt};

pub(crate) use variables::Variables;
use visitor::Visitor;

#[derive(Debug, Default)]
pub struct Linter {
  context: Context,
}
impl Linter {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn check(mut self, ast: &AST) -> Vec<LintDiagnostic> {
    for statement in &ast.statements {
      self.visit_statement(statement);
    }

    let variables = Variables::from(ast);
    for rule in rules::RULES {
      rule.visit_variables(&mut self.context, &variables);
    }

    self.context.diagnostics
  }
}
impl Visitor for Linter {
  fn enter_expression(&mut self, expression: &Expression) {
    for rule in rules::RULES {
      rule.visit_expression(&mut self.context, expression);
    }
  }

  fn enter_statement(&mut self, statement: &Statement) {
    for rule in rules::RULES {
      rule.visit_statement(&mut self.context, statement);
    }
  }
}

trait LintRule {
  fn name(&self) -> &'static str;
  fn message(&self) -> &'static str;

  fn visit_expression(&self, _: &mut Context, _: &Expression) {}
  fn visit_statement(&self, _: &mut Context, _: &Statement) {}

  fn visit_variables(&self, _: &mut Context, _: &Variables) {}
}

#[derive(Debug, Default)]
struct Context {
  diagnostics: Vec<LintDiagnostic>,
}
impl Context {
  fn add_diagnostic(&mut self, rule: &'static dyn LintRule, span: Span) {
    self.diagnostics.push(LintDiagnostic {
      title: rule.name(),
      message: rule.message(),
      span,
    });
  }
}

/// A diagnostic warning found by the linter
#[derive(Clone, Copy, Debug)]
pub struct LintDiagnostic {
  /// The name of the lint rule which triggered this diagnostic
  pub title: &'static str,
  /// The help message of the lint rule
  pub message: &'static str,
  /// The span of the source code which triggered this diagnostic
  span: Span,
}
impl LintDiagnostic {
  /// The title and message of the lint in a combined string
  #[must_use]
  pub fn full_message(&self) -> String {
    let mut message = self.title.to_owned();
    message.push('\n');
    message.push_str(self.message);
    message
  }
}
impl GetSpan for LintDiagnostic {
  fn span(&self) -> Span {
    self.span
  }
}
impl fmt::Display for LintDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.title, self.message)
  }
}
impl error::Error for LintDiagnostic {}
