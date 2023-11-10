mod helpers;
mod rules;

#[cfg(test)]
mod test;

use crate::ast::{Expression, Span, Statement, Visitor, AST};

trait LintRule {
  fn name(&self) -> &'static str;
  fn message(&self) -> &'static str;

  fn visit_expression(&self, _: &mut Context, _: &Expression) {}
  fn visit_statement(&self, _: &mut Context, _: &Statement) {}
}

#[derive(Debug, Default)]
pub struct Linter {
  context: Context,
}
impl Linter {
  fn new() -> Self {
    Self::default()
  }

  pub fn check(ast: &AST) -> Vec<Diagnostic> {
    let mut linter = Self::new();

    for statement in &ast.statements {
      linter.visit_statement(statement);
    }

    linter.context.diagnostics
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

#[derive(Debug)]
pub struct Diagnostic {
  pub title: &'static str,
  pub message: &'static str,
  pub span: Span,
}

#[derive(Debug, Default)]
struct Context {
  diagnostics: Vec<Diagnostic>,
}
impl Context {
  fn add_diagnostic(&mut self, rule: &'static dyn LintRule, span: Span) {
    self.diagnostics.push(Diagnostic {
      title: rule.name(),
      message: rule.message(),
      span,
    });
  }
}
