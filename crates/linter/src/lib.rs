//! # Linter
//! Static analysis to find possible problems in an AST

#![feature(let_chains)]
#![feature(decl_macro)]
#![deny(unsafe_code)]

use bang_parser::{
  ast::{Expression, Statement},
  GetSpan, Span, AST,
};
use std::{error, fmt};

mod helpers;
mod rules;
mod visitor;

#[cfg(test)]
mod test;
use visitor::Visitor;

/// Runs the linter against a given AST, returns a list of diagnostics found
///
/// # Examples
/// ```
/// use bang_linter::lint;
/// use bang_parser::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let diagnostics = lint(&ast);
/// ```
#[must_use]
pub fn lint(ast: &AST) -> Vec<LintDiagnostic> {
  Linter::new().check(ast)
}

#[derive(Debug, Default)]
struct Linter {
  context: Context,
}
impl Linter {
  fn new() -> Self {
    Self::default()
  }

  fn check(mut self, ast: &AST) -> Vec<LintDiagnostic> {
    for statement in &ast.statements {
      self.visit_statement(statement);
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
#[derive(Clone, Debug)]
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
