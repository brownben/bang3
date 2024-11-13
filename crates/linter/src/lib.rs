//! # Linter
//! Static analysis to find possible problems in an AST

#![feature(let_chains)]
#![feature(decl_macro)]
#![deny(unsafe_code)]

use bang_syntax::{
  ast::{expression::Expression, statement::Statement},
  Span, AST,
};
use std::{error, fmt};

mod helpers;
mod rules;

/// Runs the linter against a given AST, returns a list of diagnostics found
///
/// # Examples
/// ```
/// use bang_linter::lint;
/// use bang_syntax::parse;
/// let source = "5 + 3";
/// let ast = parse(source);
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
    for statement in &ast.root_statements {
      for rule in rules::RULES {
        rule.visit_statement(&mut self.context, statement, ast);
      }
    }
    for statement in &ast.statements {
      for rule in rules::RULES {
        rule.visit_statement(&mut self.context, statement, ast);
      }
    }
    for expression in &ast.expressions {
      for rule in rules::RULES {
        rule.visit_expression(&mut self.context, expression, ast);
      }
    }

    self.context.diagnostics
  }
}

trait LintRule {
  fn name(&self) -> &'static str;
  fn message(&self) -> &'static str;
  fn is_unused(&self) -> bool {
    false
  }

  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    let (_, _, _) = (context, expression, ast);
  }
  fn visit_statement(&self, context: &mut Context, statement: &Statement, ast: &AST) {
    let (_, _, _) = (context, statement, ast);
  }
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
      is_unused: rule.is_unused(),
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
  /// Whether this diagnostic indicates unused code
  is_unused: bool,
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

  /// Whether this diagnostic indicates unused code
  #[must_use]
  pub fn is_unused(&self) -> bool {
    self.is_unused
  }

  /// The span of the source code which triggered this diagnostic
  pub fn span(&self) -> Span {
    self.span
  }
}
impl fmt::Display for LintDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.title, self.message)
  }
}
impl error::Error for LintDiagnostic {}
