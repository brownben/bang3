//! # Bang - My Language

#![feature(let_chains)]
#![feature(lint_reasons)]
#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::wildcard_imports)]

mod allocator;
pub mod ast;
mod linter;
mod parser;

#[doc(inline)]
pub use allocator::Allocator;
#[doc(inline)]
pub use ast::AST;
pub use linter::LintDiagnostic;
pub use parser::ParseError;

/// Parses a source code string into an AST
///
/// # Examples
/// ```
/// use bang::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator).unwrap();
/// ```
///
/// # Errors
/// If the syntax of the source is invalid.
pub fn parse<'source, 'ast>(
  source: &'source str,
  allocator: &'ast Allocator,
) -> Result<AST<'source, 'ast>, ParseError> {
  parser::Parser::new(source, allocator).parse()
}

/// Runs the linter against a given AST, returns a list of diagnostics found
///
/// # Examples
/// ```
/// use bang::{lint, parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator).unwrap();
/// let diagnostics = lint(&ast);
/// ```
#[must_use]
pub fn lint(ast: &AST) -> Vec<LintDiagnostic> {
  linter::Linter::new().check(ast)
}
