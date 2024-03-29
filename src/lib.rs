//! # Bang - My Language

#![feature(let_chains)]
#![feature(lint_reasons)]
#![feature(strict_provenance)]
#![feature(decl_macro)]

pub mod ast;
mod formatter;
mod interpreter;
mod linter;
mod parser;

/// More efficient datastructures than in standard library
pub(crate) mod collections {
  pub use rustc_hash::FxHashMap as HashMap;
  pub use smallvec::SmallVec;
  pub use smartstring::alias::String;
}

pub(crate) mod allocator {
  /// Arena allocator
  pub type Allocator = bumpalo::Bump;
  pub type Box<'allocator, T> = bumpalo::boxed::Box<'allocator, T>;
  pub type Vec<'allocator, T> = bumpalo::collections::Vec<'allocator, T>;
}

#[doc(inline)]
pub use allocator::Allocator;

#[doc(inline)]
pub use ast::AST;

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
pub use parser::ParseError;

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
pub use linter::LintDiagnostic;

/// Opinionated formatting of an AST into a string.
/// Tries to respect the print width given in the config.
///
/// # Examples
/// ```
/// use bang::{format, parse, Allocator, FormatterConfig};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator).unwrap();
/// let config = FormatterConfig::default();
/// let formatted = format(&ast, config);
/// ```
#[must_use]
pub fn format(ast: &AST, config: FormatterConfig) -> String {
  let allocator = Allocator::new();
  let formatter = formatter::Formatter::new(config, &allocator);
  formatter.print(ast)
}
pub use formatter::Config as FormatterConfig;

/// Compile an AST into a bytecode chunk
///
/// # Examples
/// ```
/// use bang::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator).unwrap();
/// let chunk = bang::compile(&ast).unwrap();
/// ```
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile<'s: 'a, 'a>(ast: &AST<'s, '_>) -> Result<Chunk, CompileError> {
  let mut compiler = interpreter::Compiler::new();
  compiler.compile(ast)?;
  let chunk = compiler.finish();
  Ok(chunk)
}
pub use interpreter::{Chunk, CompileError, RuntimeError, Value, VM};
