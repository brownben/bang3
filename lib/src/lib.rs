//! # Bang
//!
//! My attempt at creating my own language. A strongly typed, functional, bytecode interpreter written.

#![feature(let_chains)]
#![feature(strict_provenance)]
#![feature(decl_macro)]
#![feature(negative_impls)]
#![deny(unsafe_code)]

mod ast;
mod formatter;
mod interpreter;
mod linter;
mod parser;
mod typechecker;

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
pub use allocator::Allocator;

/// Parses a source code string into an AST
///
/// # Examples
/// ```
/// use bang::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// ```
///
/// # Errors
/// If the syntax of the source is invalid.
pub fn parse<'source, 'ast>(
  source: &'source str,
  allocator: &'ast Allocator,
) -> AST<'source, 'ast> {
  parser::Parser::new(source, allocator).parse()
}
pub use ast::{GetSpan, LineIndex, Span};
pub use parser::{ParseError, AST};

/// Compile an AST into a bytecode chunk
///
/// # Examples
/// ```
/// use bang::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let chunk = bang::compile(&ast, &allocator).unwrap();
/// ```
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile<'s: 'a, 'a>(
  ast: &AST<'s, '_>,
  allocator: &Allocator,
) -> Result<Chunk, CompileError> {
  let mut compiler = interpreter::Compiler::new(allocator);
  compiler.compile(ast)?;
  let chunk = compiler.finish();
  Ok(chunk)
}
pub use interpreter::{Chunk, CompileError, FastNativeFunction, RuntimeError, Value, VM};

/// Runs the linter against a given AST, returns a list of diagnostics found
///
/// # Examples
/// ```
/// use bang::{lint, parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
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
/// let ast = parse(source, &allocator);
/// let config = FormatterConfig::default();
/// let formatted = format(&ast, config);
/// ```
#[must_use]
pub fn format(ast: &AST, config: FormatterConfig) -> String {
  let allocator = Allocator::new();
  let formatter = formatter::Formatter::new(config, &allocator);
  formatter.print(ast)
}
pub use formatter::config;
pub use formatter::config::Config as FormatterConfig;

/// Runs the typechecker against a given AST, returns a list of errors found
///
/// # Examples
/// ```
/// use bang::{typecheck, parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let diagnostics = typecheck(&ast);
/// ```
#[must_use]
pub fn typecheck(ast: &AST) -> Vec<TypeError> {
  typechecker::Typechecker::new().check(ast)
}
pub use typechecker::TypeError;
