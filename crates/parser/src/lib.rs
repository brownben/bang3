//! # Parser
//! Parse source code into an Abstract Syntax Tree
//!
//! A pratt parser, based on [Crafting Interpreters](https://craftinginterpreters.com/parsing-expressions.html).
//!
//! Tries to be error tolerant, and recover from errors to continue parsing. If a token
//! is expected but is not found it will just assume it exists and continue parsing
//! (adding an error), otherwise if an error is found it will skip to the start of a new
//! line and try parsing again.

#![feature(let_chains)]
#![feature(decl_macro)]
#![deny(unsafe_code)]

#[doc(hidden)]
pub mod ast;
mod parser;
mod tokeniser;

#[cfg(test)]
mod test;

/// Parses a source code string into an AST
///
/// # Examples
/// ```
/// use bang_parser::{parse, Allocator};
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

/// Bump allocator for the AST
pub mod allocator {
  /// Arena allocator
  pub type Allocator = bumpalo::Bump;
  /// `Box` in the bump allocator
  pub type Box<'allocator, T> = bumpalo::boxed::Box<'allocator, T>;
  /// `Vec` in the bump allocator
  pub type Vec<'allocator, T> = bumpalo::collections::Vec<'allocator, T>;
}
pub use allocator::Allocator;
