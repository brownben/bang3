//! # Syntax
//! Parse source code into an Abstract Syntax Tree
//!
//! A pratt parser, based on [Crafting Interpreters](https://craftinginterpreters.com/parsing-expressions.html).
//!
//! It is a red-green style AST. The source is first tokenised and then subsequently a
//! tree is built from the tokens, with the tree referencing the tokens. All references
//! for tokens, expressions, and statements is done with integers to save space - so when
//! the tree is accessed the ast needs to be passed as a reference.
//!
//! Tries to be error tolerant, and recover from errors to continue parsing. If a token
//! is expected but is not found it will just assume it exists and continue parsing
//! (adding an error), otherwise if an error is found it will skip to the start of a new
//! line and try parsing again.

#![feature(let_chains)]
#![feature(decl_macro)]

pub mod ast;
mod parser;
mod span;
mod tokeniser;

#[cfg(test)]
mod test;

/// Parses a source code string into an AST.
///
/// # Examples
/// ```
/// use bang_syntax::parse;
/// let ast = parse("5 + 3");
///
/// assert!(ast.is_valid());
/// ```
pub fn parse(source: &str) -> AST {
  parser::Parser::new(source).parse()
}

/// Get the tokens from a source code string
pub fn tokenise(source: &str) -> impl Iterator<Item = tokeniser::Token> + '_ {
  tokeniser::Tokeniser::from(source)
}

pub use ast::AST;
pub use parser::ParseError;
pub use span::{LineIndex, Span};
pub use tokeniser::{Token, TokenKind};
