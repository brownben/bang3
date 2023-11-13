//! # Abstract Syntax Tree
//!
//! The output of the parser, a tree representation of the source code which can be used for compilation and analysis

pub(crate) mod expression;
pub(crate) use expression::Expression;

mod lines;
pub use lines::LineIndex;

mod prettyprint;

mod span;
pub use span::{GetSpan, Span};

pub(crate) mod statement;
pub(crate) use statement::Statement;

mod visitor;
pub(crate) use visitor::Visitor;

use crate::allocator::Vec;
/// An Abstract Syntax Tree produced by the parser
#[derive(Debug)]
pub struct AST<'source, 'allocator> {
  pub(crate) statements: Vec<'allocator, Statement<'source, 'allocator>>,
}
