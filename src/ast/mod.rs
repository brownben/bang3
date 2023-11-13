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
pub struct AST<'source, 'allocator> {
  pub(crate) statements: Vec<'allocator, Statement<'source, 'allocator>>,
}
