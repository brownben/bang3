pub mod expression;
pub use expression::Expression;

mod prettyprint;

mod span;
pub use span::{GetSpan, Span};

pub mod statement;
pub use statement::Statement;

mod visitor;
pub use visitor::Visitor;

use crate::allocator::Vec;
pub struct AST<'source, 'allocator> {
  pub(crate) statements: Vec<'allocator, Statement<'source, 'allocator>>,
}
