use super::{
  expression::Expression,
  span::{GetSpan, Span},
};
use crate::allocator::Box;

#[derive(Debug)]
pub enum Statement<'source, 'ast> {
  Comment(Box<'ast, CommentStmt<'source>>),
  Expression(Box<'ast, Expression<'source, 'ast>>),
  Let(Box<'ast, Let<'source, 'ast>>),
}

#[derive(Debug)]
pub struct CommentStmt<'source> {
  pub text: &'source str,
  pub span: Span,
}

#[derive(Debug)]
pub struct Let<'source, 'ast> {
  pub identifier: &'source str,
  pub expression: Expression<'source, 'ast>,
  pub span: Span,
}

impl<'s, 'ast> From<Box<'ast, CommentStmt<'s>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, CommentStmt<'s>>) -> Self {
    Self::Comment(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Expression<'s, 'ast>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, Expression<'s, 'ast>>) -> Self {
    Self::Expression(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Let<'s, 'ast>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, Let<'s, 'ast>>) -> Self {
    Self::Let(value)
  }
}

impl GetSpan for Statement<'_, '_> {
  fn span(&self) -> Span {
    match self {
      Statement::Comment(x) => x.span(),
      Statement::Expression(x) => x.span(),
      Statement::Let(x) => x.span(),
    }
  }
}
impl GetSpan for CommentStmt<'_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Let<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
