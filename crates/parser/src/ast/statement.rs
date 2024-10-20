use super::{
  expression::{Expression, Variable},
  span::{GetSpan, Span},
};
use crate::allocator::{Box, Vec};

#[derive(Debug)]
pub enum Statement<'source, 'ast> {
  Comment(Box<'ast, CommentStmt<'source>>),
  Expression(Box<'ast, Expression<'source, 'ast>>),
  Import(Box<'ast, Import<'source, 'ast>>),
  Let(Box<'ast, Let<'source, 'ast>>),
  Return(Box<'ast, Return<'source, 'ast>>),
}

#[derive(Debug)]
pub struct CommentStmt<'source> {
  pub text: &'source str,
  pub span: Span,
}

#[derive(Debug)]
pub struct Import<'source, 'ast> {
  pub module: Variable<'source>,
  pub items: Vec<'ast, ImportItem<'source>>,
  pub items_span: Span,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ImportItem<'source> {
  pub name: Variable<'source>,
  pub alias: Option<Variable<'source>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Let<'source, 'ast> {
  pub identifier: Variable<'source>,
  pub expression: Expression<'source, 'ast>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Return<'source, 'ast> {
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
impl<'s, 'ast> From<Box<'ast, Import<'s, 'ast>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, Import<'s, 'ast>>) -> Self {
    Self::Import(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Let<'s, 'ast>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, Let<'s, 'ast>>) -> Self {
    Self::Let(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Return<'s, 'ast>>> for Statement<'s, 'ast> {
  fn from(value: Box<'ast, Return<'s, 'ast>>) -> Self {
    Self::Return(value)
  }
}

impl GetSpan for Statement<'_, '_> {
  fn span(&self) -> Span {
    match self {
      Statement::Comment(x) => x.span(),
      Statement::Expression(x) => x.span(),
      Statement::Import(x) => x.span(),
      Statement::Let(x) => x.span(),
      Statement::Return(x) => x.span(),
    }
  }
}
impl GetSpan for CommentStmt<'_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Import<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for ImportItem<'_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Let<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Return<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
