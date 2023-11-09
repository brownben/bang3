use super::{
  span::{GetSpan, Span},
  statement::Statement,
};
use crate::allocator::{Box, Vec};
use std::fmt;

#[derive(Debug)]
pub enum Expression<'source, 'ast> {
  Binary(Box<'ast, Binary<'source, 'ast>>),
  Block(Box<'ast, Block<'source, 'ast>>),
  Call(Box<'ast, Call<'source, 'ast>>),
  Comment(Box<'ast, Comment<'source, 'ast>>),
  Group(Box<'ast, Group<'source, 'ast>>),
  If(Box<'ast, If<'source, 'ast>>),
  Literal(Box<'ast, Literal<'source>>),
  Unary(Box<'ast, Unary<'source, 'ast>>),
  Variable(Box<'ast, Variable<'source>>),
}

#[derive(Debug)]
pub struct Binary<'source, 'ast> {
  pub left: Expression<'source, 'ast>,
  pub operator: BinaryOperator,
  pub right: Expression<'source, 'ast>,
  pub span: Span,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Remainder,
  NotEqual,
  Equal,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  And,
  Or,
  Pipeline,
}
impl fmt::Display for BinaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Add => write!(f, "+"),
      Self::Subtract => write!(f, "-"),
      Self::Multiply => write!(f, "*"),
      Self::Divide => write!(f, "/"),
      Self::Remainder => write!(f, "%"),
      Self::NotEqual => write!(f, "!="),
      Self::Equal => write!(f, "=="),
      Self::Greater => write!(f, ">"),
      Self::GreaterEqual => write!(f, ">="),
      Self::Less => write!(f, "<"),
      Self::LessEqual => write!(f, "<="),
      Self::And => write!(f, "and"),
      Self::Or => write!(f, "or"),
      Self::Pipeline => write!(f, ">>"),
    }
  }
}

#[derive(Debug)]
pub struct Block<'source, 'ast> {
  pub statements: Vec<'ast, Statement<'source, 'ast>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Call<'source, 'ast> {
  pub expression: Expression<'source, 'ast>,
  pub argument: Option<Expression<'source, 'ast>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Comment<'source, 'ast> {
  pub expression: Expression<'source, 'ast>,
  pub text: &'source str,
  pub span: Span,
}

#[derive(Debug)]
pub struct Group<'source, 'ast> {
  pub expression: Expression<'source, 'ast>,
  pub span: Span,
}

#[derive(Debug)]
pub struct If<'source, 'ast> {
  pub condition: Expression<'source, 'ast>,
  pub then: Expression<'source, 'ast>,
  pub otherwise: Option<Expression<'source, 'ast>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Literal<'source> {
  pub kind: LiteralKind<'source>,
  pub span: Span,
}
#[derive(Debug)]
pub enum LiteralKind<'source> {
  Boolean(bool),
  Number(f64),
  String(&'source str),
}

#[derive(Debug)]
pub struct Unary<'source, 'ast> {
  pub operator: UnaryOperator,
  pub expression: Expression<'source, 'ast>,
  pub span: Span,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
  Not,
  Minus,
}
impl fmt::Display for UnaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Not => write!(f, "!"),
      Self::Minus => write!(f, "-"),
    }
  }
}

#[derive(Debug)]
pub struct Variable<'source> {
  pub name: &'source str,
  pub span: Span,
}

impl<'s, 'ast> From<Box<'ast, Binary<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Binary<'s, 'ast>>) -> Self {
    Self::Binary(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Block<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Block<'s, 'ast>>) -> Self {
    Self::Block(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Call<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Call<'s, 'ast>>) -> Self {
    Self::Call(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Comment<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Comment<'s, 'ast>>) -> Self {
    Self::Comment(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Group<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Group<'s, 'ast>>) -> Self {
    Self::Group(value)
  }
}
impl<'s, 'ast> From<Box<'ast, If<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, If<'s, 'ast>>) -> Self {
    Self::If(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Literal<'s>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Literal<'s>>) -> Self {
    Self::Literal(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Unary<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Unary<'s, 'ast>>) -> Self {
    Self::Unary(value)
  }
}
impl<'s, 'ast> From<Box<'ast, Variable<'s>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Variable<'s>>) -> Self {
    Self::Variable(value)
  }
}

impl GetSpan for Expression<'_, '_> {
  fn span(&self) -> Span {
    match self {
      Self::Binary(x) => x.span(),
      Self::Block(x) => x.span(),
      Self::Call(x) => x.span(),
      Self::Comment(x) => x.span(),
      Self::Group(x) => x.span(),
      Self::If(x) => x.span(),
      Self::Literal(x) => x.span(),
      Self::Unary(x) => x.span(),
      Self::Variable(x) => x.span(),
    }
  }
}
impl GetSpan for Binary<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Block<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Comment<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Call<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Group<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for If<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Literal<'_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Unary<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Variable<'_> {
  fn span(&self) -> Span {
    self.span
  }
}
