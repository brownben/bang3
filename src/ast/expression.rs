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
  Function(Box<'ast, Function<'source, 'ast>>),
  Group(Box<'ast, Group<'source, 'ast>>),
  If(Box<'ast, If<'source, 'ast>>),
  Literal(Box<'ast, Literal<'source>>),
  Match(Box<'ast, Match<'source, 'ast>>),
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
  AddString,
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
impl BinaryOperator {
  pub fn as_str(self) -> &'static str {
    match self {
      Self::Add => "+",
      Self::Subtract => "-",
      Self::Multiply => "*",
      Self::Divide => "/",
      Self::Remainder => "%",
      Self::AddString => "++",
      Self::NotEqual => "!=",
      Self::Equal => "==",
      Self::Greater => ">",
      Self::GreaterEqual => ">=",
      Self::Less => "<",
      Self::LessEqual => "<=",
      Self::And => "and",
      Self::Or => "or",
      Self::Pipeline => ">>",
    }
  }
}
impl fmt::Display for BinaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.as_str())
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
  pub argument: Expression<'source, 'ast>,
  pub span: Span,
}

#[derive(Debug)]
pub struct Comment<'source, 'ast> {
  pub expression: Expression<'source, 'ast>,
  pub text: &'source str,
  pub span: Span,
}

#[derive(Debug)]
pub struct Function<'source, 'ast> {
  pub parameter: &'source str,
  pub body: Expression<'source, 'ast>,
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
#[derive(Debug, PartialEq)]
pub enum LiteralKind<'source> {
  Boolean(bool),
  Number { value: f64, raw: &'source str },
  String(&'source str),
}

#[derive(Debug)]
pub struct Match<'source, 'ast> {
  pub value: Expression<'source, 'ast>,
  pub cases: Vec<'ast, MatchCase<'source, 'ast>>,
  pub span: Span,
}
#[derive(Debug)]
pub struct MatchCase<'source, 'ast> {
  pub pattern: Pattern<'source, 'ast>,
  pub expression: Expression<'source, 'ast>,
  pub span: Span,
}
#[derive(Debug)]
pub enum Pattern<'source, 'ast> {
  Identifier(Variable<'source>),
  Literal(Literal<'source>),
  Range(Box<'ast, PatternRange<'source, 'ast>>),
}
#[derive(Debug)]
pub struct PatternRange<'source, 'ast> {
  pub start: Option<Pattern<'source, 'ast>>,
  pub end: Option<Pattern<'source, 'ast>>,
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
impl UnaryOperator {
  pub fn as_str(self) -> &'static str {
    match self {
      Self::Not => "!",
      Self::Minus => "-",
    }
  }
}
impl fmt::Display for UnaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.as_str())
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
impl<'s, 'ast> From<Box<'ast, Function<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Function<'s, 'ast>>) -> Self {
    Self::Function(value)
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
impl<'s, 'ast> From<Box<'ast, Match<'s, 'ast>>> for Expression<'s, 'ast> {
  fn from(value: Box<'ast, Match<'s, 'ast>>) -> Self {
    Self::Match(value)
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
      Self::Function(x) => x.span(),
      Self::Group(x) => x.span(),
      Self::If(x) => x.span(),
      Self::Literal(x) => x.span(),
      Self::Match(x) => x.span(),
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
impl GetSpan for Function<'_, '_> {
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
impl GetSpan for Match<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for MatchCase<'_, '_> {
  fn span(&self) -> Span {
    self.span
  }
}
impl GetSpan for Pattern<'_, '_> {
  fn span(&self) -> Span {
    match self {
      Pattern::Identifier(x) => x.span(),
      Pattern::Literal(x) => x.span(),
      Pattern::Range(x) => x.span(),
    }
  }
}
impl GetSpan for PatternRange<'_, '_> {
  fn span(&self) -> Span {
    match (&self.start, &self.end) {
      (None, None) => unreachable!("range must have either start or end"),
      (None, Some(x)) | (Some(x), None) => x.span(),
      (Some(start), Some(end)) => start.span().merge(end.span()),
    }
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
