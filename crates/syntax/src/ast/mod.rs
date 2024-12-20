//! # AST
//!
//! The definition of the  Abstract Syntax Tree (AST)

use crate::{
  LineIndex,
  parser::ParseError,
  tokeniser::{Token, Tokeniser},
};
use std::{cell::OnceCell, num::NonZero, ops, str};

pub mod expression;
mod prettyprint;
pub mod statement;
pub mod types;

#[doc(inline)]
pub use expression::Expression;
#[doc(inline)]
pub use statement::Statement;
#[doc(inline)]
pub use types::Type;

/// Abstract Syntax Tree representing the source
#[must_use]
#[derive(Debug)]
pub struct AST {
  /// The source code which the AST is for
  pub source: String,
  /// The tokens of the source
  pub tokens: Vec<Token>,

  /// The main statements of the source
  pub root_statements: Vec<Statement>,
  /// Statements which appear within [`expression::Block`] expressions
  pub statements: Vec<Statement>,
  /// The expressions in the source
  pub expressions: Vec<Expression>,
  /// The types in the source
  pub types: Vec<Type>,

  /// Errors found during parsing
  pub errors: Vec<ParseError>,
  /// Index of line locations, lazily constructed when required
  line_index: OnceCell<LineIndex>,
}
impl AST {
  pub(crate) fn new(source: String) -> Self {
    let tokens = Tokeniser::from(source.as_str()).collect();

    Self {
      source,
      tokens,

      root_statements: Vec::new(),
      statements: Vec::new(),
      expressions: Vec::new(),
      types: Vec::new(),

      errors: Vec::new(),
      line_index: OnceCell::new(),
    }
  }

  /// Reset the AST into an new state, so the allocation can be reused
  pub(crate) fn reuse(&mut self, source: String) {
    self.source = source;
    self.tokens.clear();
    self.tokens.extend(Tokeniser::from(self.source.as_str()));

    self.line_index = OnceCell::new();
    self.root_statements.clear();
    self.statements.clear();
    self.expressions.clear();
    self.types.clear();
    self.errors.clear();
  }

  /// Is the parsed AST valid, with no errors found during parsing?
  #[must_use]
  pub fn is_valid(&self) -> bool {
    self.errors.is_empty()
  }

  /// Is the AST able to be formatted?
  ///
  /// There may be errors, but the formatting would be valid
  #[must_use]
  pub fn is_formattable(&self) -> bool {
    self.errors.iter().all(ParseError::is_formattable)
  }

  /// The line index for the AST, the locations of the new lines in the source code
  ///
  /// It is lazily initialised, and will be initialised on the first call
  pub fn line_index(&self) -> &LineIndex {
    self
      .line_index
      .get_or_init(|| LineIndex::from_source(&self.source))
  }

  /// All statements in the source
  pub fn all_statements(&self) -> impl Iterator<Item = &Statement> {
    self.root_statements.iter().chain(self.statements.iter())
  }

  /// Gets the [`Type`] that has been parsed
  ///
  /// # Panics
  /// This will panic if the AST was not parsed using [`crate::parse_type`]
  pub fn get_type(&self) -> &Type {
    assert!(!self.types.is_empty());

    // Make sure that it is not used on a "normal" AST
    assert!(self.root_statements.is_empty());
    assert!(self.statements.is_empty());
    assert!(self.expressions.is_empty());

    self.types.last().unwrap()
  }

  pub(crate) fn add_expression(&mut self, expression: impl Into<Expression>) -> ExpressionIdx {
    let id = self.expressions.len() + 1;
    self.expressions.push(expression.into());
    ExpressionIdx(NonZero::new(u32::try_from(id).unwrap()).unwrap())
  }

  pub(crate) fn add_statement(&mut self, statement: Statement) -> StatementIdx {
    let id = self.statements.len() + 1;
    self.statements.push(statement);
    StatementIdx(NonZero::new(u32::try_from(id).unwrap()).unwrap())
  }

  pub(crate) fn add_type(&mut self, type_: impl Into<Type>) -> TypeIdx {
    let id = self.types.len() + 1;
    self.types.push(type_.into());
    TypeIdx(NonZero::new(u32::try_from(id).unwrap()).unwrap())
  }

  pub(crate) fn get_token_text(&self, token: TokenIdx) -> &str {
    let token = self[token];

    let start = usize::try_from(token.start).unwrap();
    let end = start + usize::from(token.length);

    unsafe { str::from_utf8_unchecked(&self.source.as_bytes()[start..end]) }
  }
}

impl ops::Index<ExpressionIdx> for AST {
  type Output = Expression;

  fn index(&self, index: ExpressionIdx) -> &Self::Output {
    let index = usize::try_from(index.0.get()).unwrap() - 1;
    &self.expressions[index]
  }
}
impl ops::IndexMut<ExpressionIdx> for AST {
  fn index_mut(&mut self, index: ExpressionIdx) -> &mut Self::Output {
    &mut self.expressions[usize::try_from(index.0.get()).unwrap() - 1]
  }
}

impl ops::Index<TypeIdx> for AST {
  type Output = Type;

  fn index(&self, index: TypeIdx) -> &Self::Output {
    let index = usize::try_from(index.0.get()).unwrap() - 1;
    &self.types[index]
  }
}
impl ops::Index<StatementIdx> for AST {
  type Output = Statement;

  fn index(&self, index: StatementIdx) -> &Self::Output {
    let index = usize::try_from(index.0.get()).unwrap() - 1;
    &self.statements[index]
  }
}
impl ops::Index<TokenIdx> for AST {
  type Output = Token;

  fn index(&self, index: TokenIdx) -> &Self::Output {
    let index = usize::try_from(index.0.get()).unwrap() - 1;
    &self.tokens[index]
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ExpressionIdx(NonZero<u32>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct StatementIdx(NonZero<u32>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeIdx(NonZero<u32>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TokenIdx(NonZero<u32>);
impl TokenIdx {
  fn next(self) -> Self {
    Self(NonZero::new(self.0.get() + 1).unwrap())
  }

  fn range(self, end: Self) -> impl Iterator<Item = Self> {
    (self.0.get()..=end.0.get()).map(|x| Self(NonZero::new(x).unwrap()))
  }
}
impl From<usize> for TokenIdx {
  fn from(value: usize) -> Self {
    Self(NonZero::new(u32::try_from(value + 1).unwrap()).unwrap())
  }
}
