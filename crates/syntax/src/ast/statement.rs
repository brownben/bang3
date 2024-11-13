//! # Statements

use crate::{
  ast::{
    expression::{Expression, Variable},
    ExpressionIdx, TokenIdx, AST,
  },
  span::Span,
  tokeniser::TokenKind,
};

/// A statement in the source code.
/// They don't produce a value, and are delimited by newlines.
#[must_use]
#[derive(Debug)]
pub enum Statement {
  /// A comment on it's own line
  Comment(CommentStmt),
  /// An expression
  Expression(ExpressionStmt),
  /// An import, e.g. `from maths import { sin, cos as cosine, tan }`
  Import(Import),
  /// A variable declaration, e.g. `let x = 1`
  Let(Let),
  /// A return statement, e.g. `return 1`
  Return(Return),
}
impl Statement {
  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    match self {
      Self::Comment(comment) => comment.span(ast),
      Self::Expression(expression) => expression.span(ast),
      Self::Import(import) => import.span(ast),
      Self::Let(let_stmt) => let_stmt.span(ast),
      Self::Return(return_stmt) => return_stmt.span(ast),
    }
  }
}

/// A comment on it's own line
#[derive(Debug)]
pub struct CommentStmt {
  pub(crate) token: TokenIdx,
}
impl CommentStmt {
  /// The text of the comment
  #[must_use]
  pub fn text<'source>(&self, ast: &AST<'source>) -> &'source str {
    ast.get_token_text(self.token)[2..].trim()
  }

  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}

/// An expression on it's own line
#[derive(Debug)]
pub struct ExpressionStmt {
  pub(crate) expression: ExpressionIdx,
}
impl ExpressionStmt {
  /// The expression
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    self.expression(ast).span(ast)
  }
}

/// An import, e.g. `from maths import { sin, cos as cosine, tan }`
#[derive(Debug)]
pub struct Import {
  pub(crate) keyword: TokenIdx,
  pub(crate) module: Option<Variable>,
  pub(crate) start: Option<TokenIdx>,
  pub(crate) end: TokenIdx,
}
/// An item which is imported from a module
#[derive(Debug)]
pub struct ImportItem<'source> {
  /// The name of the item being imported
  pub name: &'source str,
  /// The name which is is to be bound to
  pub alias: Option<&'source str>,
  /// The location of the name
  pub span: Span,
  /// The location of the alias
  pub alias_span: Option<Span>,
}
impl Import {
  /// The name of the module being imported
  #[must_use]
  pub fn module<'source>(&self, ast: &AST<'source>) -> &'source str {
    self
      .module
      .as_ref()
      .map(|name| name.name(ast))
      .unwrap_or_default()
  }
  /// The location of the module name
  pub fn module_span(&self, ast: &AST) -> Span {
    self
      .module
      .as_ref()
      .map(|module| module.span(ast))
      .unwrap_or_default()
  }

  /// The items being imported
  pub fn items<'source, 'a>(
    &'a self,
    ast: &'a AST<'source>,
  ) -> impl Iterator<Item = ImportItem<'source>> + use<'source, 'a> {
    ImportItemIterator {
      position: self.start.unwrap_or(self.end).next(),
      end: self.end,
      ast,
    }
  }
  /// The location of the import items
  pub fn items_span(&self, ast: &AST) -> Span {
    let start = self
      .start
      .map_or(ast[self.keyword].into(), |token| Span::from(ast[token]));
    let end = Span::from(ast[self.end]);

    start.merge(end)
  }

  /// Is the import statement empty?
  pub fn is_empty(&self, ast: &AST) -> bool {
    let mut position = self.start.unwrap_or(self.end).next();
    while position < self.end {
      let token = ast[position];

      if token.kind != TokenKind::EndOfLine && token.kind != TokenKind::Comma {
        return false;
      }

      position = position.next();
    }

    true
  }

  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.keyword]).merge(ast[self.end].into())
  }
}
struct ImportItemIterator<'source, 'ast> {
  position: TokenIdx,
  end: TokenIdx,
  ast: &'ast AST<'source>,
}
impl ImportItemIterator<'_, '_> {
  fn current_kind(&self) -> TokenKind {
    if self.position <= self.end {
      self.ast[self.position].kind
    } else {
      TokenKind::EndOfFile
    }
  }

  fn matches(&mut self, kind: TokenKind) -> bool {
    if self.current_kind() == kind {
      self.next_token();
      true
    } else {
      false
    }
  }

  fn next_token(&mut self) {
    self.position = self.position.next();
  }

  fn advance(&mut self) -> Span {
    let result = if self.position <= self.end {
      Span::from(self.ast[self.position])
    } else {
      Span::default()
    };
    self.next_token();
    result
  }
}
impl<'source> Iterator for ImportItemIterator<'source, '_> {
  type Item = ImportItem<'source>;

  fn next(&mut self) -> Option<Self::Item> {
    while let TokenKind::Comma | TokenKind::EndOfLine = self.current_kind() {
      self.next_token();
    }

    if self.current_kind() == TokenKind::Identifier {
      let span = self.advance();
      let alias_span = self.matches(TokenKind::As).then(|| self.advance());

      Some(ImportItem {
        name: span.source_text(self.ast.source),
        alias: alias_span.map(|span| span.source_text(self.ast.source)),
        span,
        alias_span,
      })
    } else {
      None
    }
  }
}

/// A variable declaration, e.g. `let x = 1`
#[derive(Debug)]
pub struct Let {
  pub(crate) keyword: TokenIdx,
  pub(crate) identifier: Option<Variable>,
  pub(crate) value: ExpressionIdx,
}
impl Let {
  /// The name of the variable being declared
  #[must_use]
  pub fn identifier<'source>(&self, ast: &AST<'source>) -> &'source str {
    self
      .identifier
      .as_ref()
      .map(|name| name.name(ast))
      .unwrap_or_default()
  }
  /// The location of the variable identifier
  pub fn identifier_span(&self, ast: &AST) -> Span {
    self
      .identifier
      .as_ref()
      .map(|module| module.span(ast))
      .unwrap_or_default()
  }
  /// The expression that is to be assigned to the variable
  pub fn value<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.value]
  }

  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.keyword]).merge(self.value(ast).span(ast))
  }
}

/// A return statement, e.g. `return 1`
#[derive(Debug)]
pub struct Return {
  pub(crate) keyword: TokenIdx,
  pub(crate) expression: ExpressionIdx,
}
impl Return {
  /// The expression that is to be returned
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The location of the statement
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.keyword]).merge(self.expression(ast).span(ast))
  }
}

impl From<CommentStmt> for Statement {
  fn from(value: CommentStmt) -> Self {
    Self::Comment(value)
  }
}
impl From<ExpressionIdx> for Statement {
  fn from(expression: ExpressionIdx) -> Self {
    Self::Expression(ExpressionStmt { expression })
  }
}
impl From<Import> for Statement {
  fn from(value: Import) -> Self {
    Self::Import(value)
  }
}
impl From<Let> for Statement {
  fn from(value: Let) -> Self {
    Self::Let(value)
  }
}
impl From<Return> for Statement {
  fn from(value: Return) -> Self {
    Self::Return(value)
  }
}
