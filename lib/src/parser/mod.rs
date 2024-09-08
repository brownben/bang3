//! # Parser
//! Parse source code into an Abstract Syntax Tree
//!
//! A pratt parser, based on [Crafting Interpreters](https://craftinginterpreters.com/parsing-expressions.html).
//!
//! Tries to be error tolerant, and recover from errors to continue parsing. If a token
//! is expected but is not found it will just assume it exists and continue parsing
//! (adding an error), otherwise if an error is found it will skip to the start of a new
//! line and try parsing again.

#[cfg(test)]
mod test;
mod tokeniser;

use crate::{
  allocator::{Allocator, Box, Vec},
  ast::{expression::*, statement::*, GetSpan, Span},
};
use std::{error, fmt, iter};
use tokeniser::{Token, TokenKind, Tokeniser};

/// An Abstract Syntax Tree produced by the parser
#[derive(Debug)]
pub struct AST<'source, 'allocator> {
  pub(crate) statements: Vec<'allocator, Statement<'source, 'allocator>>,

  /// The errors found whilst parsing the abstract syntax tree
  pub errors: Vec<'allocator, ParseError>,
}
impl<'ast> AST<'_, 'ast> {
  fn new(allocator: &'ast Allocator) -> Self {
    Self {
      statements: Vec::new_in(allocator),
      errors: Vec::new_in(allocator),
    }
  }
}

/// Parse a source code string into an AST
pub struct Parser<'source, 'ast> {
  /// The bump allocator to store the AST in
  allocator: &'ast Allocator,

  /// The source code string to parse
  source: &'source str,
  /// The tokeniser for the source string
  tokeniser: iter::Peekable<Tokeniser<'source>>,

  /// If the last token was a newline, and was skipped over to continue an expression
  /// Allows for assertion of new line if it was skipped over too optimistically
  skipped_newline: bool,

  /// The AST being built up by the parser
  ast: AST<'source, 'ast>,

  /// Has an error been found, and not fully handled
  error: bool,
}
impl<'s, 'ast> Parser<'s, 'ast> {
  /// Creates a new parser
  ///
  /// For given source code string, and allocator to place the AST in
  ///
  /// # Panics
  /// If the source string length is greater than `u32::MAX`
  pub fn new(source: &'s str, allocator: &'ast Allocator) -> Self {
    Self {
      allocator,
      source,
      tokeniser: Tokeniser::from(source).peekable(),
      skipped_newline: false,
      ast: AST::new(allocator),
      error: false,
    }
  }

  /// Parse the source code into an AST
  pub fn parse(mut self) -> AST<'s, 'ast> {
    while !self.is_finished() {
      let statement = self.parse_statement();
      self.ast.statements.push(statement);
    }

    self.ast
  }

  /// Check if the parser has finished parsing the source code
  fn is_finished(&mut self) -> bool {
    self.tokeniser.peek().is_none()
  }

  /// Get the next token from the tokeniser
  fn next_token(&mut self) -> Token {
    self.skipped_newline = false;
    self.tokeniser.next().unwrap_or_default()
  }

  /// Peek at the next token from the tokeniser
  fn peek_token(&mut self) -> Token {
    self.tokeniser.peek().copied().unwrap_or_default()
  }

  /// Peek at the kind of the next token from the tokeniser
  fn peek_token_kind(&mut self) -> TokenKind {
    self
      .tokeniser
      .peek()
      .map_or(TokenKind::EndOfFile, |token| token.kind)
  }

  /// Checks if the next token is of the specified kind
  ///
  /// If it is not it adds an error, but then parsing can continue assuming it exists
  fn expect(&mut self, kind: TokenKind) -> Option<Token> {
    if self.peek_token_kind() == kind {
      Some(self.next_token())
    } else {
      let error = ParseError::Expected {
        expected: kind,
        recieved: self.peek_token(),
      };
      self.ast.errors.push(error);

      None
    }
  }

  /// Checks that a newline token is next, and if not adds an error
  fn expect_newline(&mut self) {
    if self.skipped_newline {
      return;
    }

    if matches!(
      self.peek_token_kind(),
      TokenKind::EndOfLine | TokenKind::EndOfFile
    ) {
      self.next_token();
    } else {
      let error = ParseError::Expected {
        expected: TokenKind::EndOfLine,
        recieved: self.peek_token(),
      };
      self.ast.errors.push(error);
    }
  }

  /// Skip until the specified kind of token occurs, or the end of a line is reached
  ///
  /// Used to get the parser to a known place where it can resume parsing an expression
  fn resync_to(&mut self, kind: TokenKind) {
    if self.error {
      while self.peek_token_kind() != kind
        && !matches!(
          self.peek_token_kind(),
          TokenKind::Comment | TokenKind::EndOfFile | TokenKind::EndOfLine
        )
      {
        self.next_token();
      }
    }
    self.error = false;
  }

  /// Does the next token match the specified kind
  ///
  /// If it does return that token, otherwise return None
  fn matches(&mut self, kind: TokenKind) -> Option<Token> {
    if let Some(token) = self.tokeniser.peek()
      && token.kind == kind
    {
      Some(self.next_token())
    } else {
      None
    }
  }

  /// Skip over any newline tokens
  fn skip_newline(&mut self) {
    while self
      .tokeniser
      .peek()
      .is_some_and(|t| t.kind == TokenKind::EndOfLine)
    {
      self.next_token();
      self.skipped_newline = true;
    }
  }

  /// Allocate an expression into the bump allocator for the AST
  fn allocate_expression<T>(&mut self, x: T) -> Expression<'s, 'ast>
  where
    T: 'ast,
    Box<'ast, T>: Into<Expression<'s, 'ast>>,
  {
    Box::new_in(x, self.allocator).into()
  }

  /// Allocate an statement into the bump allocator for the AST
  fn allocate_statement<T>(&mut self, x: T) -> Statement<'s, 'ast>
  where
    T: 'ast,
    Box<'ast, T>: Into<Statement<'s, 'ast>>,
  {
    Box::new_in(x, self.allocator).into()
  }

  /// Parse an expression
  fn parse_expression(&mut self) -> Expression<'s, 'ast> {
    self.parse_expression_with_precedence(ParsePrecedence::Assignment)
  }

  /// Parse an expression, also checking for a newline at the end
  fn parse_expression_with_newline(&mut self) -> Expression<'s, 'ast> {
    let mut expression = self.parse_expression();

    if self.error {
      self.resync_to(TokenKind::EndOfLine);
    }

    loop {
      self.skip_newline();
      match self.peek_token_kind() {
        TokenKind::RightCurly => break, // allow block to end without newline

        // allow pipeline operator to start after a newline - different to other operators
        TokenKind::RightRight => expression = self.pipeline(expression),
        _ => break self.expect_newline(),
      }
    }

    expression
  }

  /// Parse an expression up to a given precedence
  fn parse_expression_with_precedence(
    &mut self,
    precedence: ParsePrecedence,
  ) -> Expression<'s, 'ast> {
    let token = self.next_token();
    let mut previous = match self.prefix_expression(token) {
      Ok(expression) => expression,
      Err(error) => {
        self.error = true;
        self.ast.errors.push(error);
        return Expression::Invalid;
      }
    };

    while let Some(t) = self.tokeniser.peek()
      && precedence <= ParsePrecedence::from(t.kind)
      && !self.skipped_newline
    {
      let token = self.next_token();
      previous = self.infix_expression(previous, token);
    }

    previous
  }

  /// Parse a prefix expression
  ///
  /// An expression which starts/ makes up the first part of the expression
  fn prefix_expression(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    match token.kind {
      TokenKind::True | TokenKind::False | TokenKind::Number | TokenKind::String => {
        Ok(self.literal(token))
      }
      TokenKind::Identifier if self.matches(TokenKind::FatRightArrow).is_some() => {
        Ok(self.function(token))
      }
      TokenKind::Identifier => {
        let variable = self.variable(token);
        Ok(self.allocate_expression(variable))
      }

      TokenKind::LeftCurly => Ok(self.block(token)),
      TokenKind::LeftParen => Ok(self.group(token)),
      TokenKind::Minus | TokenKind::Bang => Ok(self.unary(token)),

      TokenKind::If => Ok(self.if_(token)),
      TokenKind::Match => self.match_(token),

      TokenKind::Unknown => Err(ParseError::UnknownCharacter(token)),
      TokenKind::UnterminatedString => Err(ParseError::UnterminatedString(token)),
      _ => Err(ParseError::ExpectedExpression(token)),
    }
  }

  /// Parse an infix expression
  ///
  /// An expression which relies on a previous expression to be complete.
  /// Passes the previously parsed expression to make up the start of the expression.
  /// It takes the relevant token as the operator, and the sub-function to parse the rest
  fn infix_expression(&mut self, lhs: Expression<'s, 'ast>, token: Token) -> Expression<'s, 'ast> {
    match token.kind {
      TokenKind::LeftParen => self.call(lhs, token),
      TokenKind::Comment => self.comment(lhs, token),
      TokenKind::Plus
      | TokenKind::Minus
      | TokenKind::Star
      | TokenKind::Slash
      | TokenKind::Percent
      | TokenKind::PlusPlus
      | TokenKind::BangEqual
      | TokenKind::EqualEqual
      | TokenKind::Greater
      | TokenKind::GreaterEqual
      | TokenKind::Less
      | TokenKind::LessEqual
      | TokenKind::And
      | TokenKind::Or
      | TokenKind::RightRight => self.binary(lhs, token),
      _ => unreachable!("only infix operators are passed"),
    }
  }

  /// Parses a binary expression (mathematical, comparison, logical, and pipeline)
  fn binary(&mut self, left: Expression<'s, 'ast>, token: Token) -> Expression<'s, 'ast> {
    let operator = match token.kind {
      TokenKind::Plus => BinaryOperator::Add,
      TokenKind::Minus => BinaryOperator::Subtract,
      TokenKind::Star => BinaryOperator::Multiply,
      TokenKind::Slash => BinaryOperator::Divide,
      TokenKind::Percent => BinaryOperator::Remainder,
      TokenKind::PlusPlus => BinaryOperator::AddString,
      TokenKind::BangEqual => BinaryOperator::NotEqual,
      TokenKind::EqualEqual => BinaryOperator::Equal,
      TokenKind::Greater => BinaryOperator::Greater,
      TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
      TokenKind::Less => BinaryOperator::Less,
      TokenKind::LessEqual => BinaryOperator::LessEqual,
      TokenKind::And => BinaryOperator::And,
      TokenKind::Or => BinaryOperator::Or,
      TokenKind::RightRight => BinaryOperator::Pipeline,
      _ => unreachable!("only binary operators are passed"),
    };

    self.skip_newline();
    let precedence = ParsePrecedence::from(token.kind);
    let right = self.parse_expression_with_precedence(precedence.next());
    let span = left.span().merge(right.span());

    self.allocate_expression(Binary {
      left,
      operator,
      right,
      span,
    })
  }

  /// Parses a pipeline expression
  ///
  /// This is an infix expression, but is the special case where the operator may be on
  ///  the next line, which is not allowed for other operators and handled in the
  ///  [`parse_expression_with_newline`] function.
  fn pipeline(&mut self, left: Expression<'s, 'ast>) -> Expression<'s, 'ast> {
    let token = self.next_token();
    let precedence = ParsePrecedence::from(token.kind);
    let right = self.parse_expression_with_precedence(precedence.next());
    let span = left.span().merge(right.span());

    self.allocate_expression(Binary {
      left,
      operator: BinaryOperator::Pipeline,
      right,
      span,
    })
  }

  /// Parses a block expression (a series of statements in curly braces)
  fn block(&mut self, opening_curly: Token) -> Expression<'s, 'ast> {
    let mut statements = Vec::with_capacity_in(4, self.allocator);
    let closing_curly = loop {
      statements.push(self.parse_statement());
      self.skip_newline();

      if let Some(closing_curly) = self.matches(TokenKind::RightCurly) {
        break Some(closing_curly);
      } else if self.peek_token_kind() == TokenKind::EndOfFile {
        self.ast.errors.push(ParseError::Expected {
          expected: TokenKind::RightCurly,
          recieved: Token::default(),
        });
        break None;
      }
    };

    let span = if let Some(closing_curly) = closing_curly {
      Span::from(opening_curly).merge(closing_curly.into())
    } else {
      Span::from(opening_curly).merge(statements.last().unwrap().span())
    };

    self.allocate_expression(Block { statements, span })
  }

  /// Parses a call expression
  fn call(
    &mut self,
    expression: Expression<'s, 'ast>,
    _left_bracket: Token,
  ) -> Expression<'s, 'ast> {
    self.skip_newline();
    let argument = if self.peek_token_kind() == TokenKind::RightParen {
      None
    } else {
      Some(self.parse_expression())
    };
    self.skip_newline();

    let span = if let Some(right_paren) = self.expect(TokenKind::RightParen) {
      expression.span().merge(right_paren.into())
    } else if let Some(argument) = &argument {
      expression.span().merge(argument.span())
    } else {
      expression.span()
    };

    self.allocate_expression(Call {
      expression,
      argument,
      span,
    })
  }

  /// Parses a comment expression
  fn comment(&mut self, expression: Expression<'s, 'ast>, comment: Token) -> Expression<'s, 'ast> {
    let comment_span = Span::from(comment);
    let text = &comment_span.source_text(self.source)[2..];
    let span = expression.span().merge(comment_span);

    self.allocate_expression(Comment {
      expression,
      text,
      span,
    })
  }

  /// Parses a function expression
  fn function(&mut self, parameter: Token) -> Expression<'s, 'ast> {
    let parameter_span = Span::from(parameter);
    let parameter = parameter_span.source_text(self.source);

    let body = self.parse_expression();
    let span = parameter_span.merge(body.span());

    self.allocate_expression(Function {
      name: None,
      parameter,
      body,
      span,
    })
  }

  /// Parses a group expression (an expression in parentheses)
  fn group(&mut self, opening_paren: Token) -> Expression<'s, 'ast> {
    self.skip_newline();
    let expression = self.parse_expression();
    self.resync_to(TokenKind::RightParen);
    self.skip_newline();

    let span = if let Some(closing_paren) = self.expect(TokenKind::RightParen) {
      Span::from(opening_paren).merge(closing_paren.into())
    } else {
      Span::from(opening_paren).merge(expression.span())
    };

    self.allocate_expression(Group { expression, span })
  }

  /// Parses an if expression
  fn if_(&mut self, if_token: Token) -> Expression<'s, 'ast> {
    self.expect(TokenKind::LeftParen);
    self.skip_newline();
    let condition = self.parse_expression();
    self.skip_newline();
    self.expect(TokenKind::RightParen);

    let then = self.parse_expression();

    self.skip_newline();
    self.expect(TokenKind::Else);
    let otherwise = self.parse_expression();

    let span = Span::from(if_token).merge(otherwise.span());

    self.allocate_expression(If {
      condition,
      then,
      otherwise,
      span,
    })
  }

  /// Parses a literal value
  fn literal(&mut self, token: Token) -> Expression<'s, 'ast> {
    let literal = match token.kind {
      TokenKind::True | TokenKind::False => Self::literal_boolean(token),
      TokenKind::Number => self.literal_number(token),
      TokenKind::String => self.literal_string(token),
      _ => unreachable!("only literal tokens are passed"),
    };

    self.allocate_expression(literal)
  }

  /// Parses a boolean literal
  fn literal_boolean(token: Token) -> Literal<'s> {
    let span = Span::from(token);
    let kind = match token.kind {
      TokenKind::True => LiteralKind::Boolean(true),
      TokenKind::False => LiteralKind::Boolean(false),
      _ => unreachable!("only literal tokens are passed"),
    };

    Literal { kind, span }
  }

  /// Parses a number literal
  fn literal_number(&mut self, token: Token) -> Literal<'s> {
    let span = Span::from(token);
    let raw = span.source_text(self.source);

    let value = if raw.contains('_') {
      raw.replace('_', "").parse()
    } else {
      raw.parse()
    }
    .expect("string to be valid number representation");

    Literal {
      kind: LiteralKind::Number { value, raw },
      span,
    }
  }

  /// Parses a string literal
  fn literal_string(&mut self, token: Token) -> Literal<'s> {
    let span = Span::from(token);
    let full_string = span.source_text(self.source);
    let string_without_quotes = &full_string[1..(full_string.len() - 1)];

    Literal {
      kind: LiteralKind::String(string_without_quotes),
      span,
    }
  }

  /// Parses a match expression
  fn match_(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let value = self.parse_expression();
    let mut cases = Vec::new_in(self.allocator);

    self.skip_newline();
    self.expect(TokenKind::Pipe);

    loop {
      let pattern = self.pattern()?;
      self.expect(TokenKind::RightArrow);
      let expression = self.parse_expression();
      let span = pattern.span().merge(expression.span());

      cases.push(MatchCase {
        pattern,
        expression,
        span,
      });

      self.skip_newline();
      if self.matches(TokenKind::Pipe).is_none() {
        break;
      }
    }

    let span = Span::from(token).merge(cases.last().unwrap().span());
    Ok(self.allocate_expression(Match { value, cases, span }))
  }

  /// Parses a pattern in a match expression
  fn pattern(&mut self) -> ParseResult<Pattern<'s, 'ast>> {
    let token = self.next_token();
    let pattern = match token.kind {
      TokenKind::String => Pattern::Literal(self.literal_string(token)),
      TokenKind::Number => Pattern::Literal(self.literal_number(token)),
      TokenKind::True | TokenKind::False => {
        return Ok(Pattern::Literal(Self::literal_boolean(token)))
      }
      TokenKind::Identifier => return Ok(Pattern::Identifier(self.variable(token))),
      TokenKind::DotDot => return self.pattern_range(None, true),
      _ => return Err(ParseError::ExpectedPattern(token)),
    };

    if self.matches(TokenKind::DotDot).is_none() {
      return Ok(pattern);
    }

    let range_has_end = self.peek_token_kind() != TokenKind::RightArrow;
    self.pattern_range(Some(pattern), range_has_end)
  }

  /// Parses a range pattern
  fn pattern_range(
    &mut self,
    start: Option<Pattern<'s, 'ast>>,
    has_end: bool,
  ) -> ParseResult<Pattern<'s, 'ast>> {
    let mut pattern = PatternRange { start, end: None };

    if has_end {
      let token = self.next_token();

      pattern.end = match token.kind {
        TokenKind::String => Some(Pattern::Literal(self.literal_string(token))),
        TokenKind::Number => Some(Pattern::Literal(self.literal_number(token))),
        _ => return Err(ParseError::ExpectedPatternRangeEnd(token)),
      }
    }

    Ok(Pattern::Range(Box::new_in(pattern, self.allocator)))
  }

  /// Parses a unary operator expression (negation, not)
  fn unary(&mut self, token: Token) -> Expression<'s, 'ast> {
    let operator = match token.kind {
      TokenKind::Bang => UnaryOperator::Not,
      TokenKind::Minus => UnaryOperator::Minus,
      _ => unreachable!("only unary operators are passed"),
    };
    let expression = self.parse_expression_with_precedence(ParsePrecedence::Unary);
    let span = Span::from(token).merge(expression.span());

    self.allocate_expression(Unary {
      operator,
      expression,
      span,
    })
  }

  /// Wraps a identifier token into a variable expression
  fn variable(&mut self, token: Token) -> Variable<'s> {
    let span = Span::from(token);

    Variable {
      name: span.source_text(self.source),
      span,
    }
  }

  /// Parses a statement
  fn parse_statement(&mut self) -> Statement<'s, 'ast> {
    self.skip_newline();

    match self.peek_token_kind() {
      TokenKind::Let => self.declaration_statement(),
      TokenKind::Comment => self.comment_statement(),
      _ => self.expression_statement(),
    }
  }

  /// Parses a comment statement (a commnent alone on a line)
  fn comment_statement(&mut self) -> Statement<'s, 'ast> {
    let span = Span::from(self.next_token());
    let text = &span.source_text(self.source)[2..];
    self.skip_newline();

    self.allocate_statement(CommentStmt { text, span })
  }

  /// Parses a let variable declaration
  fn declaration_statement(&mut self) -> Statement<'s, 'ast> {
    let let_token = self.next_token();

    let identifier = self
      .expect(TokenKind::Identifier)
      .map(|token| Span::from(token).source_text(self.source))
      .unwrap_or_default();

    self.expect(TokenKind::Equal);

    let mut expression = self.parse_expression_with_newline();
    self.resync_to(TokenKind::EndOfLine);
    self.expect_newline();

    let span = Span::from(let_token).merge(expression.span());

    if let Expression::Function(ref mut function) = &mut expression {
      function.name = Some(identifier);
    }

    self.allocate_statement(Let {
      identifier,
      expression,
      span,
    })
  }

  /// Parses an expression on its own line
  fn expression_statement(&mut self) -> Statement<'s, 'ast> {
    let expression = self.parse_expression_with_newline();
    self.resync_to(TokenKind::EndOfLine);

    self.allocate_statement(expression)
  }
}

/// An error which arose during parsing
#[derive(Clone, Copy, Debug)]
pub enum ParseError {
  /// Expected a token of a certain kind
  Expected {
    /// Expected Token Kind to be
    expected: TokenKind,
    /// Recieved this Token instead
    recieved: Token,
  },
  /// Expected Expression
  ExpectedExpression(Token),
  /// Expected Pattern
  ExpectedPattern(Token),
  /// Expected End of a Range Pattern
  ExpectedPatternRangeEnd(Token),
  /// Unknown Character
  UnknownCharacter(Token),
  /// Unterminated String Literal
  UnterminatedString(Token),
}
impl ParseError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> String {
    match self {
      Self::Expected { expected, .. } => format!("Expected {expected}"),
      Self::ExpectedExpression(_) => "Expected Expression".into(),
      Self::ExpectedPattern(_) => "Expected Pattern".into(),
      Self::ExpectedPatternRangeEnd(_) => "Expected End of Pattern Range".into(),
      Self::UnknownCharacter(_) => "Unknown Character".into(),
      Self::UnterminatedString(_) => "Unterminated String".into(),
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    match self {
      Self::Expected { expected, recieved } => {
        format!("expected {expected} but got {}", recieved.kind)
      }
      Self::ExpectedExpression(t) => {
        format!("expected expression but got {}", t.kind)
      }
      Self::ExpectedPattern(t) => {
        format!("expected pattern but got {}", t.kind)
      }
      Self::ExpectedPatternRangeEnd(t) => {
        format!("expected end of range pattern but got {}", t.kind)
      }
      Self::UnknownCharacter(_) => "got unknown character".into(),
      Self::UnterminatedString(_) => "missing closing quote for string".into(),
    }
  }

  /// The title and message of the lint in a combined string
  #[must_use]
  pub fn full_message(&self) -> String {
    let mut message = self.title();
    message.push('\n');
    message.push_str(&self.message());
    message
  }
}
impl GetSpan for ParseError {
  fn span(&self) -> Span {
    let token = match self {
      Self::Expected { recieved, .. } => recieved,
      Self::ExpectedExpression(token)
      | Self::ExpectedPattern(token)
      | Self::ExpectedPatternRangeEnd(token)
      | Self::UnknownCharacter(token)
      | Self::UnterminatedString(token) => token,
    };

    Span::from(*token)
  }
}
impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for ParseError {}

type ParseResult<T> = Result<T, ParseError>;

/// The precendence of the different binary operators
#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq)]
enum ParsePrecedence {
  None = 1,
  Assignment, // =
  Pipeline,   // >>
  Or,         // or
  And,        // and
  Nullish,    // ??
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // * /
  Unary,      // ! -
  Call,       // () []
  Primary,
  Comment,
}
impl ParsePrecedence {
  fn next(self) -> Self {
    match self {
      Self::None => Self::Assignment,
      Self::Assignment => Self::Pipeline,
      Self::Pipeline => Self::Or,
      Self::Or => Self::And,
      Self::And => Self::Nullish,
      Self::Nullish => Self::Equality,
      Self::Equality => Self::Comparison,
      Self::Comparison => Self::Term,
      Self::Term => Self::Factor,
      Self::Factor => Self::Unary,
      Self::Unary => Self::Call,
      Self::Call | Self::Primary | Self::Comment => Self::Primary,
    }
  }
}
impl From<TokenKind> for ParsePrecedence {
  fn from(kind: TokenKind) -> Self {
    match kind {
      TokenKind::LeftParen => Self::Call,
      TokenKind::Plus | TokenKind::Minus | TokenKind::PlusPlus => Self::Term,
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Self::Factor,
      TokenKind::BangEqual | TokenKind::EqualEqual => Self::Equality,
      TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => {
        Self::Comparison
      }
      TokenKind::Or => Self::Or,
      TokenKind::And => Self::And,
      TokenKind::RightRight => Self::Pipeline,
      TokenKind::Comment => Self::Comment,
      _ => Self::None,
    }
  }
}
