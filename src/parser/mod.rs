//! # Parser
//! Parse source code into an AST

#[cfg(test)]
mod test;
mod tokeniser;

use crate::{
  allocator::{Allocator, Box, Vec},
  ast::{expression::*, statement::*, GetSpan, Span, AST},
};
use std::{error, fmt, iter};
use tokeniser::{Token, TokenKind, Tokeniser};

/// Parse a source code string into an AST
pub struct Parser<'source, 'ast> {
  allocator: &'ast Allocator,

  source: &'source str,
  tokeniser: iter::Peekable<Tokeniser<'source>>,

  skipped_newline: bool,
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
    }
  }

  /// Parse the source code into an AST
  pub fn parse(mut self) -> ParseResult<AST<'s, 'ast>> {
    let mut statements = Vec::new_in(self.allocator);
    while !self.is_finished() {
      statements.push(self.parse_statement()?);
    }

    Ok(AST { statements })
  }

  fn is_finished(&mut self) -> bool {
    self.tokeniser.peek().is_none()
  }

  fn next_token(&mut self) -> Token {
    self.skipped_newline = false;
    self.tokeniser.next().unwrap_or_default()
  }

  fn peek_token(&mut self) -> TokenKind {
    self
      .tokeniser
      .peek()
      .map(|token| token.kind)
      .unwrap_or_default()
  }

  fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
    let token = self.next_token();

    if token.kind == kind {
      Ok(token)
    } else {
      Err(ParseError::Expected {
        expected: kind,
        recieved: token,
      })
    }
  }

  fn expect_newline(&mut self) -> ParseResult<()> {
    if self.skipped_newline {
      return Ok(());
    }

    let token = self.next_token();

    if matches!(
      token.kind,
      TokenKind::EndOfLine | TokenKind::EndOfFile | TokenKind::Unknown
    ) {
      Ok(())
    } else {
      Err(ParseError::Expected {
        expected: TokenKind::EndOfLine,
        recieved: token,
      })
    }
  }

  fn matches(&mut self, kind: TokenKind) -> Option<Token> {
    if let Some(token) = self.tokeniser.peek()
      && token.kind == kind
    {
      Some(self.next_token())
    } else {
      None
    }
  }

  fn skip_newline(&mut self) {
    while self
      .tokeniser
      .peek()
      .is_some_and(|t| t.kind == TokenKind::EndOfLine)
    {
      self.next_token();
    }
  }

  fn skip_maybe_newline(&mut self) {
    while self
      .tokeniser
      .peek()
      .is_some_and(|t| t.kind == TokenKind::EndOfLine)
    {
      self.next_token();
      self.skipped_newline = true;
    }
  }

  #[allow(clippy::unnecessary_wraps, reason = "is always wrapped at call site")]
  fn allocate_expression<T>(&mut self, x: T) -> ParseResult<Expression<'s, 'ast>>
  where
    T: 'ast,
    Box<'ast, T>: Into<Expression<'s, 'ast>>,
  {
    Ok(Box::new_in(x, self.allocator).into())
  }

  #[allow(clippy::unnecessary_wraps, reason = "is always wrapped at call site")]
  fn allocate_statement<T>(&mut self, x: T) -> ParseResult<Statement<'s, 'ast>>
  where
    T: 'ast,
    Box<'ast, T>: Into<Statement<'s, 'ast>>,
  {
    Ok(Box::new_in(x, self.allocator).into())
  }

  fn parse_expression(&mut self) -> ParseResult<Expression<'s, 'ast>> {
    self.parse_expression_inner(ParsePrecedence::Assignment)
  }

  fn parse_expression_inner(
    &mut self,
    precedence: ParsePrecedence,
  ) -> ParseResult<Expression<'s, 'ast>> {
    let token = self.next_token();
    let mut previous = self.prefix_expression(token)?;

    while let Some(t) = self.tokeniser.peek()
      && precedence <= ParsePrecedence::from(t.kind)
      && !self.skipped_newline
    {
      let token = self.next_token();
      previous = self.infix_expression(previous, token)?;
    }

    Ok(previous)
  }

  fn prefix_expression(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    match token.kind {
      TokenKind::True | TokenKind::False | TokenKind::Number | TokenKind::String => {
        self.literal(token)
      }
      TokenKind::Identifier if self.matches(TokenKind::FatRightArrow).is_some() => {
        self.function(token)
      }
      TokenKind::Identifier => {
        let variable = self.variable(token);
        self.allocate_expression(variable)
      }

      TokenKind::LeftCurly => self.block(token),
      TokenKind::LeftParen => self.group(token),
      TokenKind::Minus | TokenKind::Bang => self.unary(token),

      TokenKind::If => self.if_(token),
      TokenKind::Match => self.match_(token),

      TokenKind::Unknown => Err(ParseError::UnknownCharacter(token)),
      TokenKind::UnterminatedString => Err(ParseError::UnterminatedString(token)),
      _ => Err(ParseError::ExpectedExpression(token)),
    }
  }

  fn infix_expression(
    &mut self,
    lhs: Expression<'s, 'ast>,
    token: Token,
  ) -> ParseResult<Expression<'s, 'ast>> {
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

  fn binary(
    &mut self,
    left: Expression<'s, 'ast>,
    token: Token,
  ) -> ParseResult<Expression<'s, 'ast>> {
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
    let right = self.parse_expression_inner(precedence.next())?;
    let span = left.span().merge(right.span());

    self.allocate_expression(Binary {
      left,
      operator,
      right,
      span,
    })
  }

  fn block(&mut self, opening_curly: Token) -> ParseResult<Expression<'s, 'ast>> {
    let mut statements = Vec::with_capacity_in(4, self.allocator);
    let closing_curly = loop {
      statements.push(self.parse_statement()?);
      self.skip_newline();

      if let Some(closing_curly) = self.matches(TokenKind::RightCurly) {
        break closing_curly;
      }
    };
    let span = Span::from(opening_curly).merge(closing_curly.into());

    self.allocate_expression(Block { statements, span })
  }

  fn call(
    &mut self,
    expression: Expression<'s, 'ast>,
    _left_bracket: Token,
  ) -> ParseResult<Expression<'s, 'ast>> {
    self.skip_newline();
    let argument = self.parse_expression()?;
    self.skip_newline();
    let right_paren = self.expect(TokenKind::RightParen)?;
    let span = expression.span().merge(right_paren.into());

    self.allocate_expression(Call {
      expression,
      argument,
      span,
    })
  }

  fn comment(
    &mut self,
    expression: Expression<'s, 'ast>,
    comment: Token,
  ) -> ParseResult<Expression<'s, 'ast>> {
    let comment_span = Span::from(comment);
    let text = &comment_span.source_text(self.source)[2..];
    let span = expression.span().merge(comment_span);

    self.allocate_expression(Comment {
      expression,
      text,
      span,
    })
  }

  fn function(&mut self, parameter: Token) -> ParseResult<Expression<'s, 'ast>> {
    let parameter_span = Span::from(parameter);
    let parameter = parameter_span.source_text(self.source);

    let body = self.parse_expression()?;
    let span = parameter_span.merge(body.span());

    self.allocate_expression(Function {
      parameter,
      body,
      span,
    })
  }

  fn group(&mut self, opening_paren: Token) -> ParseResult<Expression<'s, 'ast>> {
    self.skip_newline();
    let expression = self.parse_expression()?;
    self.skip_newline();
    let closing_paren = self.expect(TokenKind::RightParen)?;
    let span = Span::from(opening_paren).merge(closing_paren.into());

    self.allocate_expression(Group { expression, span })
  }

  fn if_(&mut self, if_token: Token) -> ParseResult<Expression<'s, 'ast>> {
    self.expect(TokenKind::LeftParen)?;
    self.skip_newline();
    let condition = self.parse_expression()?;
    self.skip_newline();
    self.expect(TokenKind::RightParen)?;

    let then = self.parse_expression()?;

    self.skip_maybe_newline();
    self.expect(TokenKind::Else)?;
    let otherwise = self.parse_expression()?;

    let span = Span::from(if_token).merge(otherwise.span());

    self.allocate_expression(If {
      condition,
      then,
      otherwise,
      span,
    })
  }

  fn literal(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let literal = match token.kind {
      TokenKind::True | TokenKind::False => Self::literal_boolean(token),
      TokenKind::Number => self.literal_number(token),
      TokenKind::String => self.literal_string(token),
      _ => unreachable!("only literal tokens are passed"),
    };

    self.allocate_expression(literal)
  }

  fn literal_boolean(token: Token) -> Literal<'s> {
    let span = Span::from(token);
    let kind = match token.kind {
      TokenKind::True => LiteralKind::Boolean(true),
      TokenKind::False => LiteralKind::Boolean(false),
      _ => unreachable!("only literal tokens are passed"),
    };

    Literal { kind, span }
  }

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

  fn literal_string(&mut self, token: Token) -> Literal<'s> {
    let span = Span::from(token);
    let full_string = span.source_text(self.source);
    let string_without_quotes = &full_string[1..(full_string.len() - 1)];

    Literal {
      kind: LiteralKind::String(string_without_quotes),
      span,
    }
  }

  fn match_(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let value = self.parse_expression()?;
    let mut cases = Vec::new_in(self.allocator);

    self.skip_newline();
    self.expect(TokenKind::Pipe)?;

    loop {
      let pattern = self.pattern()?;
      self.expect(TokenKind::RightArrow)?;
      let expression = self.parse_expression()?;
      let span = pattern.span().merge(expression.span());

      cases.push(MatchCase {
        pattern,
        expression,
        span,
      });

      self.skip_maybe_newline();
      if self.matches(TokenKind::Pipe).is_none() {
        break;
      }
    }

    let span = Span::from(token).merge(cases.last().unwrap().span());
    self.allocate_expression(Match { value, cases, span })
  }

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

    let range_has_end = self.peek_token() != TokenKind::RightArrow;
    self.pattern_range(Some(pattern), range_has_end)
  }

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

  fn unary(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let operator = match token.kind {
      TokenKind::Bang => UnaryOperator::Not,
      TokenKind::Minus => UnaryOperator::Minus,
      _ => unreachable!("only unary operators are passed"),
    };
    let expression = self.parse_expression_inner(ParsePrecedence::Unary)?;
    let span = Span::from(token).merge(expression.span());

    self.allocate_expression(Unary {
      operator,
      expression,
      span,
    })
  }

  fn variable(&mut self, token: Token) -> Variable<'s> {
    let span = Span::from(token);

    Variable {
      name: span.source_text(self.source),
      span,
    }
  }

  fn parse_statement(&mut self) -> ParseResult<Statement<'s, 'ast>> {
    self.skip_newline();

    match self.peek_token() {
      TokenKind::Let => self.declaration_statement(),
      TokenKind::Comment => self.comment_statement(),
      _ => self.expression_statement(),
    }
  }

  fn comment_statement(&mut self) -> ParseResult<Statement<'s, 'ast>> {
    let span = Span::from(self.next_token());
    let text = &span.source_text(self.source)[2..];
    self.expect_newline()?;

    self.allocate_statement(CommentStmt { text, span })
  }

  fn declaration_statement(&mut self) -> ParseResult<Statement<'s, 'ast>> {
    let let_token = self.expect(TokenKind::Let)?;
    let identifier_token = self.expect(TokenKind::Identifier)?;
    self.expect(TokenKind::Equal)?;
    let expression = self.parse_expression()?;
    self.expect_newline()?;

    let identifier = Span::from(identifier_token).source_text(self.source);
    let span = Span::from(let_token).merge(expression.span());

    self.allocate_statement(Let {
      identifier,
      expression,
      span,
    })
  }

  fn expression_statement(&mut self) -> ParseResult<Statement<'s, 'ast>> {
    let expression = self.parse_expression()?;

    // Blocks should end with an expression, thus allow block end instead of new line
    match self.peek_token() {
      TokenKind::RightCurly => {}
      _ => self.expect_newline()?,
    }

    self.allocate_statement(expression)
  }
}

/// An error from parsing the source code
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
