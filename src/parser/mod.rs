#[cfg(test)]
mod test;
mod tokeniser;

use crate::{
  allocator::{Allocator, Box, Vec},
  ast::{expression::*, statement::*, GetSpan, Span, AST},
};
use std::{error, fmt, iter};
use tokeniser::{Token, TokenKind, Tokeniser};

#[derive(Clone, Debug)]
pub enum ParseError {
  Expected {
    expected: TokenKind,
    recieved: Token,
  },
  ExpectedExpression(Token),
  ExpectedPattern(Token),
  UnknownCharacter(Token),
  UnterminatedString(Token),
}
impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Expected { expected, recieved } => {
        write!(f, "expected {expected} but got {}", recieved.kind)
      }
      Self::ExpectedExpression(t) => {
        write!(f, "expected expression but got {}", t.kind)
      }
      Self::ExpectedPattern(t) => {
        write!(f, "expected pattern but got {}", t.kind)
      }
      Self::UnknownCharacter(_) => write!(f, "got unknown character"),
      Self::UnterminatedString(_) => write!(f, "unterminated string"),
    }
  }
}
impl error::Error for ParseError {}

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
      TokenKind::Plus | TokenKind::Minus => Self::Term,
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Self::Factor,
      TokenKind::BangEqual | TokenKind::EqualEqual => Self::Equality,
      TokenKind::Greater | TokenKind::GreaterEqual => Self::Comparison,
      TokenKind::Less | TokenKind::LessEqual => Self::Comparison,
      TokenKind::Or => Self::Or,
      TokenKind::And => Self::And,
      TokenKind::RightRight => Self::Pipeline,
      TokenKind::Comment => Self::Comment,
      _ => Self::None,
    }
  }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'source, 'ast> {
  allocator: &'ast Allocator,

  source: &'source str,
  tokeniser: iter::Peekable<Tokeniser<'source>>,

  skipped_newline: bool,
}
impl<'s, 'ast> Parser<'s, 'ast> {
  pub fn new(source: &'s str, allocator: &'ast Allocator) -> Self {
    Self {
      allocator,

      source,
      tokeniser: Tokeniser::from(source).peekable(),

      skipped_newline: false,
    }
  }

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

  fn peek_token(&mut self) -> Token {
    self.tokeniser.peek().copied().unwrap_or_default()
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

  fn allocate_expression<T>(&mut self, x: T) -> ParseResult<Expression<'s, 'ast>>
  where
    T: 'ast,
    Box<'ast, T>: Into<Expression<'s, 'ast>>,
  {
    Ok(Box::new_in(x, self.allocator).into())
  }

  fn allocate_statement<T>(&mut self, x: T) -> ParseResult<Statement<'s, 'ast>>
  where
    T: 'ast,
    Box<'ast, T>: Into<Statement<'s, 'ast>>,
  {
    Ok(Box::new_in(x, self.allocator).into())
  }

  pub fn parse_expression(&mut self) -> ParseResult<Expression<'s, 'ast>> {
    self.parse_expression_inner(ParsePrecedence::None)
  }

  fn parse_expression_inner(
    &mut self,
    precedence: ParsePrecedence,
  ) -> ParseResult<Expression<'s, 'ast>> {
    let token = self.next_token();
    let mut previous = self.prefix_expression(token)?;

    while let Some(t) = self.tokeniser.peek()
      && precedence < ParsePrecedence::from(t.kind)
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

    if let Some(right_paren) = self.matches(TokenKind::RightParen) {
      let span = expression.span().merge(right_paren.into());
      return self.allocate_expression(Call {
        expression,
        span,
        argument: None,
      });
    }

    let argument = Some(self.parse_expression()?);
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

    let if_token_span = Span::from(if_token);
    let then = self.parse_expression()?;

    self.skip_maybe_newline();
    let (otherwise, span) = if self.matches(TokenKind::Else).is_some() {
      let expression = self.parse_expression()?;
      let span = if_token_span.merge(expression.span());

      (Some(expression), span)
    } else {
      (None, if_token_span.merge(then.span()))
    };

    self.allocate_expression(If {
      condition,
      then,
      otherwise,
      span,
    })
  }

  fn literal(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let literal = match token.kind {
      TokenKind::True | TokenKind::False => self.literal_boolean(token),
      TokenKind::Number => self.literal_number(token),
      TokenKind::String => self.literal_string(token),
      _ => unreachable!("only literal tokens are passed"),
    };

    self.allocate_expression(literal)
  }

  fn literal_boolean(&mut self, token: Token) -> Literal<'s> {
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
    let number_source = span.source_text(self.source);

    let value = if number_source.contains('_') {
      number_source.replace('_', "").parse()
    } else {
      number_source.parse()
    }
    .expect("string to be valid number representation");

    Literal {
      kind: LiteralKind::Number(value),
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

      if self.matches(TokenKind::Comma).is_some() {
        self.skip_maybe_newline();
      }
      if self.matches(TokenKind::Pipe).is_none() {
        break;
      }
    }

    let span = Span::from(token).merge(cases.last().unwrap().span());
    self.allocate_expression(Match { value, cases, span })
  }

  fn pattern(&mut self) -> ParseResult<Pattern<'s>> {
    let token = self.next_token();
    match token.kind {
      TokenKind::Identifier => Ok(Pattern::Identifier(self.variable(token))),
      TokenKind::String => Ok(Pattern::Literal(self.literal_string(token))),
      TokenKind::Number => Ok(Pattern::Literal(self.literal_number(token))),
      TokenKind::True => Ok(Pattern::Literal(self.literal_boolean(token))),
      TokenKind::False => Ok(Pattern::Literal(self.literal_boolean(token))),
      _ => Err(ParseError::ExpectedPattern(token)),
    }
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

  pub fn parse_statement(&mut self) -> ParseResult<Statement<'s, 'ast>> {
    self.skip_newline();

    match self.peek_token().kind {
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
    match self.peek_token().kind {
      TokenKind::RightCurly => {}
      _ => self.expect_newline()?,
    }

    self.allocate_statement(expression)
  }
}
