use crate::{
  allocator::{Allocator, Box, Vec},
  ast::{expression::*, statement::*, GetSpan, Span, AST},
  tokeniser::{Token, TokenKind, Tokeniser},
};
use std::{error, fmt, iter};

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

#[cfg(test)]
mod test {
  use super::*;
  use indoc::indoc;

  fn parse_expression<'a>(
    source: &'a str,
    allocator: &'a Allocator,
  ) -> ParseResult<Expression<'a, 'a>> {
    Parser::new(source, allocator).parse_expression()
  }

  fn parse_statement<'a>(source: &'a str, allocator: &'a Allocator) -> ParseResult<AST<'a, 'a>> {
    Parser::new(source, allocator).parse()
  }

  fn parse_to_string<'s, 'ast>(source: &'s str) -> String {
    let allocator = Allocator::new();
    let ast = Parser::new(source, &allocator).parse().unwrap();
    ast.to_string()
  }

  #[test]
  fn unterminated_string() {
    let allocator = Allocator::new();
    assert!(parse_expression("'unterminated string", &allocator).is_err());
    assert!(parse_expression("\"un", &allocator).is_err());
    assert!(parse_expression("`", &allocator).is_err());

    assert!(parse_expression("``", &allocator).is_ok());
    assert!(parse_expression("`hello world`", &allocator).is_ok());
  }

  #[test]
  fn unknown_character() {
    let allocator = Allocator::new();
    assert!(parse_expression("¬", &allocator).is_err());
    assert!(parse_expression("🤗", &allocator).is_err());

    // Having unknown characters in strings are fine
    assert!(parse_expression("'¬'", &allocator).is_ok());
    assert!(parse_expression("'🤗'", &allocator).is_ok());
  }

  #[test]
  fn binary() {
    let ast = parse_to_string("4 + 23");
    let expected = indoc! {"
      ├─ Binary (+)
      │  ├─ Number (4)
      │  ╰─ Number (23)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("4 * 56 >> 32");
    let expected = indoc! {"
      ├─ Binary (>>)
      │  ├─ Binary (*)
      │  │  ├─ Number (4)
      │  │  ╰─ Number (56)
      │  ╰─ Number (32)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("false or 6.12");
    let expected = indoc! {"
      ├─ Binary (or)
      │  ├─ Boolean (false)
      │  ╰─ Number (6.12)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("5 % 2 == 1 && 3 / 1");
    let expected = indoc! {"
      ├─ Binary (and)
      │  ├─ Binary (==)
      │  │  ├─ Binary (%)
      │  │  │  ├─ Number (5)
      │  │  │  ╰─ Number (2)
      │  │  ╰─ Number (1)
      │  ╰─ Binary (/)
      │     ├─ Number (3)
      │     ╰─ Number (1)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn binary_missing_left_expression() {
    let allocator = Allocator::new();
    assert!(parse_expression("+ 5", &allocator).is_err());
    assert!(parse_expression("*", &allocator).is_err());
    assert!(parse_expression(" || 7", &allocator).is_err());
  }

  #[test]
  fn binary_missing_right_expression() {
    let allocator = Allocator::new();
    assert!(parse_expression("4 +", &allocator).is_err());
    assert!(parse_expression("'hello' *", &allocator).is_err());
    assert!(parse_expression("false ||", &allocator).is_err());
  }

  #[test]
  fn blocks() {
    let ast = parse_to_string("{false}");
    let expected = indoc! {"
      ├─ Block
      │  ╰─ Boolean (false)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string(indoc! {"{
      let x = 4
      x + 2
    }"});
    let expected = indoc! {"
      ├─ Block
      │  ├─ Let 'x' =
      │  │  ╰─ Number (4)
      │  ╰─ Binary (+)
      │     ├─ Variable (x)
      │     ╰─ Number (2)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("{call()}");
    let expected = indoc! {"
      ├─ Block
      │  ├─ Variable (call)
      │  │  ╰─ Call
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn blocks_incompelete() {
    let allocator = Allocator::new();
    assert!(parse_expression("{let x = 4\nx + 2", &allocator).is_err());
    assert!(parse_expression("{}", &allocator).is_err());
    assert!(parse_expression("{let a = 1}", &allocator).is_err());
    assert!(parse_expression("{7", &allocator).is_err());
  }

  #[test]
  fn call() {
    let ast = parse_to_string("function('hello world')");
    let expected = indoc! {"
      ├─ Variable (function)
      │  ╰─ Call
      │     ╰─ String 'hello world'
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("f ( 7 ) + 5");
    let expected = indoc! {"
      ├─ Binary (+)
      │  ├─ Variable (f)
      │  │  ╰─ Call
      │  │     ╰─ Number (7)
      │  ╰─ Number (5)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("f() + 32.1");
    let expected = indoc! {"
      ├─ Binary (+)
      │  ├─ Variable (f)
      │  │  ╰─ Call
      │  ╰─ Number (32.1)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn call_missing_brackets() {
    let allocator = Allocator::new();
    assert!(parse_expression("f(", &allocator).is_err());
    assert!(parse_expression("f(6", &allocator).is_err());

    assert!(parse_expression("f ()", &allocator).is_ok());
    assert!(parse_expression("f()", &allocator).is_ok());
    assert!(parse_expression("f(\n)", &allocator).is_ok());
    assert!(parse_expression("f(\n'hello')", &allocator).is_ok());
    assert!(parse_expression("f('hello'\n)", &allocator).is_ok());
    assert!(parse_expression("f(\n'hello'\n)", &allocator).is_ok());
  }

  #[test]
  fn comment() {
    let ast = parse_to_string("5 // hello world");
    let expected = indoc! {"
      ├─ Number (5)
      │  ╰─ Comment (hello world)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn function() {
    let ast = parse_to_string("x => x + 1");
    let expected = indoc! {"
      ├─ Function: x =>
      │  ╰─ Binary (+)
      │     ├─ Variable (x)
      │     ╰─ Number (1)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("let plusOne = a => { a + 1 }");
    let expected = indoc! {"
      ├─ Let 'plusOne' =
      │  ╰─ Function: a =>
      │     ╰─ Block
      │        ╰─ Binary (+)
      │           ├─ Variable (a)
      │           ╰─ Number (1)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn group() {
    let ast = parse_to_string("('hello world')");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ String 'hello world'
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("(4)");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Number (4)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn if_() {
    let ast = parse_to_string("if (true) 7 else 5");
    let expected = indoc! {"
      ├─ If
      │  ├─ Condition
      │  │  ╰─ Boolean (true)
      │  ├─ Then
      │  │  ╰─ Number (7)
      │  ╰─ Otherwise
      │     ╰─ Number (5)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("if (7 > x) doStuff");
    let expected = indoc! {"
      ├─ If
      │  ├─ Condition
      │  │  ╰─ Binary (>)
      │  │     ├─ Number (7)
      │  │     ╰─ Variable (x)
      │  ╰─ Then
      │     ╰─ Variable (doStuff)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn if_missing_parts() {
    let allocator = Allocator::new();
    assert!(parse_expression("if 'hello')", &allocator).is_err());
    assert!(parse_expression("if'hello')", &allocator).is_err());
    assert!(parse_expression("if ('hello')", &allocator).is_err());
    assert!(parse_expression("if('hello')", &allocator).is_err());
    assert!(parse_expression("if ('hello'", &allocator).is_err());

    assert!(parse_expression("if ('hello') then", &allocator).is_ok());
    assert!(parse_expression("if('hello') then", &allocator).is_ok());
    assert!(parse_expression("if(\n'hello') then", &allocator).is_ok());
    assert!(parse_expression("if('hello'\n) then", &allocator).is_ok());
    assert!(parse_expression("if(\n'hello'\n) then", &allocator).is_ok());
  }

  #[test]
  fn group_no_end_bracket() {
    let allocator = Allocator::new();
    assert!(parse_expression("(4 + 5 ", &allocator).is_err());
    assert!(parse_expression("('hello'", &allocator).is_err());
    assert!(parse_expression("(", &allocator).is_err());

    assert!(parse_expression("('hello')", &allocator).is_ok());
    assert!(parse_expression("(5)", &allocator).is_ok());
  }

  #[test]
  fn literal() {
    let ast = parse_to_string("false");
    let expected = "├─ Boolean (false)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("true");
    let expected = "├─ Boolean (true)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("5.6");
    let expected = "├─ Number (5.6)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("5_000.03");
    let expected = "├─ Number (5000.03)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("'string'");
    let expected = "├─ String 'string'\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("'string\nnew\nlines'");
    let expected = "├─ String 'string\nnew\nlines'\n";
    assert_eq!(ast, expected);
  }

  #[test]
  fn match_() {
    let ast = parse_to_string("match n | 1 -> 0");
    let expected = indoc! {"
      ├─ Match
      │  ├─ Variable (n)
      │  ╰─ Cases:
      │     ╰─ Pattern ─ Literal (1)
      │        ╰─ Number (0)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string(indoc! {"
        match n
          | 1 -> 0,
          | 2 -> 1,
          | n -> n * 5,
    "});
    let expected = indoc! {"
      ├─ Match
      │  ├─ Variable (n)
      │  ╰─ Cases:
      │     ├─ Pattern ─ Literal (1)
      │     │  ╰─ Number (0)
      │     ├─ Pattern ─ Literal (2)
      │     │  ╰─ Number (1)
      │     ╰─ Pattern ─ Identifier 'n'
      │        ╰─ Binary (*)
      │           ├─ Variable (n)
      │           ╰─ Number (5)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn match_missing_parts() {
    let allocator = Allocator::new();
    assert!(parse_statement("match | 1 -> 2", &allocator).is_err());
    assert!(parse_statement("match 3 | -5", &allocator).is_err());
    assert!(parse_statement("match 3 1 -> 3", &allocator).is_err());
    assert!(parse_statement("match 3 | 1 -> 3, 2 -> 4", &allocator).is_err());
    assert!(parse_statement("match 3 | 1 -> 3\n 2 -> 4", &allocator).is_err());
    assert!(parse_statement("match 3 | 1,", &allocator).is_err());
  }

  #[test]
  fn unary() {
    let ast = parse_to_string("!false");
    let expected = indoc! {"
      ├─ Unary (!)
      │  ╰─ Boolean (false)
    "};
    assert_eq!(ast, expected);

    let ast = parse_to_string("-.5");
    let expected = indoc! {"
      ├─ Unary (-)
      │  ╰─ Number (0.5)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn variable() {
    let ast = parse_to_string("a_long_variable");
    let expected = "├─ Variable (a_long_variable)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("aLongVariable");
    let expected = "├─ Variable (aLongVariable)\n";
    assert_eq!(ast, expected);

    let ast = parse_to_string("count");
    let expected = "├─ Variable (count)\n";
    assert_eq!(ast, expected);
  }

  #[test]
  fn comment_statement() {
    let ast = parse_to_string("// comments");
    let expected = "├─ Comment (comments)\n";
    assert_eq!(ast, expected);
  }

  #[test]
  fn let_statement() {
    let ast = parse_to_string("let variable = 4 + 33");
    let expected = indoc! {"
      ├─ Let 'variable' =
      │  ╰─ Binary (+)
      │     ├─ Number (4)
      │     ╰─ Number (33)
    "};
    assert_eq!(ast, expected);
  }

  #[test]
  fn let_statement_missing_parts() {
    let allocator = Allocator::new();
    assert!(parse_statement("let = 7", &allocator).is_err());
    assert!(parse_statement("let var", &allocator).is_err());
    assert!(parse_statement("let var =", &allocator).is_err());
    assert!(parse_statement("let var 7", &allocator).is_err());
    assert!(parse_statement("let false = 7", &allocator).is_err());

    assert!(parse_statement("let var = 7", &allocator).is_ok());
  }
}
