use crate::{
  allocator::{Allocator, Box},
  ast::{expression::*, GetSpan, Span},
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
  source: &'source str,
  tokeniser: iter::Peekable<Tokeniser<'source>>,
  allocator: &'ast Allocator,
}
impl<'s, 'ast> Parser<'s, 'ast> {
  pub fn new(source: &'s str, allocator: &'ast Allocator) -> Self {
    Self {
      source,
      tokeniser: Tokeniser::from(source).peekable(),
      allocator,
    }
  }

  pub fn parse_expression(&mut self) -> ParseResult<Expression<'s, 'ast>> {
    self.parse_expression_inner(ParsePrecedence::None)
  }

  fn next_token(&mut self) -> Token {
    self.tokeniser.next().unwrap_or_default()
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

  fn allocate_expression<T>(&mut self, x: T) -> ParseResult<Expression<'s, 'ast>>
  where
    T: 'ast,
    Box<'ast, T>: Into<Expression<'s, 'ast>>,
  {
    Ok(Box::new_in(x, self.allocator).into())
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
      TokenKind::True | TokenKind::False => self.literal_boolean(token),
      TokenKind::Number => self.literal_number(token),
      TokenKind::String => self.literal_string(token),
      TokenKind::Identifier => self.variable(token),

      TokenKind::LeftParen => self.group(token),
      TokenKind::If => self.if_(token),
      TokenKind::Minus | TokenKind::Bang => self.unary(token),

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

  fn group(&mut self, opening_brace: Token) -> ParseResult<Expression<'s, 'ast>> {
    self.skip_newline();
    let expression = self.parse_expression()?;
    self.skip_newline();
    let closing_brace = self.expect(TokenKind::RightParen)?;
    let span = Span::from(opening_brace).merge(closing_brace.into());

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

  fn literal_boolean(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let span = Span::from(token);
    let kind = match token.kind {
      TokenKind::True => LiteralKind::Boolean(true),
      TokenKind::False => LiteralKind::Boolean(false),
      _ => unreachable!("only literal tokens are passed"),
    };

    self.allocate_expression(Literal { kind, span })
  }

  fn literal_number(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let span = Span::from(token);
    let number_source = span.source_text(self.source);

    let value = if number_source.contains('_') {
      number_source.replace('_', "").parse()
    } else {
      number_source.parse()
    }
    .expect("string to be valid number representation");

    self.allocate_expression(Literal {
      kind: LiteralKind::Number(value),
      span,
    })
  }

  fn literal_string(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let span = Span::from(token);
    let full_string = span.source_text(self.source);
    let string_without_quotes = &full_string[1..(full_string.len() - 1)];

    self.allocate_expression(Literal {
      kind: LiteralKind::String(string_without_quotes),
      span,
    })
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

  fn variable(&mut self, token: Token) -> ParseResult<Expression<'s, 'ast>> {
    let span = Span::from(token);

    self.allocate_expression(Variable {
      name: span.source_text(self.source),
      span,
    })
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use indoc::indoc;

  fn parse<'s, 'ast>(
    source: &'s str,
    allocator: &'ast Allocator,
  ) -> ParseResult<Expression<'s, 'ast>> {
    Parser::new(source, allocator).parse_expression()
  }

  fn parse_to_string<'s, 'ast>(source: &'s str) -> String {
    let allocator = Allocator::new();
    let ast = Parser::new(source, &allocator).parse_expression().unwrap();
    ast.to_string()
  }

  #[test]
  fn unterminated_string() {
    let allocator = Allocator::new();
    assert!(parse("'unterminated string", &allocator).is_err());
    assert!(parse("\"un", &allocator).is_err());
    assert!(parse("`", &allocator).is_err());

    assert!(parse("``", &allocator).is_ok());
    assert!(parse("`hello world`", &allocator).is_ok());
  }

  #[test]
  fn unknown_character() {
    let allocator = Allocator::new();
    assert!(parse("¬", &allocator).is_err());
    assert!(parse("🤗", &allocator).is_err());

    // Having unknown characters in strings are fine
    assert!(parse("'¬'", &allocator).is_ok());
    assert!(parse("'🤗'", &allocator).is_ok());
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
    assert!(parse("+ 5", &allocator).is_err());
    assert!(parse("*", &allocator).is_err());
    assert!(parse(" || 7", &allocator).is_err());
  }

  #[test]
  fn binary_missing_right_expression() {
    let allocator = Allocator::new();
    assert!(parse("4 +", &allocator).is_err());
    assert!(parse("'hello' *", &allocator).is_err());
    assert!(parse("false ||", &allocator).is_err());
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
    assert!(parse("f(", &allocator).is_err());
    assert!(parse("f(6", &allocator).is_err());

    assert!(parse("f ()", &allocator).is_ok());
    assert!(parse("f()", &allocator).is_ok());
    assert!(parse("f(\n)", &allocator).is_ok());
    assert!(parse("f(\n'hello')", &allocator).is_ok());
    assert!(parse("f('hello'\n)", &allocator).is_ok());
    assert!(parse("f(\n'hello'\n)", &allocator).is_ok());
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
    assert!(parse("if 'hello')", &allocator).is_err());
    assert!(parse("if'hello')", &allocator).is_err());
    assert!(parse("if ('hello')", &allocator).is_err());
    assert!(parse("if('hello')", &allocator).is_err());
    assert!(parse("if ('hello'", &allocator).is_err());

    assert!(parse("if ('hello') then", &allocator).is_ok());
    assert!(parse("if('hello') then", &allocator).is_ok());
    assert!(parse("if(\n'hello') then", &allocator).is_ok());
    assert!(parse("if('hello'\n) then", &allocator).is_ok());
    assert!(parse("if(\n'hello'\n) then", &allocator).is_ok());
  }

  #[test]
  fn group_no_end_bracket() {
    let allocator = Allocator::new();
    assert!(parse("(4 + 5 ", &allocator).is_err());
    assert!(parse("('hello'", &allocator).is_err());
    assert!(parse("(", &allocator).is_err());

    assert!(parse("('hello')", &allocator).is_ok());
    assert!(parse("(5)", &allocator).is_ok());
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
}
