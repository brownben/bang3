use crate::{
  ast::{
    AST, ExpressionIdx, StatementIdx, TokenIdx, TypeIdx, expression::*, statement::*, types::*,
  },
  span::Span,
  tokeniser::{Token, TokenKind},
};
use std::{error, fmt};
use thin_vec::ThinVec;

pub struct Parser<'ast> {
  /// The AST being built up by the parser
  ast: &'ast mut AST,

  /// The current token
  position: usize,
  /// If the last token was a newline, and was skipped over to continue an expression
  /// Allows for assertion of new line if it was skipped over too optimistically
  skipped_newline: bool,
  /// How many functions are we currently in
  function_depth: usize,
  /// Have we encountered an error, and want to resync at the next chance?
  should_resync: bool,
  /// Has the first part of a `>>` token been used by a type?
  type_parameter_close: bool,
}
impl<'ast> Parser<'ast> {
  pub fn new(ast: &'ast mut AST) -> Self {
    Self {
      ast,
      position: 0,
      skipped_newline: false,
      function_depth: 0,
      should_resync: false,
      type_parameter_close: false,
    }
  }

  pub fn parse(mut self) {
    self.skip_newline();

    while !self.is_finished() {
      let statement = self.parse_statement();
      self.ast.root_statements.push(statement);
      self.skip_newline();
    }
  }

  fn add_error(&mut self, error: ParseError) {
    self.ast.errors.push(error);
  }

  fn is_finished(&mut self) -> bool {
    self.current_kind() == TokenKind::EndOfFile
  }

  fn current_token_id(&self) -> TokenIdx {
    if self.position < self.ast.tokens.len() {
      TokenIdx::from(self.position)
    } else {
      TokenIdx::from(self.ast.tokens.len() - 1)
    }
  }

  fn current_token(&self) -> Token {
    self.ast[self.current_token_id()]
  }

  fn current_kind(&self) -> TokenKind {
    if self.position < self.ast.tokens.len() {
      self.ast.tokens[self.position].kind
    } else {
      TokenKind::EndOfFile
    }
  }

  #[must_use]
  fn advance(&mut self) -> (TokenKind, TokenIdx) {
    let result = (self.current_kind(), self.current_token_id());
    self.skipped_newline = false;
    self.position += 1;
    result
  }

  fn skip_newline(&mut self) {
    while self.current_kind() == TokenKind::EndOfLine {
      self.position += 1;
      self.skipped_newline = true;
    }
  }

  fn skip_single_newline(&mut self) {
    if self.current_kind() == TokenKind::EndOfLine {
      self.position += 1;
      self.skipped_newline = true;
    }
  }

  fn expect(&mut self, kind: TokenKind) -> Option<TokenIdx> {
    if self.current_kind() == kind {
      Some(self.advance().1)
    } else {
      self.add_error(ParseError::Expected {
        expected: kind,
        recieved: self.current_token(),
      });

      None
    }
  }

  fn matches(&mut self, kind: TokenKind) -> bool {
    if self.current_kind() == kind {
      self.position += 1;
      true
    } else {
      false
    }
  }

  fn expect_newline(&mut self) {
    if self.skipped_newline {
      return;
    }

    match self.current_kind() {
      TokenKind::EndOfLine => self.position += 1,
      TokenKind::EndOfFile | TokenKind::Unknown => {}
      _ => self.add_error(ParseError::Expected {
        expected: TokenKind::EndOfLine,
        recieved: self.current_token(),
      }),
    }
  }

  fn resync_if_error(&mut self, kind: TokenKind) {
    if self.should_resync {
      while self.current_kind() != kind
        && !matches!(
          self.current_kind(),
          TokenKind::EndOfFile | TokenKind::EndOfLine
        )
      {
        self.position += 1;
      }
    }
    self.should_resync = false;
  }

  fn parse_expression(&mut self) -> ExpressionIdx {
    self.parse_expression_with_precedence(ParsePrecedence::LOWEST)
  }

  fn parse_expression_with_newline(&mut self) -> ExpressionIdx {
    let expression = self.parse_expression_with_precedence(ParsePrecedence::LOWEST);
    self.resync_if_error(TokenKind::EndOfLine);

    self.skip_newline();
    match self.current_kind() {
      // allow block to end without newline
      TokenKind::RightCurly => {}
      // else check for a new-line
      _ => self.expect_newline(),
    }

    expression
  }

  fn parse_expression_with_precedence(&mut self, precedence: ParsePrecedence) -> ExpressionIdx {
    let (kind, token) = self.advance();
    let mut previous = match self.prefix_expression(kind, token) {
      Ok(expression) => expression,
      Err(error) => {
        self.add_error(error);
        self.should_resync = true;
        return self.ast.add_expression(Invalid { token });
      }
    };

    self.skip_single_newline();
    while precedence <= ParsePrecedence::from(self.current_kind())
      && (!self.skipped_newline || self.current_kind() == TokenKind::RightRight)
    {
      let (kind, token) = self.advance();
      previous = self.infix_expression(previous, kind, token);
    }

    previous
  }

  /// Parse a prefix expression
  ///
  /// An expression which starts/ makes up the first part of the expression
  fn prefix_expression(
    &mut self,
    token_kind: TokenKind,
    token: TokenIdx,
  ) -> Result<ExpressionIdx, ParseError> {
    match token_kind {
      TokenKind::True | TokenKind::False | TokenKind::Number | TokenKind::String => {
        Ok(self.ast.add_expression(Literal { token }))
      }
      TokenKind::FormatStringStart => Ok(self.format_string(token)),

      TokenKind::Identifier if self.matches(TokenKind::FatRightArrow) => Ok(self.function(token)),
      TokenKind::Identifier if self.matches(TokenKind::ColonColon) => Ok(self.module_access(token)),
      TokenKind::Identifier => Ok(self.ast.add_expression(Variable { token })),

      TokenKind::LeftCurly => Ok(self.block(token)),
      TokenKind::LeftParen => Ok(self.group(token)),
      TokenKind::LeftSquare => Ok(self.list(token)),
      TokenKind::Minus | TokenKind::Bang => Ok(self.unary(token)),

      TokenKind::If => Ok(self.if_(token)),
      TokenKind::Match => Ok(self.match_(token)),

      TokenKind::Unknown => Err(ParseError::UnknownCharacter(self.ast[token])),
      TokenKind::UnterminatedString => Err(ParseError::UnterminatedString(self.ast[token])),
      _ => Err(ParseError::ExpectedExpression(self.ast[token])),
    }
  }

  /// Parse an infix expression
  ///
  /// An expression which relies on a previous expression to be complete.
  /// Passes the previously parsed expression to make up the start of the expression.
  /// It takes the relevant token as the operator, and the sub-function to parse the rest
  fn infix_expression(
    &mut self,
    lhs: ExpressionIdx,
    kind: TokenKind,
    operator: TokenIdx,
  ) -> ExpressionIdx {
    match kind {
      TokenKind::LeftParen => self.call(lhs, operator),
      TokenKind::Comment => self.comment(lhs, operator),
      _ => self.binary(lhs, kind, operator),
    }
  }

  fn possibly_missing_identifier(&mut self, expected_next: TokenKind) -> Option<Variable> {
    if self.current_kind() == expected_next {
      self.add_error(ParseError::MissingIdentifier(self.current_token()));
      return None;
    }

    let (kind, token) = self.advance();
    if kind != TokenKind::Identifier {
      self.add_error(ParseError::Expected {
        expected: TokenKind::Identifier,
        recieved: self.ast[token],
      });
    };

    Some(Variable { token })
  }

  fn possibly_missing_module_name(&mut self) -> Option<Variable> {
    if self.current_kind() == TokenKind::Import {
      self.add_error(ParseError::MissingModuleName(self.current_token()));
      return None;
    }

    let (kind, token) = self.advance();
    if kind != TokenKind::Identifier {
      self.add_error(ParseError::Expected {
        expected: TokenKind::Identifier,
        recieved: self.ast[token],
      });
    };

    Some(Variable { token })
  }
}
// Expressions
impl Parser<'_> {
  fn binary(&mut self, left: ExpressionIdx, kind: TokenKind, operator: TokenIdx) -> ExpressionIdx {
    if kind == TokenKind::Equal {
      let possible_assignment = matches!(self.ast[left], Expression::Variable(_));
      self.add_error(ParseError::NoSingleEqualOperator {
        token: self.ast[operator],
        possible_assignment,
      });
    }

    self.skip_newline();
    let right = self.parse_expression_with_precedence(ParsePrecedence::from(kind).next());

    self.ast.add_expression(Binary {
      left,
      operator,
      right,
    })
  }

  fn block(&mut self, opening: TokenIdx) -> ExpressionIdx {
    let mut statements = ThinVec::new();

    let mut ends_with_expression = false;
    loop {
      self.skip_newline();
      let statement = self.parse_statement();

      // check that the last statement is an expression
      match &statement {
        Statement::Comment(_) => {}
        Statement::Expression(_) | Statement::Return(_) => ends_with_expression = true,
        Statement::Import(_) | Statement::Let(_) => ends_with_expression = false,
      };

      statements.push(self.ast.add_statement(statement));
      self.skip_newline();

      if let TokenKind::RightCurly | TokenKind::EndOfFile = self.current_kind() {
        break;
      }
    }
    let closing = self.expect(TokenKind::RightCurly);

    if !ends_with_expression {
      self.add_error(ParseError::BlockMustEndWithExpression(
        self.ast[closing.unwrap_or(opening)],
      ));
    }

    self.ast.add_expression(Block {
      opening,
      statements,
      closing,
    })
  }

  fn call(&mut self, callee: ExpressionIdx, opening: TokenIdx) -> ExpressionIdx {
    self.skip_newline();
    let argument = (self.current_kind() != TokenKind::RightParen).then(|| self.parse_expression());
    self.skip_newline();
    let closing = self.expect(TokenKind::RightParen);

    self.ast.add_expression(Call {
      callee,
      opening,
      argument,
      closing,
    })
  }

  fn comment(&mut self, expression: ExpressionIdx, comment: TokenIdx) -> ExpressionIdx {
    self.ast.add_expression(Comment {
      expression,
      comment,
    })
  }

  fn format_string(&mut self, first_string_part: TokenIdx) -> ExpressionIdx {
    let mut strings = ThinVec::new();
    let mut expressions = ThinVec::new();

    strings.push(first_string_part);

    loop {
      expressions.push(self.parse_expression());

      match self.current_kind() {
        TokenKind::FormatStringPart => {
          let (_, token) = self.advance();
          strings.push(token);
        }
        TokenKind::FormatStringEnd => {
          let (_, token) = self.advance();
          strings.push(token);
          break;
        }
        TokenKind::FormatStringUnterminated => {
          let (_, token) = self.advance();
          strings.push(token);
          break self.add_error(ParseError::UnterminatedString(self.ast[token]));
        }
        _ => {
          break self.add_error(ParseError::Expected {
            expected: TokenKind::FormatStringPart,
            recieved: self.current_token(),
          });
        }
      }
    }

    self.ast.add_expression(FormatString {
      strings,
      expressions,
    })
  }

  fn function(&mut self, parameter: TokenIdx) -> ExpressionIdx {
    self.function_depth += 1;
    let expression = self.parse_expression_with_precedence(ParsePrecedence::Function);
    self.function_depth -= 1;

    self.ast.add_expression(Function {
      name: None,
      parameter: Variable { token: parameter },
      body: expression,
    })
  }

  fn group(&mut self, start: TokenIdx) -> ExpressionIdx {
    self.skip_newline();

    if self.current_kind() == TokenKind::RightParen {
      return self.empty_group(start);
    }

    let expression = self.parse_expression();
    self.skip_newline();
    self.resync_if_error(TokenKind::RightParen);
    let end = self.expect(TokenKind::RightParen);

    self.ast.add_expression(Group {
      start,
      expression,
      end,
    })
  }

  fn empty_group(&mut self, start: TokenIdx) -> ExpressionIdx {
    let (_, end) = self.advance();
    let expression = self.ast.add_expression(Invalid { token: end });
    self.add_error(ParseError::ExpectedExpression(self.ast[end]));

    self.ast.add_expression(Group {
      start,
      expression,
      end: Some(end),
    })
  }

  fn if_(&mut self, keyword: TokenIdx) -> ExpressionIdx {
    self.expect(TokenKind::LeftParen);
    self.skip_newline();
    let condition = self.parse_expression();
    self.skip_newline();
    self.expect(TokenKind::RightParen);

    self.skip_newline();
    let then = self.parse_expression();
    self.skip_newline();

    let otherwise = if self.matches(TokenKind::Else) {
      Some(self.parse_expression())
    } else {
      None
    };

    self.ast.add_expression(If {
      keyword,
      condition,
      then,
      otherwise,
    })
  }

  fn list(&mut self, start: TokenIdx) -> ExpressionIdx {
    self.skip_newline();
    let mut items = ThinVec::new();

    loop {
      self.skip_newline();

      if self.is_finished() || self.current_kind() == TokenKind::RightSquare {
        break;
      }

      items.push(self.parse_expression());

      match self.current_kind() {
        TokenKind::RightSquare | TokenKind::EndOfFile => break,
        TokenKind::Comma => _ = self.advance(),
        _ => {}
      }
    }
    let end = self.expect(TokenKind::RightSquare);

    self.ast.add_expression(List { start, items, end })
  }

  fn match_(&mut self, keyword: TokenIdx) -> ExpressionIdx {
    let value = self.parse_expression();
    let mut arms = ThinVec::new();

    self.skip_newline();
    self.expect(TokenKind::Pipe);

    loop {
      let pattern = self.pattern();
      let guard = self.matches(TokenKind::If).then(|| self.parse_expression());
      self.expect(TokenKind::RightArrow);
      let expression = self.parse_expression();

      arms.push(MatchArm {
        pattern,
        guard,
        expression,
      });

      self.skip_newline();
      if !self.matches(TokenKind::Pipe) {
        break;
      }
    }

    self.ast.add_expression(Match {
      keyword,
      value,
      arms,
    })
  }

  fn module_access(&mut self, module: TokenIdx) -> ExpressionIdx {
    let item = if self.current_kind().is_keyword() {
      let (_, token) = self.advance();
      Some(token)
    } else {
      self.expect(TokenKind::Identifier)
    };

    self.ast.add_expression(ModuleAccess { module, item })
  }

  fn pattern(&mut self) -> Pattern {
    if self.current_kind() == TokenKind::RightArrow {
      self.add_error(ParseError::MissingPattern(self.current_token()));
      return Pattern::Invalid;
    }

    let (kind, token) = self.advance();
    let start = match kind {
      TokenKind::String | TokenKind::Number => Some(Literal { token }),
      TokenKind::True | TokenKind::False => return Pattern::Literal(Literal { token }),
      TokenKind::Identifier => return Pattern::Identifier(Variable { token }),
      TokenKind::DotDot => return self.pattern_range(None, token),
      _ => {
        self.add_error(ParseError::ExpectedPattern(self.ast[token]));
        return Pattern::Invalid;
      }
    };

    if self.current_kind() == TokenKind::DotDot {
      let (_, dots) = self.advance();

      if self.current_kind() == TokenKind::RightArrow || self.current_kind() == TokenKind::If {
        Pattern::Range(PatternRange::new(start, dots, None))
      } else {
        self.pattern_range(start, dots)
      }
    } else {
      Pattern::Literal(start.unwrap())
    }
  }

  fn pattern_range(&mut self, start: Option<Literal>, dots: TokenIdx) -> Pattern {
    match self.current_kind() {
      TokenKind::String | TokenKind::Number => {
        let (_, token) = self.advance();
        Pattern::Range(PatternRange::new(start, dots, Some(Literal { token })))
      }
      TokenKind::RightArrow => {
        self.add_error(ParseError::ExpectedPatternRangeEnd(self.current_token()));
        Pattern::Invalid
      }
      _ => {
        let (_, token) = self.advance();
        self.add_error(ParseError::ExpectedPatternRangeEnd(self.ast[token]));
        Pattern::Invalid
      }
    }
  }

  fn unary(&mut self, operator: TokenIdx) -> ExpressionIdx {
    let expression = self.parse_expression_with_precedence(ParsePrecedence::Unary);

    self.ast.add_expression(Unary {
      operator,
      expression,
    })
  }
}
// Statements
impl Parser<'_> {
  fn parse_statement(&mut self) -> Statement {
    self.skip_newline();

    match self.current_kind() {
      TokenKind::Comment => self.comment_statement(),
      TokenKind::From => self.import_statement(),
      TokenKind::Let => self.let_statement(None),
      TokenKind::Return => self.return_statement(),
      _ => self.expression_statement(),
    }
  }

  fn comment_statement(&mut self) -> Statement {
    let (_, start) = self.advance();
    let mut end = start;

    while self.matches(TokenKind::EndOfLine) {
      if self.current_kind() == TokenKind::Comment {
        (_, end) = self.advance();
      } else {
        break;
      }
    }

    if self.current_kind() == TokenKind::Let {
      // this is a doc comment, so we attatch it with the relevant let statement
      let doc_comment = self.ast.add_statement(CommentStmt { start, end }.into());
      return self.let_statement(Some(doc_comment));
    }

    CommentStmt { start, end }.into()
  }

  fn expression_statement(&mut self) -> Statement {
    let expression = self.parse_expression_with_newline();

    expression.into()
  }

  fn import_statement(&mut self) -> Statement {
    let (_, keyword) = self.advance();
    let module = self.possibly_missing_module_name();
    self.expect(TokenKind::Import);
    self.skip_newline();
    let start_curly = self.expect(TokenKind::LeftCurly);
    loop {
      match self.current_kind() {
        TokenKind::RightCurly => break,
        TokenKind::Comma | TokenKind::EndOfLine => self.position += 1,
        TokenKind::Identifier => {
          self.position += 1;

          if self.matches(TokenKind::As) {
            self.possibly_missing_identifier(TokenKind::Comma);
          }
        }
        token if token.is_keyword() => {
          let token = self.current_token();
          self.position += 1;

          if self.matches(TokenKind::As) {
            self.possibly_missing_identifier(TokenKind::Comma);
          } else {
            self.add_error(ParseError::KeywordAsImportItem(token));
          }
        }
        _ => {
          self.should_resync = true;
          break self.add_error(ParseError::ExpectedImportItem(self.current_token()));
        }
      }
    }
    self.skip_newline();
    self.resync_if_error(TokenKind::RightCurly);
    let end_curly = self.expect(TokenKind::RightCurly);
    self.expect_newline();

    let start = start_curly;
    let end = end_curly.unwrap_or(self.current_token_id());

    Statement::Import(Import {
      keyword,
      module,
      start,
      end,
    })
  }

  fn let_statement(&mut self, doc_comment: Option<StatementIdx>) -> Statement {
    let (_, keyword) = self.advance();
    let identifier = self.possibly_missing_identifier(TokenKind::Equal);
    let annotation = self.matches(TokenKind::Colon).then(|| self.parse_type());
    self.expect(TokenKind::Equal);
    let value = self.parse_expression_with_newline();

    if let Expression::Function(function) = &mut self.ast[value] {
      function.name.clone_from(&identifier);
    }

    Statement::Let(Let {
      doc_comment,
      keyword,
      annotation,
      identifier,
      value,
    })
  }

  fn return_statement(&mut self) -> Statement {
    let (_, keyword) = self.advance();
    let expression = self.parse_expression_with_newline();

    if self.function_depth == 0 {
      self.add_error(ParseError::ReturnOutsideFunction(self.ast[keyword]));
    }

    Statement::Return(Return {
      keyword,
      expression,
    })
  }
}
// Types
impl Parser<'_> {
  fn parse_type(&mut self) -> TypeIdx {
    let type_ = match self.current_kind() {
      TokenKind::Identifier => self.type_primitive(),
      TokenKind::LeftParen => self.type_group(),
      TokenKind::Caret => self.type_variable(),
      _ => {
        self.add_error(ParseError::ExpectedType(self.current_token()));
        self.ast.add_type(TypeInvalid {
          token: self.current_token_id(),
        })
      }
    };

    if self.matches(TokenKind::FatRightArrow) {
      self.type_function(type_)
    } else {
      type_
    }
  }

  fn type_primitive(&mut self) -> TypeIdx {
    let (_, identifier) = self.advance();

    if self.current_kind() == TokenKind::Less {
      let (_, opening) = self.advance();
      self.type_structure(identifier, opening)
    } else {
      self.ast.add_type(TypePrimitive::from(identifier))
    }
  }

  fn type_variable(&mut self) -> TypeIdx {
    let (_, caret) = self.advance();
    match self.expect(TokenKind::Identifier) {
      Some(name) => self.ast.add_type(TypeVariable { caret, name }),
      None => self.ast.add_type(TypeInvalid { token: caret }),
    }
  }

  fn type_function(&mut self, parameter: TypeIdx) -> TypeIdx {
    let return_ = self.parse_type();
    self.ast.add_type(TypeFunction { parameter, return_ })
  }

  fn type_group(&mut self) -> TypeIdx {
    let (_, start) = self.advance();
    self.skip_newline();

    if self.current_kind() == TokenKind::RightParen {
      return self.empty_type_group(start);
    }

    let type_ = self.parse_type();
    self.skip_newline();
    self.resync_if_error(TokenKind::RightParen);
    let end = self.expect(TokenKind::RightParen);

    self.ast.add_type(TypeGroup { start, type_, end })
  }

  fn type_structure(&mut self, structure: TokenIdx, opening: TokenIdx) -> TypeIdx {
    let parameter = self.parse_type();
    self.skip_newline();
    let closing = match self.current_kind() {
      TokenKind::RightRight if !self.type_parameter_close => {
        self.type_parameter_close = true;
        Some(self.current_token_id())
      }
      TokenKind::RightRight if self.type_parameter_close => {
        self.type_parameter_close = false;
        let (_, closing) = self.advance();
        Some(closing)
      }
      _ => self.expect(TokenKind::Greater),
    };
    self.skip_newline();

    self.ast.add_type(TypeStructure {
      structure,
      opening,
      parameter,
      closing,
    })
  }

  fn empty_type_group(&mut self, start: TokenIdx) -> TypeIdx {
    let (_, end) = self.advance();
    let type_ = self.ast.add_type(TypeInvalid { token: end });
    self.add_error(ParseError::ExpectedType(self.ast[end]));

    self.ast.add_type(TypeGroup {
      start,
      type_,
      end: Some(end),
    })
  }
}

/// The precendence of the different operators
#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq)]
enum ParsePrecedence {
  None = 1,
  Assignment, // =
  Pipeline,   // >>
  Function,   // =>
  Or,         // or
  And,        // and
  Nullish,    // ??
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // * /
  Unary,      // ! -
  Call,       // () []
  Primary,    // string, number, variable
  Comment,    // // comment
}
impl ParsePrecedence {
  const LOWEST: Self = Self::Assignment;

  fn next(self) -> Self {
    match self {
      Self::None => Self::Assignment,
      Self::Assignment => Self::Pipeline,
      Self::Pipeline => Self::Function,
      Self::Function => Self::Or,
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
      TokenKind::Equal => Self::Assignment,
      _ => Self::None,
    }
  }
}

/// An error which arose during parsing
#[derive(Clone, Debug)]
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
  /// Missing Import Item
  ExpectedImportItem(Token),
  /// Expected Type
  ExpectedType(Token),
  /// Unknown Character
  UnknownCharacter(Token),

  /// Missing Identifier
  MissingIdentifier(Token),
  /// Missing Module Name in Import
  MissingModuleName(Token),
  /// Missing Pattern
  MissingPattern(Token),

  /// Unterminated String Literal
  UnterminatedString(Token),

  /// Block must end with expression
  BlockMustEndWithExpression(Token),
  /// Return Outside of Function
  ReturnOutsideFunction(Token),

  /// No Single Equal Operator
  NoSingleEqualOperator {
    /// The equal token
    token: Token,
    /// Is the left hand side a possible assignment target
    possible_assignment: bool,
  },
  /// Keyword as Import Item
  KeywordAsImportItem(Token),
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
      Self::ExpectedImportItem(_) => "Expected Import Item".into(),
      Self::ExpectedType(_) => "Expected Type".into(),
      Self::UnknownCharacter(_) => "Unknown Character".into(),
      Self::MissingIdentifier(_) => "Missing Identifier".into(),
      Self::MissingModuleName(_) => "Missing Module Name in Import".into(),
      Self::MissingPattern(_) => "Missing Pattern".into(),
      Self::UnterminatedString(_) => "Unterminated String".into(),
      Self::BlockMustEndWithExpression(_) => "Block Must End With Expression".into(),
      Self::ReturnOutsideFunction(_) => "Return Outside of Function".into(),
      Self::NoSingleEqualOperator { .. } => "No Single Equal Operator".into(),
      Self::KeywordAsImportItem(_) => "Keyword as Import Item".into(),
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
      Self::ExpectedImportItem(t) => {
        format!("expected import item but got {}", t.kind)
      },
      Self::ExpectedType(t) => {
        format!("expected type but got {}", t.kind)
      },
      Self::UnknownCharacter(_) => "got unknown character".into(),
      Self::MissingIdentifier(_) => "expected identifier for variable name".into(),
      Self::MissingModuleName(_) => "expected module name for import".into(),
      Self::MissingPattern(_) => "expected pattern to match on".into(),
      Self::UnterminatedString(_) => "missing closing quote for string".into(),
      Self::BlockMustEndWithExpression(_) => {
        "a block must return a value, so must end with an expression rather than a declaration"
        .into()
      }
      Self::ReturnOutsideFunction(_) => "can only return a value from a function".into(),
      Self::NoSingleEqualOperator { possible_assignment: false, .. } => {
        "a single equal is not an operator. use `==` for equality".into()
      }
      Self::NoSingleEqualOperator { possible_assignment: true, .. } => {
        "a single equal is not an operator. start line with `let` for variable declaration, or use `==` for equality".into()
      }
      Self::KeywordAsImportItem(_) => {
        "import items may be keywords, but they must be renamed with `as` or accessed using module access".into()
      },
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

  /// The location of the error
  pub fn span(&self) -> Span {
    match self {
      ParseError::Expected { recieved, .. } => recieved.into(),
      ParseError::ExpectedExpression(token) => token.into(),
      ParseError::ExpectedPattern(token) => token.into(),
      ParseError::ExpectedPatternRangeEnd(token) => token.into(),
      ParseError::ExpectedImportItem(token) => token.into(),
      ParseError::ExpectedType(token) => token.into(),
      ParseError::UnknownCharacter(token) => token.into(),
      ParseError::MissingIdentifier(token) => token.into(),
      ParseError::MissingModuleName(token) => token.into(),
      ParseError::MissingPattern(token) => token.into(),
      ParseError::UnterminatedString(token) => token.into(),
      ParseError::BlockMustEndWithExpression(token) => token.into(),
      ParseError::ReturnOutsideFunction(token) => token.into(),
      ParseError::NoSingleEqualOperator { token, .. } => token.into(),
      ParseError::KeywordAsImportItem(token) => token.into(),
    }
  }

  /// Is the AST able to be correctly formatted with this error?
  #[must_use]
  pub fn is_formattable(&self) -> bool {
    #[allow(clippy::match_like_matches_macro)]
    match self {
      Self::BlockMustEndWithExpression(_) => true,
      Self::ReturnOutsideFunction(_) => true,
      Self::NoSingleEqualOperator { .. } => true,
      Self::MissingIdentifier(_) => true,
      Self::MissingModuleName(_) => true,
      Self::MissingPattern(_) => true,
      Self::KeywordAsImportItem(_) => true,
      _ => false,
    }
  }
}
impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for ParseError {}
