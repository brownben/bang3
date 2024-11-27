//! # Expressions

use crate::{
  ast::statement::Statement,
  ast::{AST, ExpressionIdx, StatementIdx, TokenIdx},
  span::Span,
  tokeniser::TokenKind,
};
use std::fmt;
use thin_vec::ThinVec;

/// An expression, which can be evaluated to a value
#[must_use]
#[derive(Debug)]
pub enum Expression {
  /// A binary expression, e.g. `1 + 2`, `true and false`
  Binary(Binary),
  /// A block of statements, e.g. `{ .. }`
  Block(Block),
  /// A function call, e.g. `foo(1)`
  Call(Call),
  /// An expression followed by a comment, e.g. `5 // comment`
  Comment(Comment),
  /// A format string, a string where expressions are interpolated, e.g. \`hello {name}\`
  FormatString(FormatString),
  /// A function, e.g. `x => x + 1`
  Function(Function),
  /// An expression in parentheses, e.g. `(1 + 2)`
  Group(Group),
  /// An if expression - based on the condition execute a single branch, e.g. `if (x) 1 else 2`
  If(If),
  /// A literal value (string/ number/ boolean), e.g. `1`, `true`, `"hello"`
  Literal(Literal),
  /// A match expression, e.g. `match x | 1 => 1 | _ => 2`
  Match(Match),
  /// Accessing an item from a module, e.g. `maths::sin`
  ModuleAccess(ModuleAccess),
  /// A unary expression, e.g. `!true`, `-1`
  Unary(Unary),
  /// A variable, e.g. `x`
  Variable(Variable),
  /// An invalid expression
  Invalid(Invalid),
}
impl Expression {
  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    match self {
      Self::Binary(binary) => binary.span(ast),
      Self::Block(block) => block.span(ast),
      Self::Call(call) => call.span(ast),
      Self::Comment(comment) => comment.span(ast),
      Self::FormatString(format_string) => format_string.span(ast),
      Self::Function(function) => function.span(ast),
      Self::Group(group) => group.span(ast),
      Self::If(if_) => if_.span(ast),
      Self::Literal(literal) => literal.span(ast),
      Self::Match(match_) => match_.span(ast),
      Self::ModuleAccess(module_access) => module_access.span(ast),
      Self::Unary(unary) => unary.span(ast),
      Self::Variable(variable) => variable.span(ast),
      Self::Invalid(invalid) => invalid.span(ast),
    }
  }
}

/// A binary expression, e.g. `1 + 2`, `true and false`
#[derive(Debug)]
pub struct Binary {
  pub(crate) left: ExpressionIdx,
  pub(crate) operator: TokenIdx,
  pub(crate) right: ExpressionIdx,
}
impl Binary {
  /// The left hand side of the operation
  pub fn left<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.left]
  }
  /// The right hand side of the operation
  pub fn right<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.right]
  }

  /// The operator of the operation
  pub fn operator(&self, ast: &AST) -> BinaryOperator {
    match ast[self.operator].kind {
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
      TokenKind::Equal => BinaryOperator::InvalidSingleEqual,
      _ => unreachable!(),
    }
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    self.left(ast).span(ast).merge(self.right(ast).span(ast))
  }
}

/// A block of statements, e.g. `{ .. }`
#[derive(Debug)]
pub struct Block {
  pub(crate) opening: TokenIdx,
  pub(crate) statements: ThinVec<StatementIdx>,
  pub(crate) closing: Option<TokenIdx>,
}
impl Block {
  /// How many statements are in a block
  #[must_use]
  pub fn len(&self) -> usize {
    self.statements.len()
  }
  /// Does the block have any statements?
  #[must_use]
  pub fn is_empty(&self) -> bool {
    self.statements.is_empty()
  }

  /// Get a specific statement by index in a block
  pub fn statement<'a>(&'a self, index: usize, ast: &'a AST) -> &'a Statement {
    &ast[self.statements[index]]
  }
  /// The statements in a block
  #[must_use]
  pub fn statements<'a>(
    &'a self,
    ast: &'a AST,
  ) -> impl DoubleEndedIterator<Item = &'a Statement> + ExactSizeIterator {
    self
      .statements
      .iter()
      .map(|statement_idx| &ast[*statement_idx])
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let end = if let Some(closing) = self.closing {
      Span::from(ast[closing])
    } else if let Some(last_statement) = self.statements.last() {
      ast[*last_statement].span(ast)
    } else {
      Span::default()
    };

    Span::from(ast[self.opening]).merge(end)
  }
}

/// A function call, e.g. `foo(1)`
#[derive(Debug)]
pub struct Call {
  pub(crate) callee: ExpressionIdx,
  pub(crate) opening: TokenIdx,
  pub(crate) argument: Option<ExpressionIdx>,
  pub(crate) closing: Option<TokenIdx>,
}
impl Call {
  /// The expression which will be called
  pub fn callee<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.callee]
  }
  /// The expression to be passed as an argument
  #[must_use]
  pub fn argument<'a>(&self, ast: &'a AST) -> Option<&'a Expression> {
    self.argument.map(|argument| &ast[argument])
  }
  /// The span for the argument (includes the brackets)
  pub fn argument_span(&self, ast: &AST) -> Span {
    let end = self
      .closing
      .map(|closing| Span::from(ast[closing]))
      .or(self.argument(ast).map(|a| a.span(ast)))
      .unwrap_or_default();
    Span::from(ast[self.opening]).merge(end)
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let end = if let Some(closing) = self.closing {
      Span::from(ast[closing])
    } else if let Some(argument) = self.argument {
      ast[argument].span(ast)
    } else {
      Span::from(ast[self.opening])
    };

    self.callee(ast).span(ast).merge(end)
  }
}

/// An expression followed by a comment, e.g. `5 // comment`
#[derive(Debug)]
pub struct Comment {
  pub(crate) expression: ExpressionIdx,
  pub(crate) comment: TokenIdx,
}
impl Comment {
  /// The expression before the comment
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The text of the comment
  #[must_use]
  pub fn text<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.comment)[2..].trim()
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    // For easy diagnostics, we generally want to highlight the expression not the comment
    self.expression(ast).span(ast)
  }
}

/// A format string, a string where expressions are interpolated, e.g. \`hello {name}\`
#[derive(Debug)]
pub struct FormatString {
  pub(crate) strings: ThinVec<TokenIdx>,
  pub(crate) expressions: ThinVec<ExpressionIdx>,
}
impl FormatString {
  /// The strings which appear within the format string
  pub fn strings<'a>(&self, ast: &'a AST) -> impl Iterator<Item = &'a str> {
    self.strings.iter().map(|token| {
      let string = ast.get_token_text(*token);

      if string.len() >= 2 {
        &string[1..(string.len() - 1)]
      } else {
        &string[1..]
      }
    })
  }
  /// The strings in the format string and their spans
  pub fn strings_with_spans<'a>(&'a self, ast: &'a AST) -> impl Iterator<Item = (&'a str, Span)> {
    let spans = self.strings.iter().map(|token| ast[*token].into());
    self.strings(ast).zip(spans)
  }

  /// The expressions which appear within the format string
  pub fn expressions<'a>(&'a self, ast: &'a AST) -> impl Iterator<Item = &'a Expression> {
    self.expressions.iter().map(|expression| &ast[*expression])
  }

  /// The location of the expression
  #[allow(clippy::missing_panics_doc, reason = "strings is not empty")]
  pub fn span(&self, ast: &AST) -> Span {
    let start = Span::from(ast[self.strings[0]]);
    let end = Span::from(ast[*self.strings.last().unwrap()]);

    start.merge(end)
  }
}

/// A function, e.g. `x => x + 1`
#[derive(Debug)]
pub struct Function {
  pub(crate) name: Option<Variable>,
  /// The functions parameter
  pub parameter: Variable,
  pub(crate) body: ExpressionIdx,
}
impl Function {
  /// The name of the function, if it has one
  #[must_use]
  pub fn name<'a>(&self, ast: &'a AST) -> Option<&'a str> {
    self.name.as_ref().map(|name| name.name(ast))
  }
  /// The body of the function
  pub fn body<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.body]
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.parameter.token]).merge(self.body(ast).span(ast))
  }
}

/// An expression in parentheses, e.g. `(1 + 2)`
#[derive(Debug)]
pub struct Group {
  pub(crate) start: TokenIdx,
  pub(crate) expression: ExpressionIdx,
  pub(crate) end: Option<TokenIdx>,
}
impl Group {
  /// The expression within the parentheses
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let start = Span::from(ast[self.start]);

    if let Some(end) = self.end {
      start.merge(ast[end].into())
    } else {
      start.merge(self.expression(ast).span(ast))
    }
  }
}

/// A literal value (string/ number/ boolean), e.g. `1`, `true`, `"hello"`
#[derive(Debug)]
pub struct Literal {
  pub(crate) token: TokenIdx,
}
impl Literal {
  /// The value of the literal
  pub fn value<'a>(&self, ast: &'a AST) -> LiteralValue<'a> {
    let token = ast[self.token];
    match token.kind {
      TokenKind::Number => LiteralValue::Number(self.number_value(ast)),
      TokenKind::String => LiteralValue::String(self.string_value(ast)),
      TokenKind::True => LiteralValue::Boolean(true),
      TokenKind::False => LiteralValue::Boolean(false),
      _ => unreachable!(),
    }
  }
  /// The underlying raw string contents of the literal
  pub fn raw_value<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.token)
  }

  fn number_value(&self, ast: &AST) -> f64 {
    let raw = ast.get_token_text(self.token);

    if raw.contains('_') {
      raw.replace('_', "").parse()
    } else {
      raw.parse()
    }
    .expect("string to be valid number representation")
  }

  fn string_value<'a>(&self, ast: &'a AST) -> &'a str {
    let string = ast.get_token_text(self.token);
    &string[1..(string.len() - 1)]
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}

/// An if expression - based on the condition execute a single branch, e.g. `if (x) 1 else 2`
#[derive(Debug)]
pub struct If {
  pub(crate) keyword: TokenIdx,
  pub(crate) condition: ExpressionIdx,
  pub(crate) then: ExpressionIdx,
  pub(crate) otherwise: Option<ExpressionIdx>,
}
impl If {
  /// The condition
  pub fn condition<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.condition]
  }
  /// The first branch, executed if the condition is true
  pub fn then<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.then]
  }
  /// A second optional branch, executed if the condition is false
  #[must_use]
  pub fn otherwise<'a>(&self, ast: &'a AST) -> Option<&'a Expression> {
    self.otherwise.as_ref().map(|otherwise| &ast[*otherwise])
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let end = if let Some(otherwise) = self.otherwise {
      ast[otherwise].span(ast)
    } else {
      ast[self.then].span(ast)
    };

    Span::from(ast[self.keyword]).merge(end)
  }
}

/// A match expression, e.g. `match x | 1 => 1 | _ => 2`
///
/// Matches a value against a series of patterns and executes the first branch that matches.
#[derive(Debug)]
pub struct Match {
  pub(crate) keyword: TokenIdx,
  pub(crate) value: ExpressionIdx,
  pub(crate) arms: ThinVec<MatchArm>,
}
impl Match {
  /// The value being matched upon
  pub fn value<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.value]
  }
  /// The arms of the match
  #[must_use]
  pub fn arms(&self) -> impl ExactSizeIterator<Item = &MatchArm> {
    self.arms.iter()
  }
  /// The arms of the match
  #[allow(clippy::missing_panics_doc)]
  pub fn arms_span(&self, ast: &AST) -> Span {
    if self.arms.is_empty() {
      return Span::default();
    }

    let start = self.value(ast).span(ast).end;
    let end = self.arms.last().unwrap().span(ast);
    Span::new(start, start).merge(end)
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let end = if let Some(last_arm) = self.arms.last() {
      last_arm.span(ast)
    } else {
      self.value(ast).span(ast)
    };

    Span::from(ast[self.keyword]).merge(end)
  }
}
/// An arm of a match expression, e.g. `1 => 1`
#[derive(Debug)]
pub struct MatchArm {
  /// The pattern to match against
  pub pattern: Pattern,
  pub(crate) guard: Option<ExpressionIdx>,
  pub(crate) expression: ExpressionIdx,
}
impl MatchArm {
  /// The guard of the arm, if it has one `PATTERN if GUARD -> EXPRESSION`
  ///
  /// The expression is only run if the pattern matches and the guard is truthy
  #[must_use]
  pub fn guard<'a>(&self, ast: &'a AST) -> Option<&'a Expression> {
    self.guard.as_ref().map(|guard| &ast[*guard])
  }
  /// The expression to execute if the pattern matches
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    self.pattern.span(ast).merge(self.expression(ast).span(ast))
  }
}
/// A pattern to match against, e.g. `1`, `x`, `1..2`, `..'b'`
#[derive(Debug)]
pub enum Pattern {
  /// A variable, e.g. `x`
  Identifier(Variable),
  /// A literal value, e.g. `1`, `true`, `"hello"`
  Literal(Literal),
  /// A range, e.g. `1..2`, `..'b'` - is a comparison against the start and end
  /// The start or end missing indicates an open range. It is inclusive of both values.
  /// The only literals which can be used as part of a range are numbers and strings (ordered).
  Range(Option<Literal>, Option<Literal>),
  /// An invalid pattern
  Invalid,
}
impl Pattern {
  /// The location of the pattern
  pub fn span(&self, ast: &AST) -> Span {
    match self {
      Pattern::Identifier(identifier) => identifier.span(ast),
      Pattern::Literal(literal) => literal.span(ast),
      Pattern::Range(Some(start), Some(end)) => start.span(ast).merge(end.span(ast)),
      Pattern::Range(Some(start), None) => start.span(ast),
      Pattern::Range(None, Some(end)) => end.span(ast),
      Pattern::Range(None, None) => unreachable!(),
      Pattern::Invalid => Span::default(),
    }
  }
}

/// Accessing an item from a module, e.g. `maths::sin`
#[derive(Debug)]
pub struct ModuleAccess {
  /// The module being accessed
  pub(crate) module: TokenIdx,
  /// The item being accessed in the module
  pub(crate) item: Option<TokenIdx>,
}
impl ModuleAccess {
  /// The module being accessed
  pub fn module<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.module)
  }

  /// The item being accessed from the module
  pub fn item<'a>(&self, ast: &'a AST) -> &'a str {
    self.item.map(|i| ast.get_token_text(i)).unwrap_or_default()
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    let end = self.item.unwrap_or(self.module.next());

    Span::from(ast[self.module]).merge(ast[end].into())
  }

  /// The location of the item being accessed from the module
  pub fn item_span(&self, ast: &AST) -> Span {
    self.item.map(|i| ast[i]).unwrap_or_default().into()
  }
}

/// A unary expression, e.g. `!true`, `-1`
#[derive(Debug)]
pub struct Unary {
  pub(crate) operator: TokenIdx,
  pub(crate) expression: ExpressionIdx,
}
impl Unary {
  /// The operator of the expression
  pub fn operator(&self, ast: &AST) -> UnaryOperator {
    match ast[self.operator].kind {
      TokenKind::Minus => UnaryOperator::Minus,
      TokenKind::Bang => UnaryOperator::Not,
      _ => unreachable!(),
    }
  }
  /// The expression being operated on
  pub fn expression<'a>(&self, ast: &'a AST) -> &'a Expression {
    &ast[self.expression]
  }

  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.operator]).merge(self.expression(ast).span(ast))
  }
}

/// A variable, e.g. `x`
#[derive(Clone, Debug)]
pub struct Variable {
  pub(crate) token: TokenIdx,
}
impl Variable {
  /// The name of the variable
  #[must_use]
  pub fn name<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.token)
  }

  /// The location of the variable
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}

/// An invalid expression
#[derive(Debug)]
pub struct Invalid {
  pub(crate) token: TokenIdx,
}
impl Invalid {
  /// The location of the expression
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}

impl From<Binary> for Expression {
  fn from(value: Binary) -> Self {
    Self::Binary(value)
  }
}
impl From<Block> for Expression {
  fn from(value: Block) -> Self {
    Self::Block(value)
  }
}
impl From<Call> for Expression {
  fn from(value: Call) -> Self {
    Self::Call(value)
  }
}
impl From<Comment> for Expression {
  fn from(value: Comment) -> Self {
    Self::Comment(value)
  }
}
impl From<FormatString> for Expression {
  fn from(value: FormatString) -> Self {
    Self::FormatString(value)
  }
}
impl From<Function> for Expression {
  fn from(value: Function) -> Self {
    Self::Function(value)
  }
}
impl From<Group> for Expression {
  fn from(value: Group) -> Self {
    Self::Group(value)
  }
}
impl From<If> for Expression {
  fn from(value: If) -> Self {
    Self::If(value)
  }
}
impl From<Literal> for Expression {
  fn from(value: Literal) -> Self {
    Self::Literal(value)
  }
}
impl From<Match> for Expression {
  fn from(value: Match) -> Self {
    Self::Match(value)
  }
}
impl From<ModuleAccess> for Expression {
  fn from(value: ModuleAccess) -> Self {
    Self::ModuleAccess(value)
  }
}
impl From<Unary> for Expression {
  fn from(value: Unary) -> Self {
    Self::Unary(value)
  }
}
impl From<Variable> for Expression {
  fn from(value: Variable) -> Self {
    Self::Variable(value)
  }
}
impl From<Invalid> for Expression {
  fn from(value: Invalid) -> Self {
    Self::Invalid(value)
  }
}

/// The operators which can be used in binary expressions
#[must_use]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
  /// `+`
  Add,
  /// `-`
  Subtract,
  /// `*`
  Multiply,
  /// `/`
  Divide,
  /// `%`
  Remainder,
  /// `++`
  AddString,
  /// `!=`
  NotEqual,
  /// `==`
  Equal,
  /// `>`
  Greater,
  /// `>=`
  GreaterEqual,
  /// `<`
  Less,
  /// `<=`
  LessEqual,
  /// `and`
  And,
  /// `or`
  Or,
  /// function call pipeline, e.g. `x >> f` == `f(x)`
  Pipeline,
  /// Invalid operator
  InvalidSingleEqual,
}
impl BinaryOperator {
  /// The string representation of the operator
  #[must_use]
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
      Self::InvalidSingleEqual => "=",
    }
  }
}
impl fmt::Display for BinaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

/// The operators which can be used in unary expressions
#[must_use]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
  /// `!`
  Not,
  /// `-`
  Minus,
}
impl UnaryOperator {
  /// The string representation of the operator
  #[must_use]
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

/// The types of literals
#[must_use]
#[derive(Debug, PartialEq)]
pub enum LiteralValue<'source> {
  /// A boolean literal
  Boolean(bool),
  /// A number literal
  Number(f64),
  /// A string literal
  String(&'source str),
}
