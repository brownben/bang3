//! Helpers to simplify the definitions of lint rules
use bang_syntax::{
  Span,
  ast::{AST, expression::*, statement::*},
};

pub(super) fn unwrap<'a>(expression: &'a Expression, ast: &'a AST) -> &'a Expression {
  match expression {
    Expression::Comment(comment) => unwrap(comment.expression(ast), ast),
    Expression::Group(group) => unwrap(group.expression(ast), ast),
    Expression::Block(block) => {
      let non_comment_statements = || {
        block
          .statements(ast)
          .filter(|s| !matches!(s, Statement::Comment(_)))
      };

      if non_comment_statements().count() == 1
        && let Some(statement) = non_comment_statements().next()
        && let Statement::Expression(expression) = statement
      {
        unwrap(expression.expression(ast), ast)
      } else {
        expression
      }
    }
    _ => expression,
  }
}

pub(super) fn is_zero(expression: &Expression, ast: &AST) -> bool {
  if let Expression::Literal(literal) = &expression
    && let LiteralValue::Number(value) = &literal.value(ast)
  {
    return *value == 0.0;
  }

  false
}

pub(super) trait IsConstant {
  fn is_constant(&self, ast: &AST) -> bool;
}

impl IsConstant for Expression {
  fn is_constant(&self, ast: &AST) -> bool {
    match self {
      Self::Call(_) | Self::ModuleAccess(_) | Self::Variable(_) | Self::Invalid(_) => false,
      Self::Function(_) | Self::Literal(_) => true,
      Self::Binary(x) => x.is_constant(ast),
      Self::Block(x) => x.is_constant(ast),
      Self::Comment(x) => x.expression(ast).is_constant(ast),
      Self::Group(x) => x.expression(ast).is_constant(ast),
      Self::FormatString(x) => x.is_constant(ast),
      Self::If(x) => x.is_constant(ast),
      Self::Match(x) => x.is_constant(ast),
      Self::Unary(x) => x.expression(ast).is_constant(ast),
    }
  }
}
impl IsConstant for Statement {
  fn is_constant(&self, ast: &AST) -> bool {
    match self {
      Self::Comment(_) | Self::Import(_) => true,
      Self::Expression(x) => x.expression(ast).is_constant(ast),
      Self::Let(x) => x.is_constant(ast),
      Self::Return(x) => x.is_constant(ast),
    }
  }
}

impl IsConstant for Binary {
  fn is_constant(&self, ast: &AST) -> bool {
    self.operator(ast) != BinaryOperator::Pipeline
      && self.left(ast).is_constant(ast)
      && self.right(ast).is_constant(ast)
  }
}
impl IsConstant for Block {
  fn is_constant(&self, ast: &AST) -> bool {
    self.statements(ast).all(|s| s.is_constant(ast))
  }
}
impl IsConstant for FormatString {
  fn is_constant(&self, ast: &AST) -> bool {
    self.expressions(ast).all(|e| e.is_constant(ast))
  }
}
impl IsConstant for Group {
  fn is_constant(&self, ast: &AST) -> bool {
    self.expression(ast).is_constant(ast)
  }
}
impl IsConstant for If {
  fn is_constant(&self, ast: &AST) -> bool {
    self.condition(ast).is_constant(ast)
      && self.then(ast).is_constant(ast)
      && self.otherwise(ast).is_none_or(|e| e.is_constant(ast))
  }
}
impl IsConstant for Match {
  fn is_constant(&self, ast: &AST) -> bool {
    self.value(ast).is_constant(ast) && self.arms().all(|arm| arm.is_constant(ast))
  }
}
impl IsConstant for MatchArm {
  fn is_constant(&self, ast: &AST) -> bool {
    self.expression(ast).is_constant(ast) && self.guard(ast).is_none_or(|x| x.is_constant(ast))
  }
}

impl IsConstant for Let {
  fn is_constant(&self, ast: &AST) -> bool {
    self.value(ast).is_constant(ast)
  }
}
impl IsConstant for Return {
  fn is_constant(&self, ast: &AST) -> bool {
    self.expression(ast).is_constant(ast)
  }
}

pub(super) trait ASTEquality {
  fn equals(&self, other: &Self, ast: &AST) -> bool;
}

impl ASTEquality for Expression {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    match (unwrap(self, ast), unwrap(other, ast)) {
      (Self::Binary(x), Self::Binary(y)) => x.equals(y, ast),
      (Self::Block(x), Self::Block(y)) => x.equals(y, ast),
      (Self::Call(x), Self::Call(y)) => x.equals(y, ast),
      // We don't compare functions, as they only have one instance
      (Self::FormatString(x), Self::FormatString(y)) => x.equals(y, ast),
      (Self::If(x), Self::If(y)) => x.equals(y, ast),
      (Self::Literal(x), Self::Literal(y)) => x.equals(y, ast),
      (Self::Match(x), Self::Match(y)) => x.equals(y, ast),
      (Self::Unary(x), Self::Unary(y)) => x.equals(y, ast),
      (Self::Variable(x), Self::Variable(y)) => x.equals(y, ast),
      _ => false,
    }
  }
}
impl ASTEquality for Statement {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    match (self, other) {
      (Self::Expression(x), Self::Expression(y)) => x.equals(y, ast),
      (Self::Import(x), Self::Import(y)) => x.equals(y, ast),
      (Self::Let(x), Self::Let(y)) => x.equals(y, ast),
      (Self::Return(x), Self::Return(y)) => x.equals(y, ast),
      _ => false,
    }
  }
}

impl ASTEquality for Binary {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.operator(ast) == other.operator(ast)
      && self.left(ast).equals(other.left(ast), ast)
      && self.right(ast).equals(other.right(ast), ast)
  }
}
impl ASTEquality for Block {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    let mut statements = self.statements(ast);
    let mut other_statements = other.statements(ast);

    let mut a = statements.next();
    let mut b = other_statements.next();
    loop {
      if a.is_none() && b.is_none() {
        return true;
      }

      while let Some(Statement::Comment(_)) = a {
        a = statements.next();
      }
      while let Some(Statement::Comment(_)) = b {
        b = other_statements.next();
        continue;
      }

      if !a.equals(&b, ast) {
        return false;
      }

      a = statements.next();
      b = other_statements.next();
    }
  }
}
impl ASTEquality for Call {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.callee(ast).equals(other.callee(ast), ast)
      && self.argument(ast).equals(&other.argument(ast), ast)
  }
}
impl ASTEquality for FormatString {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self
      .expressions(ast)
      .zip(other.expressions(ast))
      .all(|(x, y)| x.equals(y, ast))
      && self
        .strings(ast)
        .zip(other.strings(ast))
        .all(|(x, y)| x == y)
  }
}
impl ASTEquality for If {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.condition(ast).equals(other.condition(ast), ast)
      && self.then(ast).equals(other.then(ast), ast)
      && self.otherwise(ast).equals(&other.otherwise(ast), ast)
  }
}
impl ASTEquality for Literal {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.value(ast) == other.value(ast)
  }
}
impl ASTEquality for Match {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.value(ast).equals(other.value(ast), ast)
      && self.arms().zip(other.arms()).all(|(x, y)| x.equals(y, ast))
  }
}
impl ASTEquality for MatchArm {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.pattern.equals(&other.pattern, ast)
      && self.guard(ast).equals(&other.guard(ast), ast)
      && self.expression(ast).equals(other.expression(ast), ast)
  }
}
impl ASTEquality for Pattern {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    match (self, other) {
      (Self::Identifier(_), Self::Identifier(_)) => true,
      (Self::Literal(x), Self::Literal(y)) => x.equals(y, ast),
      (Self::Range(a, b), Self::Range(c, d)) => a.equals(c, ast) && b.equals(d, ast),
      _ => false,
    }
  }
}
impl ASTEquality for Unary {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.operator(ast) == other.operator(ast)
      && self.expression(ast).equals(other.expression(ast), ast)
  }
}
impl ASTEquality for Variable {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.name(ast) == other.name(ast)
  }
}

impl ASTEquality for ExpressionStmt {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.expression(ast).equals(other.expression(ast), ast)
  }
}
impl ASTEquality for Import {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.module(ast) == other.module(ast)
      && self
        .items(ast)
        .zip(other.items(ast))
        .all(|(x, y)| x.equals(&y, ast))
  }
}
impl ASTEquality for ImportItem<'_> {
  fn equals(&self, other: &Self, _ast: &AST) -> bool {
    self.name == other.name && self.alias == other.alias
  }
}
impl ASTEquality for Let {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.identifier(ast) == other.identifier(ast) && self.value(ast).equals(other.value(ast), ast)
  }
}
impl ASTEquality for Return {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    self.expression(ast).equals(other.expression(ast), ast)
  }
}

impl<T: ASTEquality> ASTEquality for Option<T> {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    match (self, other) {
      (Some(x), Some(y)) => x.equals(y, ast),
      (None, None) => true,
      _ => false,
    }
  }
}
impl<T: ASTEquality> ASTEquality for &T {
  fn equals(&self, other: &Self, ast: &AST) -> bool {
    (*self).equals(other, ast)
  }
}

pub(super) trait ReturnAnalysis {
  /// Does the AST node always return?
  fn always_returns(&self, ast: &AST) -> bool;

  /// Does the AST node end with a return statement?
  ///
  /// Gets the span of the return statement if it exists.
  fn ends_with_return(&self, _ast: &AST) -> Option<Span> {
    None
  }
}

impl ReturnAnalysis for Statement {
  fn always_returns(&self, ast: &AST) -> bool {
    match self {
      Statement::Comment(_) => false,
      Statement::Expression(expression) => expression.expression(ast).always_returns(ast),
      Statement::Import(_) => false,
      Statement::Let(_) => false,
      Statement::Return(_) => true,
    }
  }

  fn ends_with_return(&self, ast: &AST) -> Option<Span> {
    match self {
      Statement::Comment(_) => None,
      Statement::Expression(expression) => expression.expression(ast).ends_with_return(ast),
      Statement::Import(_) => None,
      Statement::Let(_) => None,
      Statement::Return(return_) => Some(return_.span(ast)),
    }
  }
}
impl ReturnAnalysis for Expression {
  fn always_returns(&self, ast: &AST) -> bool {
    match self {
      Expression::Binary(binary) => binary.always_returns(ast),
      Expression::Block(block) => block.always_returns(ast),
      Expression::Call(call) => call.always_returns(ast),
      Expression::Comment(comment) => comment.expression(ast).always_returns(ast),
      Expression::FormatString(format_string) => format_string.always_returns(ast),
      Expression::Function(_) => false,
      Expression::Group(group) => group.expression(ast).always_returns(ast),
      Expression::If(if_) => if_.always_returns(ast),
      Expression::Literal(_) => false,
      Expression::Match(match_) => match_.always_returns(ast),
      Expression::ModuleAccess(_) => false,
      Expression::Unary(unary) => unary.expression(ast).always_returns(ast),
      Expression::Variable(_) => false,
      Expression::Invalid(_) => false,
    }
  }

  fn ends_with_return(&self, ast: &AST) -> Option<Span> {
    match self {
      Expression::Block(block) => block.ends_with_return(ast),
      Expression::Comment(comment) => comment.expression(ast).ends_with_return(ast),
      Expression::Group(group) => group.expression(ast).ends_with_return(ast),
      Expression::If(if_) => if_.ends_with_return(ast),
      Expression::Match(match_) => match_.ends_with_return(ast),
      Expression::Unary(unary) => unary.expression(ast).ends_with_return(ast),

      // Can never end with a return
      Expression::Function(_)
      | Expression::Literal(_)
      | Expression::ModuleAccess(_)
      | Expression::Variable(_)
      | Expression::Invalid(_) => None,

      // Could feasibly end with a return, but would be contrived code not making sense
      Expression::Binary(_) | Expression::Call(_) | Expression::FormatString(_) => None,
    }
  }
}
impl ReturnAnalysis for Binary {
  fn always_returns(&self, ast: &AST) -> bool {
    self.left(ast).always_returns(ast) || self.right(ast).always_returns(ast)
  }
}
impl ReturnAnalysis for Block {
  fn always_returns(&self, ast: &AST) -> bool {
    self
      .statements(ast)
      .any(|statement| statement.always_returns(ast))
  }

  fn ends_with_return(&self, ast: &AST) -> Option<Span> {
    self
      .statements(ast)
      .rev()
      .find(|statement| !matches!(statement, Statement::Comment(_)))
      .and_then(|statement| statement.ends_with_return(ast))
  }
}
impl ReturnAnalysis for Call {
  fn always_returns(&self, ast: &AST) -> bool {
    self.callee(ast).always_returns(ast)
      || self.argument(ast).is_some_and(|x| x.always_returns(ast))
  }
}
impl ReturnAnalysis for FormatString {
  fn always_returns(&self, ast: &AST) -> bool {
    self
      .expressions(ast)
      .all(|expression| expression.always_returns(ast))
  }
}
impl ReturnAnalysis for If {
  fn always_returns(&self, ast: &AST) -> bool {
    self.then(ast).always_returns(ast)
      && self
        .otherwise(ast)
        .is_some_and(|otherwise| otherwise.always_returns(ast))
  }

  fn ends_with_return(&self, ast: &AST) -> Option<Span> {
    if let Some(otherwise) = self.otherwise(ast) {
      return self
        .then(ast)
        .ends_with_return(ast)
        .or(otherwise.ends_with_return(ast));
    };

    None
  }
}
impl ReturnAnalysis for Match {
  fn always_returns(&self, ast: &AST) -> bool {
    self
      .arms()
      .all(|arm| arm.expression(ast).always_returns(ast))
  }

  fn ends_with_return(&self, ast: &AST) -> Option<Span> {
    self
      .arms()
      .find_map(|arm| arm.expression(ast).ends_with_return(ast))
  }
}
