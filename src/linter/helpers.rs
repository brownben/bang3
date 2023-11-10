use crate::ast::{expression::*, statement::*};

impl Expression<'_, '_> {
  pub fn unwrap(&self) -> &Self {
    match self {
      Self::Comment(comment) => comment.expression.unwrap(),
      Self::Group(group) => group.expression.unwrap(),
      Self::Block(block) => {
        if let Some(statement) = block.statements.first()
          && let Statement::Expression(expression) = statement
        {
          expression
        } else {
          self
        }
      }
      _ => self,
    }
  }
}

pub(super) trait IsConstant {
  fn is_constant(&self) -> bool;
}

impl IsConstant for Expression<'_, '_> {
  fn is_constant(&self) -> bool {
    match self {
      Self::Call(_) | Self::Variable(_) => false,
      Self::Function(_) | Self::Literal(_) => true,
      Self::Binary(x) => x.is_constant(),
      Self::Block(x) => x.is_constant(),
      Self::Comment(x) => x.is_constant(),
      Self::Group(x) => x.is_constant(),
      Self::If(x) => x.is_constant(),
      Self::Match(x) => x.is_constant(),
      Self::Unary(x) => x.is_constant(),
    }
  }
}
impl IsConstant for Statement<'_, '_> {
  fn is_constant(&self) -> bool {
    match self {
      Self::Comment(_) => true,
      Self::Expression(x) => x.is_constant(),
      Self::Let(x) => x.is_constant(),
    }
  }
}

impl IsConstant for Binary<'_, '_> {
  fn is_constant(&self) -> bool {
    self.operator != BinaryOperator::Pipeline && self.left.is_constant() && self.right.is_constant()
  }
}
impl IsConstant for Block<'_, '_> {
  fn is_constant(&self) -> bool {
    self.statements.iter().all(IsConstant::is_constant)
  }
}
impl IsConstant for Comment<'_, '_> {
  fn is_constant(&self) -> bool {
    self.expression.is_constant()
  }
}
impl IsConstant for Group<'_, '_> {
  fn is_constant(&self) -> bool {
    self.expression.is_constant()
  }
}
impl IsConstant for If<'_, '_> {
  fn is_constant(&self) -> bool {
    self.condition.is_constant()
      && self.then.is_constant()
      && self
        .otherwise
        .as_ref()
        .map_or(true, IsConstant::is_constant)
  }
}
impl IsConstant for Match<'_, '_> {
  fn is_constant(&self) -> bool {
    self.cases.iter().all(IsConstant::is_constant)
  }
}
impl IsConstant for MatchCase<'_, '_> {
  fn is_constant(&self) -> bool {
    self.expression.is_constant()
  }
}
impl IsConstant for Unary<'_, '_> {
  fn is_constant(&self) -> bool {
    self.expression.is_constant()
  }
}

impl IsConstant for Let<'_, '_> {
  fn is_constant(&self) -> bool {
    self.expression.is_constant()
  }
}

pub(super) trait ASTEquality {
  fn equals(&self, other: &Self) -> bool;
}

impl ASTEquality for Expression<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Comment(x), y) => x.expression.equals(y),
      (x, Self::Comment(y)) => x.equals(&y.expression),
      (Self::Group(x), y) => x.expression.equals(y),
      (x, Self::Group(y)) => x.equals(&y.expression),

      (Self::Binary(x), Self::Binary(y)) => x.equals(y),
      (Self::Block(x), Self::Block(y)) => x.equals(y),
      (Self::Call(x), Self::Call(y)) => x.equals(y),
      (Self::Function(x), Self::Function(y)) => x.equals(y),
      (Self::If(x), Self::If(y)) => x.equals(y),
      (Self::Literal(x), Self::Literal(y)) => x.equals(y),
      (Self::Match(x), Self::Match(y)) => x.equals(y),
      (Self::Unary(x), Self::Unary(y)) => x.equals(y),
      (Self::Variable(x), Self::Variable(y)) => x.equals(y),
      _ => false,
    }
  }
}
impl ASTEquality for Statement<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Comment(_), Self::Comment(_)) => true,
      (Self::Expression(x), Self::Expression(y)) => x.equals(y),
      (Self::Let(x), Self::Let(y)) => x.equals(y),
      _ => false,
    }
  }
}

impl ASTEquality for Binary<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.operator == other.operator
      && self.left.equals(&other.left)
      && self.right.equals(&other.right)
  }
}
impl ASTEquality for Block<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self
      .statements
      .iter()
      .zip(&other.statements)
      .map(|(x, y)| x.equals(y))
      .all(|x| x)
  }
}
impl ASTEquality for Call<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.expression.equals(&other.expression) && self.argument.equals(&other.argument)
  }
}
impl ASTEquality for Function<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.parameter == other.parameter && self.body.equals(&other.body)
  }
}
impl ASTEquality for If<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.condition.equals(&other.condition)
      && self.then.equals(&other.then)
      && self.otherwise.equals(&other.otherwise)
  }
}
impl ASTEquality for Literal<'_> {
  fn equals(&self, other: &Self) -> bool {
    self.kind == other.kind
  }
}
impl ASTEquality for Match<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.value.equals(&other.value)
      && self
        .cases
        .iter()
        .zip(&other.cases)
        .all(|(x, y)| x.equals(y))
  }
}
impl ASTEquality for MatchCase<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.pattern.equals(&other.pattern) && self.expression.equals(&other.expression)
  }
}
impl ASTEquality for Pattern<'_> {
  fn equals(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Identifier(_), Self::Identifier(_)) => true,
      (Self::Literal(x), Self::Literal(y)) => x.equals(y),
      _ => false,
    }
  }
}
impl ASTEquality for Unary<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.operator == other.operator && self.expression.equals(&other.expression)
  }
}
impl ASTEquality for Variable<'_> {
  fn equals(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl ASTEquality for Let<'_, '_> {
  fn equals(&self, other: &Self) -> bool {
    self.identifier == other.identifier && self.expression.equals(&other.expression)
  }
}

impl<T: ASTEquality> ASTEquality for Option<T> {
  fn equals(&self, other: &Self) -> bool {
    match (self, other) {
      (Some(x), Some(y)) => x.equals(y),
      (None, None) => true,
      _ => false,
    }
  }
}