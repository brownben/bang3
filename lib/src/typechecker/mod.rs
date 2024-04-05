use crate::ast::{expression::*, statement::*, GetSpan, Span, AST};
use std::{
  error,
  fmt::{self, Debug},
  rc::Rc,
};

mod exhaustive;
#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
struct Var {
  identifier: String,
  depth: usize,
  type_: Type,
}

#[derive(Clone, Default, Debug)]
pub struct Typechecker {
  types: Vec<Type>,
  variables: Vec<Var>,
  depth: usize,
  errors: Vec<TypeError>,
}
impl Typechecker {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn check(mut self, ast: &AST) -> Vec<TypeError> {
    ast.typecheck(&mut self);
    self.errors
  }

  fn new_scope(&mut self) {
    self.depth += 1;
  }
  fn end_scope(&mut self) {
    self.depth -= 1;
    self.variables.retain(|v| v.depth <= self.depth);
  }
  fn add_variable(&mut self, identifier: String, type_: Type) {
    self.variables.push(Var {
      identifier,
      depth: self.depth,
      type_,
    });
  }

  /// Unwrap a result, saving any errors found.
  ///
  /// Used when want to continue a block without early exit on an error
  fn record_error(&mut self, result: Result<Type, TypeError>) -> Type {
    match result {
      Ok(type_) => type_,
      Err(error) => {
        self.errors.push(error);
        Type::Unknown
      }
    }
  }

  /// Define a new existential, for a type that is yet known
  fn new_type(&mut self) -> Type {
    let id = self.types.len();
    self.types.push(Type::Unknown);
    Type::Existential(id)
  }

  /// Lookup existential types to get the current known type
  fn expand_type(&self, type_: Type) -> Type {
    match type_ {
      Type::Existential(id) => {
        if self.types[id] == Type::Unknown {
          Type::Existential(id)
        } else {
          self.types[id].clone()
        }
      }
      Type::Function(function) => FunctionType {
        parameter: self.expand_type(function.parameter.clone()),
        return_type: self.expand_type(function.return_type.clone()),
      }
      .into(),
      _ => type_,
    }
  }

  /// Assert that a given type is the same as an expected type
  fn assert_type(&mut self, given: Type, expected: Type, span: Span) -> Result<Type, TypeError> {
    let given = self.expand_type(given);
    let expected = self.expand_type(expected);

    match (&given, &expected) {
      (Type::Primitive(a), Type::Primitive(b)) if a.matches(*b) => Ok(expected),
      (Type::Function(a), Type::Function(b)) => {
        let parameter = self.assert_type(a.parameter.clone(), b.parameter.clone(), span);
        let return_ = self.assert_type(a.return_type.clone(), b.return_type.clone(), span);

        match (parameter, return_) {
          (Ok(_), Ok(_)) => Ok(expected),
          _ => Err(TypeError::ExpectedDifferentType {
            expected: self.expand_type(expected.clone()).to_string(),
            given: self.expand_type(given).to_string(),
            span,
          }),
        }
      }
      (Type::Existential(a), _) | (_, Type::Existential(a)) => {
        self.types[*a] = expected.clone();
        Ok(expected)
      }
      _ => Err(TypeError::ExpectedDifferentType {
        expected: self.expand_type(expected).to_string(),
        given: self.expand_type(given).to_string(),
        span,
      }),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
  Primitive(Primitive),
  Function(Rc<FunctionType>),
  Existential(usize),
  Unknown,
}
impl Type {
  fn is_truthy(&self) -> bool {
    match self {
      Type::Primitive(p) => p.is_truthy(),
      Type::Function(_) => true,
      _ => false,
    }
  }
  fn is_falsy(&self) -> bool {
    match self {
      Type::Primitive(p) => p.is_falsy(),
      _ => false,
    }
  }

  fn is_string(&self) -> bool {
    matches!(self, Type::Primitive(Primitive::String))
  }

  fn expand_boolean(self) -> Type {
    match self {
      Type::Primitive(Primitive::True | Primitive::False) => Primitive::Boolean.into(),
      _ => self,
    }
  }
}
impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Primitive(p) => write!(f, "{p}"),
      Type::Existential(type_) => write!(f, "'{type_}"),
      Type::Function(function) => {
        write!(f, "{} -> {}", function.parameter, function.return_type)
      }
      Type::Unknown => write!(f, "unknown"),
    }
  }
}
impl From<Primitive> for Type {
  fn from(p: Primitive) -> Self {
    Type::Primitive(p)
  }
}
impl From<FunctionType> for Type {
  fn from(func: FunctionType) -> Self {
    Type::Function(func.into())
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Primitive {
  String,
  Number,
  Boolean,
  True,
  False,
}
impl Primitive {
  fn matches(self, other: Self) -> bool {
    if matches!(self, Self::True | Self::False | Self::Boolean)
      && matches!(other, Self::True | Self::False | Self::Boolean)
    {
      return true;
    }

    self == other
  }

  fn is_truthy(self) -> bool {
    self == Self::True
  }

  fn is_falsy(self) -> bool {
    self == Self::False
  }
}
impl fmt::Display for Primitive {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Primitive::String => write!(f, "string"),
      Primitive::Number => write!(f, "number"),
      Primitive::Boolean => write!(f, "boolean"),
      Primitive::True => write!(f, "true"),
      Primitive::False => write!(f, "false"),
    }
  }
}

/// The type of a function
#[derive(Clone, Debug, PartialEq, Eq)]
struct FunctionType {
  parameter: Type,
  return_type: Type,
}

trait Typecheck {
  /// Check the type of a statement is valid, adding any errors to the Typechecker
  fn typecheck(&self, t: &mut Typechecker) -> Type;
}
trait TypecheckExpression {
  /// Get the value of an expression, and check if there are any errors in the expression
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError>;
}

impl Typecheck for AST<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Type {
    let type_ = self.statements.as_slice().typecheck(t);
    t.expand_type(type_)
  }
}
impl Typecheck for &[Statement<'_, '_>] {
  fn typecheck(&self, t: &mut Typechecker) -> Type {
    let (last, statements) = self.split_last().unwrap();
    for statement in statements {
      statement.typecheck(t);
    }

    last.typecheck(t)
  }
}

impl TypecheckExpression for Expression<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    match self {
      Expression::Binary(binary) => binary.typecheck(t),
      Expression::Block(block) => block.typecheck(t),
      Expression::Call(call) => call.typecheck(t),
      Expression::Comment(comment) => comment.typecheck(t),
      Expression::Function(function) => function.typecheck(t),
      Expression::Group(group) => group.typecheck(t),
      Expression::If(if_) => if_.typecheck(t),
      Expression::Literal(literal) => literal.typecheck(t),
      Expression::Match(match_) => match_.typecheck(t),
      Expression::Unary(unary) => unary.typecheck(t),
      Expression::Variable(variable) => variable.typecheck(t),
    }
  }
}
impl TypecheckExpression for Binary<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    let left = self.left.typecheck(t)?;
    let right = self.right.typecheck(t)?;

    match self.operator {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Remainder => {
        t.assert_type(left, Primitive::Number.into(), self.left.span())?;
        t.assert_type(right, Primitive::Number.into(), self.right.span())
      }
      BinaryOperator::AddString => {
        t.assert_type(left, Primitive::String.into(), self.left.span())?;
        t.assert_type(right, Primitive::String.into(), self.right.span())
      }
      BinaryOperator::NotEqual | BinaryOperator::Equal => {
        t.assert_type(right, left, self.span)?;
        Ok(Primitive::Boolean.into())
      }
      BinaryOperator::Greater
      | BinaryOperator::GreaterEqual
      | BinaryOperator::Less
      | BinaryOperator::LessEqual => {
        let expect: Type = if left.is_string() || right.is_string() {
          Primitive::String.into()
        } else {
          Primitive::Number.into()
        };

        t.assert_type(left, expect.clone(), self.left.span())?;
        t.assert_type(right, expect, self.right.span())?;
        Ok(Primitive::Boolean.into())
      }

      BinaryOperator::And => match left {
        left if left.is_falsy() => Ok(left),
        left if left.is_truthy() => Ok(right),
        _ => t.assert_type(left, right, self.span),
      },
      BinaryOperator::Or => match left {
        left if left.is_falsy() => Ok(right),
        left if left.is_truthy() => Ok(left),
        _ => t.assert_type(left, right, self.span),
      },
      BinaryOperator::Pipeline => check_call(t, &self.right, Some(&self.left), self.span),
    }
  }
}
impl TypecheckExpression for Block<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    t.new_scope();
    let last_expression_type = self.statements.as_slice().typecheck(t);
    t.end_scope();
    Ok(last_expression_type)
  }
}
impl TypecheckExpression for Call<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    check_call(t, &self.expression, self.argument.as_ref(), self.span)
  }
}
fn check_call(
  t: &mut Typechecker,
  expression: &Expression<'_, '_>,
  argument: Option<&Expression<'_, '_>>,
  span: Span,
) -> Result<Type, TypeError> {
  let expression_type = expression.typecheck(t)?;
  let argument = if let Some(argument) = argument {
    argument.typecheck(t)?
  } else {
    Type::Unknown
  };

  match expression_type {
    Type::Function(function) => {
      let parameter = t.expand_type(function.parameter.clone());
      if argument != Type::Unknown || !matches!(parameter, Type::Existential(_)) {
        t.assert_type(argument, parameter, span)?;
      }

      Ok(function.return_type.clone())
    }
    type_ => Err(TypeError::NotCallable {
      span: expression.span(),
      type_: t.expand_type(type_).to_string(),
    }),
  }
}
impl TypecheckExpression for Comment<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    self.expression.typecheck(t)
  }
}
impl TypecheckExpression for Function<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    t.new_scope();

    // define the parameter
    let parameter_type = t.new_type();
    t.add_variable(self.parameter.to_string(), parameter_type.clone());

    let func_type = FunctionType {
      parameter: parameter_type.clone(),
      return_type: t.new_type(),
    };

    // if function has a name, define it so recursive functions work
    if let Some(name) = self.name {
      t.add_variable(name.to_string(), func_type.clone().into());
    }

    // check the body doesn't have any errors
    let body_type = self.body.typecheck(t);
    let body_type = t.record_error(body_type);

    // set the function return type to the type of the body
    t.assert_type(func_type.return_type.clone(), body_type, self.span)?;

    t.end_scope();

    Ok(func_type.into())
  }
}
impl TypecheckExpression for Group<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    self.expression.typecheck(t)
  }
}
impl TypecheckExpression for If<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    self.condition.typecheck(t)?;
    let then = self.then.typecheck(t)?;
    let otherwise = self.otherwise.typecheck(t)?;

    let branch_type = then.expand_boolean();
    t.assert_type(otherwise, branch_type, self.otherwise.span())
  }
}
impl TypecheckExpression for Literal<'_> {
  fn typecheck(&self, _: &mut Typechecker) -> Result<Type, TypeError> {
    match self.kind {
      LiteralKind::String(_) => Ok(Primitive::String.into()),
      LiteralKind::Number { .. } => Ok(Primitive::Number.into()),
      LiteralKind::Boolean(value) => {
        if value {
          Ok(Primitive::True.into())
        } else {
          Ok(Primitive::False.into())
        }
      }
    }
  }
}
impl TypecheckExpression for Match<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    let value = self.value.typecheck(t)?;

    // check patterns match the value passes
    for case in &self.cases {
      match &case.pattern {
        Pattern::Identifier(_) => {}
        Pattern::Literal(literal) => {
          let literal = literal.typecheck(t).unwrap();
          t.assert_type(value.clone(), literal, case.span)?;
        }
        Pattern::Range(range) => {
          if let Some(start) = &range.start {
            let start_type = start.get_range_literal().typecheck(t).unwrap();
            t.assert_type(value.clone(), start_type, case.span)?;
          }
          if let Some(end) = &range.end {
            let end_type = end.get_range_literal().typecheck(t).unwrap();
            t.assert_type(value.clone(), end_type, case.span)?;
          }
        }
      }
    }

    // check all cases return the same type
    let (first, cases) = self.cases.split_first().unwrap();
    let return_type = first.expression.typecheck(t)?.expand_boolean();
    for case in cases {
      let type_ = case.expression.typecheck(t)?;
      t.assert_type(type_, return_type.clone(), case.expression.span())?;
    }

    // check the match statement is exhaustive
    match exhaustive::check(&value, &self.cases) {
      exhaustive::Result::Ok => {}
      exhaustive::Result::MissingCases(missing) => {
        t.errors
          .extend(missing.iter().map(|pattern| TypeError::MissingPattern {
            message: pattern.to_string(),
            span: self.span,
          }));
      }
      exhaustive::Result::UnreachableCases(unreachable) => t.errors.extend(
        unreachable
          .into_iter()
          .map(|span| TypeError::UnreachableCase { span }),
      ),
    };

    Ok(return_type)
  }
}
impl TypecheckExpression for Unary<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    let value = self.expression.typecheck(t)?;

    match self.operator {
      UnaryOperator::Not => match value {
        value if value.is_truthy() => Ok(Primitive::False.into()),
        value if value.is_falsy() => Ok(Primitive::True.into()),
        _ => Ok(Primitive::Boolean.into()),
      },
      UnaryOperator::Minus => {
        t.assert_type(value, Primitive::Number.into(), self.expression.span())
      }
    }
  }
}
impl TypecheckExpression for Variable<'_> {
  fn typecheck(&self, t: &mut Typechecker) -> Result<Type, TypeError> {
    for v in t.variables.iter().rev() {
      if v.identifier == self.name {
        return Ok(v.type_.clone());
      }
    }

    Err(TypeError::UndefinedVariable {
      span: self.span,
      identifier: self.name.to_string(),
    })
  }
}

impl Typecheck for Statement<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Type {
    match self {
      Statement::Comment(_) => Type::Unknown,
      Statement::Expression(expression) => {
        let type_ = expression.typecheck(t);
        t.record_error(type_)
      }
      Statement::Let(let_) => let_.typecheck(t),
    }
  }
}
impl Typecheck for Let<'_, '_> {
  fn typecheck(&self, t: &mut Typechecker) -> Type {
    let type_ = self.expression.typecheck(t);
    let type_ = t.record_error(type_);
    t.add_variable(self.identifier.to_string(), type_);
    Type::Unknown
  }
}

/// A error found by the Typechecker
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
  /// A variable which was not defined was used
  UndefinedVariable {
    /// The identifier of the variable
    identifier: String,
    /// The location of the variable
    span: Span,
  },
  /// Expected a type, but recieved a different one
  ExpectedDifferentType {
    /// The type that was expected
    expected: String,
    /// The type that was recieved
    given: String,
    /// The location of the error
    span: Span,
  },
  /// Type is not callable
  NotCallable {
    /// The type that was not callable
    type_: String,
    /// The location of the error
    span: Span,
  },

  /// Match statement arms are not exhaustive
  MissingPattern {
    /// A message describing what patterns are missing
    message: String,
    /// The location of the match statement
    span: Span,
  },

  /// Match statement arm is not reachable
  UnreachableCase {
    /// The location of the match statement arm
    span: Span,
  },
}
impl TypeError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::UndefinedVariable { .. } => "Undefined Variable",
      Self::ExpectedDifferentType { .. } => "Expected Different Type",
      Self::NotCallable { .. } => "Type Not Callable",
      Self::MissingPattern { .. } => "Match Not Exhaustive",
      Self::UnreachableCase { .. } => "Unreachable Case",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    match self {
      TypeError::UndefinedVariable { identifier, .. } => {
        format!("undefined variable `{identifier}`")
      }
      TypeError::ExpectedDifferentType {
        expected, given, ..
      } => format!("expected `{expected}`, but got `{given}`"),
      TypeError::NotCallable { type_, .. } => format!("`{type_}` is not callable"),
      TypeError::MissingPattern { message, .. } => message.clone(),
      TypeError::UnreachableCase { .. } => "case is already covered, so is unreachable".to_string(),
    }
  }
}
impl GetSpan for TypeError {
  fn span(&self) -> Span {
    match self {
      TypeError::UndefinedVariable { span, .. }
      | TypeError::ExpectedDifferentType { span, .. }
      | TypeError::NotCallable { span, .. }
      | TypeError::MissingPattern { span, .. }
      | TypeError::UnreachableCase { span, .. } => *span,
    }
  }
}
impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for TypeError {}
