use crate::{exhaustive, Enviroment, Type, TypeArena, TypeError, TypeRef, TypeScheme};
use bang_parser::{
  ast::{expression::*, statement::*},
  GetSpan, Span, AST,
};

pub struct Typechecker {
  pub types: TypeArena,
  pub env: Enviroment,
  pub problems: Vec<TypeError>,
}
impl Typechecker {
  pub fn new() -> Self {
    let mut types = TypeArena::new();
    let mut env = Enviroment::new();

    env.define_builtin_variables(&mut types);

    Self {
      types,
      env,
      problems: Vec::new(),
    }
  }

  pub fn check_ast(&mut self, ast: &AST<'_, '_>) -> TypeRef {
    if ast.statements.is_empty() {
      return TypeArena::NEVER;
    }

    // Infer types
    self.env.enter_scope();
    let (last_statement, statements) = ast.statements.split_last().unwrap();
    for statement in statements {
      self.infer_statement(statement);
    }
    let result = self.infer_statement(last_statement);
    self.env.exit_scope(ast.statements.last().unwrap().span());

    // Check for unused variables
    for variable in self.env.defined_variables() {
      if !variable.name.starts_with('_') && !variable.is_used() {
        self.problems.push(TypeError::UnusedVariable {
          identifier: variable.name.clone(),
          span: variable.span(),
        });
      }
    }

    result.expression()
  }

  fn infer_statement(&mut self, statement: &Statement<'_, '_>) -> ExpressionType {
    match statement {
      Statement::Comment(_) => {}
      Statement::Expression(expression) => return expression.infer(self),
      Statement::Let(let_) => {
        if let Expression::Function(_) = &let_.expression {
          self.infer_let_with_function(let_);
          return TypeArena::NEVER.into();
        }

        let variable_type = let_.expression.infer(self).expression();

        self.env.define_variable(
          &let_.identifier,
          self.types.generalize(variable_type, self.env.depth()),
        );
      }
      Statement::Return(return_) => {
        return ExpressionType::return_(return_.expression.infer(self).expression())
      }
    };

    TypeArena::NEVER.into()
  }

  /// Infer the type of a let statement with a function
  ///
  /// Function might be recursive, so we need to define the variable before we infer the function type
  fn infer_let_with_function(&mut self, let_: &Let<'_, '_>) {
    let identifier = &let_.identifier;

    // Define the function type
    let function_type = self.new_type_var();
    self
      .env
      .define_variable(&let_.identifier, TypeScheme::monomorphic(function_type));

    // Infer the function type
    let expression_type = let_.expression.infer(self).expression();

    // Update the variable type to be the inferred type
    self.env.update_variable(
      identifier.name,
      self.types.generalize(expression_type, self.env.depth()),
    );
  }

  /// Asserts that type `a` is applicable to type `b`
  pub fn assert_type(&mut self, a: TypeRef, b: TypeRef, span: Span) {
    if self.types.unify(a, b).is_err() {
      self.problems.push(TypeError::ExpectedDifferentType {
        expected: self.types.type_to_string(b),
        given: self.types.type_to_string(a),
        span,
      });
    }
  }

  fn merge_branches(
    &mut self,
    then_type: &ExpressionType,
    else_type: &ExpressionType,
    span: Span,
  ) -> ExpressionType {
    let expression_type = match (then_type.get_expression(), else_type.get_expression()) {
      (None, None) => None,
      (None, Some(ty)) | (Some(ty), None) => Some(ty),
      (Some(then), Some(else_)) => {
        if self.types.unify(then, else_).is_err() {
          self.problems.push(TypeError::BranchesDontMatch {
            then: self.types.type_to_string(then),
            otherwise: self.types.type_to_string(else_),
            span,
          });
        };
        Some(then)
      }
    };
    let return_type = match (then_type.get_return(), else_type.get_return()) {
      (None, None) => None,
      (None, Some(ty)) | (Some(ty), None) => Some(ty),
      (Some(then), Some(else_)) => {
        if self.types.unify(then, else_).is_err() {
          self.problems.push(TypeError::BranchesDontMatch {
            then: self.types.type_to_string(then),
            otherwise: self.types.type_to_string(else_),
            span,
          });
        };
        Some(then)
      }
    };

    match (expression_type, return_type) {
      (Some(expression_type), Some(return_type)) => {
        ExpressionType::Both(expression_type, return_type)
      }
      (Some(expression_type), None) => ExpressionType::Expression(expression_type),
      (None, Some(return_type)) => ExpressionType::Return(return_type),
      (None, None) => unreachable!(),
    }
  }

  fn new_type_var(&mut self) -> TypeRef {
    self.types.new_type_var(self.env.depth())
  }
}

pub(crate) trait InferType {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType;
}

impl InferType for Expression<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    match self {
      Expression::Binary(binary) => binary.infer(t),
      Expression::Block(block) => block.infer(t),
      Expression::Call(call) => call.infer(t),
      Expression::Comment(comment) => comment.infer(t),
      Expression::FormatString(format_string) => format_string.infer(t),
      Expression::Function(function) => function.infer(t),
      Expression::Group(group) => group.infer(t),
      Expression::If(if_) => if_.infer(t),
      Expression::Literal(literal) => literal.infer(t),
      Expression::Match(match_) => match_.infer(t),
      Expression::Unary(unary) => unary.infer(t),
      Expression::Variable(variable) => variable.infer(t),
      Expression::Invalid(_) => TypeArena::UNKNOWN.into(),
    }
  }
}
impl InferType for Binary<'_, '_> {
  #[allow(clippy::match_same_arms)]
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    let left = self.left.infer(t).expression();
    let right = self.right.infer(t).expression();

    match self.operator {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Remainder => {
        t.assert_type(left, TypeArena::NUMBER, self.left.span());
        t.assert_type(right, TypeArena::NUMBER, self.right.span());

        TypeArena::NUMBER.into()
      }
      BinaryOperator::AddString => {
        t.assert_type(left, TypeArena::STRING, self.left.span());
        t.assert_type(right, TypeArena::STRING, self.right.span());
        TypeArena::STRING.into()
      }
      BinaryOperator::NotEqual | BinaryOperator::Equal => {
        t.assert_type(right, left, self.right.span());
        TypeArena::BOOLEAN.into()
      }
      BinaryOperator::Greater
      | BinaryOperator::GreaterEqual
      | BinaryOperator::Less
      | BinaryOperator::LessEqual => {
        t.assert_type(right, left, self.right.span());
        TypeArena::BOOLEAN.into()
      }
      BinaryOperator::And | BinaryOperator::Or => {
        t.assert_type(right, left, self.right.span());
        right.into()
      }
      BinaryOperator::Pipeline => {
        let callee_type = right;
        let argument_type = left;

        let return_type = t.new_type_var();
        let function_type = t.types.new_type(Type::Function(argument_type, return_type));

        if t.types.unify(function_type, callee_type).is_err() {
          match t.types.function_parameter(callee_type) {
            Some(expected) => t.problems.push(TypeError::IncorrectArgument {
              given: t.types.type_to_string(argument_type),
              expected: t.types.type_to_string(expected),
              span: self.left.span(),
            }),
            None => {
              t.problems.push(TypeError::NotCallable {
                type_: t.types.type_to_string(callee_type),
                span: self.right.span(),
              });
            }
          }
        }

        return_type.into()
      }
      BinaryOperator::Invalid => TypeArena::UNKNOWN.into(),
    }
  }
}
impl InferType for Block<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    t.env.enter_scope();

    let final_statement = self
      .statements
      .iter()
      .position(|statment| matches!(statment, Statement::Return(_)))
      .or(
        self
          .statements
          .iter()
          .enumerate()
          .rev()
          .find_map(|(id, statment)| matches!(statment, Statement::Expression(_)).then_some(id)),
      )
      .expect("block ends with an expression");

    let block_return_type = t.new_type_var();
    for (id, statement) in self.statements.iter().enumerate() {
      let statement_type = t.infer_statement(statement);

      if let Some(return_type) = statement_type.get_return() {
        t.assert_type(return_type, block_return_type, statement.span());
      }

      if id == final_statement {
        t.env.exit_scope(self.span());
        return statement_type;
      }
    }

    t.env.exit_scope(self.span());
    block_return_type.into()
  }
}
impl InferType for Call<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    let callee_type = self.expression.infer(t).expression();
    let argument_type = if let Some(argument) = &self.argument {
      argument.infer(t).expression()
    } else {
      TypeArena::NEVER
    };

    let return_type = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(argument_type, return_type));

    if t.types.unify(function_type, callee_type).is_err() {
      match t.types.function_parameter(callee_type) {
        Some(expected) if self.argument.is_none() => t.problems.push(TypeError::MissingArgument {
          expected: t.types.type_to_string(expected),
          span: self.argument_span,
        }),
        Some(expected) => t.problems.push(TypeError::IncorrectArgument {
          given: t.types.type_to_string(argument_type),
          expected: t.types.type_to_string(expected),
          span: self.argument.as_ref().unwrap().span(),
        }),
        None => t.problems.push(TypeError::NotCallable {
          type_: t.types.type_to_string(callee_type),
          span: self.expression.span(),
        }),
      }
    }

    return_type.into()
  }
}
impl InferType for Comment<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    self.expression.infer(t)
  }
}
impl InferType for FormatString<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    for expression in &self.expressions {
      expression.infer(t);
    }

    TypeArena::STRING.into()
  }
}
impl InferType for Function<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    t.env.enter_scope();

    let parameter = t.new_type_var();
    let expected_return = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(parameter, expected_return));

    t.env
      .define_variable(&self.parameter, TypeScheme::monomorphic(parameter));
    let return_type = match self.body.infer(t) {
      ExpressionType::Expression(ty) | ExpressionType::Return(ty) => ty,
      ExpressionType::Both(expression_type, return_type) => {
        t.assert_type(expression_type, return_type, self.span());
        expression_type
      }
    };
    t.types.unify(return_type, expected_return).unwrap();

    if t.types[return_type] == Type::Primitive(TypeArena::NEVER) {
      t.problems
        .push(TypeError::FunctionReturnsNever { span: self.span() });
    }

    t.env.exit_scope(self.span());

    function_type.into()
  }
}
impl InferType for Group<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    self.expression.infer(t)
  }
}
impl InferType for If<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    self.condition.infer(t);

    let then_type = self.then.infer(t);

    if let Some(otherwise) = &self.otherwise {
      let else_type = otherwise.infer(t);
      t.merge_branches(&then_type, &else_type, self.span())
    } else {
      TypeArena::NEVER.into()
    }
  }
}
impl InferType for Literal<'_> {
  fn infer(&self, _: &mut Typechecker) -> ExpressionType {
    match self.kind {
      LiteralKind::Boolean(_) => TypeArena::BOOLEAN,
      LiteralKind::Number { .. } => TypeArena::NUMBER,
      LiteralKind::String(_) => TypeArena::STRING,
    }
    .into()
  }
}
impl InferType for Match<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    let value_type = self.value.infer(t).expression();

    // check patterns match the value passes
    for case in &self.cases {
      match &case.pattern {
        Pattern::Identifier(_) => {}
        Pattern::Literal(literal) => {
          let literal = literal.infer(t).expression();
          if t.types.unify(literal, value_type).is_err() {
            t.problems.push(TypeError::PatternNeverMatches {
              pattern: t.types.type_to_string(literal),
              value: t.types.type_to_string(value_type),
              span: case.pattern.span(),
            });
          }
        }
        Pattern::Range(range) => {
          if let Some(start) = &range.start {
            let start_type = start.get_range_literal().infer(t).expression();
            if t.types.unify(start_type, value_type).is_err() {
              t.problems.push(TypeError::PatternNeverMatches {
                pattern: t.types.type_to_string(start_type),
                value: t.types.type_to_string(value_type),
                span: case.pattern.span(),
              });
            }
          }
          if let Some(end) = &range.end {
            let end_type = end.get_range_literal().infer(t).expression();
            if t.types.unify(end_type, value_type).is_err() {
              t.problems.push(TypeError::PatternNeverMatches {
                pattern: t.types.type_to_string(end_type),
                value: t.types.type_to_string(value_type),
                span: case.pattern.span(),
              });
            }
          }
        }
      }
    }

    // check all cases return the same type
    let mut return_type = t.new_type_var().into();
    for case in &self.cases {
      t.env.enter_scope();

      if let Pattern::Identifier(identifier) = &case.pattern {
        t.env
          .define_variable(identifier, TypeScheme::monomorphic(value_type));
      }

      // check the guard is valid, needs the value to be defined
      if let Some(guard) = &case.guard {
        guard.infer(t);
      }

      let case_type = case.expression.infer(t);
      return_type = t.merge_branches(&return_type, &case_type, self.span());

      t.env.exit_scope(self.span());
    }

    // check the match statement is exhaustive
    match exhaustive::check(&t.types[value_type], &self.cases) {
      exhaustive::Result::Ok => {}
      exhaustive::Result::MissingCases(missing) => {
        t.problems
          .extend(missing.iter().map(|pattern| TypeError::MissingPattern {
            message: pattern.to_string(),
            span: self.span,
          }));
      }
      exhaustive::Result::UnreachableCases(unreachable) => t.problems.extend(
        unreachable
          .into_iter()
          .map(|span| TypeError::UnreachableCase { span }),
      ),
    };

    return_type
  }
}
impl InferType for Unary<'_, '_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    let left = self.expression.infer(t).expression();
    let span = self.expression.span();

    match self.operator {
      UnaryOperator::Not => TypeArena::BOOLEAN.into(),
      UnaryOperator::Minus => {
        t.assert_type(left, TypeArena::NUMBER, span);
        TypeArena::NUMBER.into()
      }
    }
  }
}
impl InferType for Variable<'_> {
  fn infer(&self, t: &mut Typechecker) -> ExpressionType {
    if let Some(type_) = t.env.get_variable(self.name) {
      t.env.mark_variable_use(self.name, self.span());
      t.types.instantiate(type_, t.env.depth()).into()
    } else {
      t.problems.push(TypeError::UndefinedVariable {
        identifier: self.name.to_owned(),
        span: self.span,
      });
      TypeArena::UNKNOWN.into()
    }
  }
}

/// The type of an expression
///
/// An expression can have a regular expression type,
/// or it can return a from a function which may have a different type
pub(crate) enum ExpressionType {
  Expression(TypeRef),
  Return(TypeRef),
  Both(TypeRef, TypeRef),
}
impl ExpressionType {
  /// Create a type which returns a value
  fn return_(ty: TypeRef) -> Self {
    Self::Return(ty)
  }

  /// Get the expression type
  /// If it doesn't have an expression type, put unknown
  fn expression(&self) -> TypeRef {
    match self {
      Self::Expression(ty) => *ty,
      Self::Return(_) => TypeArena::UNKNOWN,
      Self::Both(expression, _) => *expression,
    }
  }

  fn get_expression(&self) -> Option<TypeRef> {
    match self {
      Self::Expression(ty) => Some(*ty),
      Self::Return(_) => None,
      Self::Both(expression, _) => Some(*expression),
    }
  }
  fn get_return(&self) -> Option<TypeRef> {
    match self {
      Self::Expression(_) => None,
      Self::Return(ty) => Some(*ty),
      Self::Both(_, return_) => Some(*return_),
    }
  }
}
impl From<TypeRef> for ExpressionType {
  fn from(ty: TypeRef) -> Self {
    Self::Expression(ty)
  }
}
