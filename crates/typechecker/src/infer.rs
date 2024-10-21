use crate::{
  exhaustive,
  stdlib::{self, ImportResult},
  Enviroment, Type, TypeArena, TypeError, TypeRef, TypeScheme,
};
use bang_syntax::{
  ast::{expression::*, statement::*},
  Span, AST,
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

  pub fn check_ast(&mut self, ast: &AST) -> TypeRef {
    if ast.root_statements.is_empty() {
      return TypeArena::NEVER;
    }

    // Infer types
    self.env.enter_scope();
    let (last_statement, statements) = ast.root_statements.split_last().unwrap();
    for statement in statements {
      self.infer_statement(statement, ast);
    }
    let result = self.infer_statement(last_statement, ast);
    self
      .env
      .exit_scope(ast.root_statements.last().unwrap().span(ast));

    // Check for unused variables
    for variable in self.env.defined_variables() {
      if !variable.name.starts_with('_') && !variable.is_used() {
        if variable.is_import {
          self.problems.push(TypeError::UnusedImport {
            identifier: variable.name.clone(),
            span: variable.span(),
          });
        } else {
          self.problems.push(TypeError::UnusedVariable {
            identifier: variable.name.clone(),
            span: variable.span(),
          });
        }
      }
    }

    result.expression()
  }

  fn infer_statement(&mut self, statement: &Statement, ast: &AST) -> ExpressionType {
    match statement {
      Statement::Comment(_) => {}
      Statement::Expression(expression) => return expression.expression(ast).infer(self, ast),
      Statement::Import(import) => {
        for item in import.items(ast) {
          match stdlib::import_value(&mut self.types, import.module(ast), item.name) {
            ImportResult::Value(type_) => self.env.define_import(&item, type_),
            ImportResult::ModuleNotFound => {
              self.problems.push(TypeError::ModuleNotFound {
                module: import.module(ast).to_owned(),
                span: import.module_span(ast),
              });
            }
            ImportResult::ItemNotFound => {
              self.problems.push(TypeError::ItemNotFound {
                module: import.module(ast).to_owned(),
                item: item.name.to_owned(),
                span: item.span,
              });
            }
          };
        }
      }
      Statement::Let(let_) => {
        if let Expression::Function(_) = &let_.value(ast) {
          self.infer_let_with_function(let_, ast);
          return TypeArena::NEVER.into();
        }

        let variable_type = let_.value(ast).infer(self, ast).expression();

        self.env.define_variable(
          let_.identifier(ast),
          let_.identifier_span(ast),
          self.types.generalize(variable_type, self.env.depth()),
        );
      }
      Statement::Return(return_) => {
        return ExpressionType::return_(return_.expression(ast).infer(self, ast).expression())
      }
    };

    TypeArena::NEVER.into()
  }

  /// Infer the type of a let statement with a function
  ///
  /// Function might be recursive, so we need to define the variable before we infer the function type
  fn infer_let_with_function(&mut self, let_: &Let, ast: &AST) {
    // Define the function type
    let function_type = self.new_type_var();
    self.env.define_variable(
      let_.identifier(ast),
      let_.identifier_span(ast),
      TypeScheme::monomorphic(function_type),
    );

    // Infer the function type
    let expression_type = let_.value(ast).infer(self, ast).expression();

    // Update the variable type to be the inferred type
    self.env.update_variable(
      let_.identifier(ast),
      self.types.generalize(expression_type, self.env.depth()),
    );
  }

  /// Asserts that type `a` is applicable to type `b`
  fn assert_type(&mut self, a: TypeRef, b: TypeRef, span: Span) {
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
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType;
}

impl InferType for Expression {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    match self {
      Expression::Binary(binary) => binary.infer(t, ast),
      Expression::Block(block) => block.infer(t, ast),
      Expression::Call(call) => call.infer(t, ast),
      Expression::Comment(comment) => comment.infer(t, ast),
      Expression::FormatString(format_string) => format_string.infer(t, ast),
      Expression::Function(function) => function.infer(t, ast),
      Expression::Group(group) => group.infer(t, ast),
      Expression::If(if_) => if_.infer(t, ast),
      Expression::Literal(literal) => literal.infer(t, ast),
      Expression::Match(match_) => match_.infer(t, ast),
      Expression::Unary(unary) => unary.infer(t, ast),
      Expression::Variable(variable) => variable.infer(t, ast),
      Expression::Invalid(_) => TypeArena::UNKNOWN.into(),
    }
  }
}
impl InferType for Binary {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    let left = self.left(ast).infer(t, ast).expression();
    let right = self.right(ast).infer(t, ast).expression();

    match self.operator(ast) {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Remainder => {
        t.assert_type(left, TypeArena::NUMBER, self.left(ast).span(ast));
        t.assert_type(right, TypeArena::NUMBER, self.right(ast).span(ast));

        TypeArena::NUMBER.into()
      }
      BinaryOperator::AddString => {
        t.assert_type(left, TypeArena::STRING, self.left(ast).span(ast));
        t.assert_type(right, TypeArena::STRING, self.right(ast).span(ast));
        TypeArena::STRING.into()
      }
      BinaryOperator::NotEqual | BinaryOperator::Equal => {
        t.assert_type(right, left, self.right(ast).span(ast));
        TypeArena::BOOLEAN.into()
      }
      BinaryOperator::Greater
      | BinaryOperator::GreaterEqual
      | BinaryOperator::Less
      | BinaryOperator::LessEqual => {
        t.assert_type(right, left, self.right(ast).span(ast));
        TypeArena::BOOLEAN.into()
      }
      BinaryOperator::And | BinaryOperator::Or => {
        t.assert_type(right, left, self.right(ast).span(ast));
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
              span: self.left(ast).span(ast),
            }),
            None => {
              t.problems.push(TypeError::NotCallable {
                type_: t.types.type_to_string(callee_type),
                span: self.right(ast).span(ast),
              });
            }
          }
        }

        return_type.into()
      }
      BinaryOperator::InvalidSingleEqual => TypeArena::UNKNOWN.into(),
    }
  }
}
impl InferType for Block {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    t.env.enter_scope();

    let final_statement = self
      .statements(ast)
      .position(|statment| matches!(statment, Statement::Return(_)))
      .or(
        self
          .statements(ast)
          .enumerate()
          .rev()
          .find_map(|(id, statment)| matches!(statment, Statement::Expression(_)).then_some(id)),
      )
      .unwrap_or(self.len() - 1);

    let block_return_type = t.new_type_var();
    for (id, statement) in self.statements(ast).enumerate() {
      let statement_type = t.infer_statement(statement, ast);

      if let Some(return_type) = statement_type.get_return() {
        t.assert_type(return_type, block_return_type, statement.span(ast));
      }

      if id == final_statement {
        t.env.exit_scope(self.span(ast));
        return statement_type;
      }
    }

    t.env.exit_scope(self.span(ast));
    block_return_type.into()
  }
}
impl InferType for Call {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    let callee_type = self.callee(ast).infer(t, ast).expression();
    let argument_type = if let Some(argument) = self.argument(ast) {
      argument.infer(t, ast).expression()
    } else {
      TypeArena::NEVER
    };

    let return_type = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(argument_type, return_type));

    if t.types.unify(function_type, callee_type).is_err() {
      match t.types.function_parameter(callee_type) {
        Some(expected) if self.argument(ast).is_none() => {
          t.problems.push(TypeError::MissingArgument {
            expected: t.types.type_to_string(expected),
            span: self.argument_span(ast),
          });
        }
        Some(expected) => t.problems.push(TypeError::IncorrectArgument {
          given: t.types.type_to_string(argument_type),
          expected: t.types.type_to_string(expected),
          span: self.argument(ast).unwrap().span(ast),
        }),
        None => t.problems.push(TypeError::NotCallable {
          type_: t.types.type_to_string(callee_type),
          span: self.callee(ast).span(ast),
        }),
      }
    }

    return_type.into()
  }
}
impl InferType for Comment {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    self.expression(ast).infer(t, ast)
  }
}
impl InferType for FormatString {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    for expression in self.expressions(ast) {
      expression.infer(t, ast);
    }

    TypeArena::STRING.into()
  }
}
impl InferType for Function {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    t.env.enter_scope();

    let parameter = t.new_type_var();
    let expected_return = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(parameter, expected_return));

    t.env.define_variable(
      self.parameter.name(ast),
      self.parameter.span(ast),
      TypeScheme::monomorphic(parameter),
    );
    let return_type = match self.body(ast).infer(t, ast) {
      ExpressionType::Expression(ty) | ExpressionType::Return(ty) => ty,
      ExpressionType::Both(expression_type, return_type) => {
        t.assert_type(expression_type, return_type, self.span(ast));
        expression_type
      }
    };
    t.types.unify(return_type, expected_return).unwrap();

    if t.types[return_type] == Type::Primitive(TypeArena::NEVER) {
      t.problems.push(TypeError::FunctionReturnsNever {
        span: self.span(ast),
      });
    }

    t.env.exit_scope(self.span(ast));

    function_type.into()
  }
}
impl InferType for Group {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    self.expression(ast).infer(t, ast)
  }
}
impl InferType for If {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    self.condition(ast).infer(t, ast);

    let then_type = self.then(ast).infer(t, ast);

    if let Some(otherwise) = self.otherwise(ast) {
      let else_type = otherwise.infer(t, ast);
      t.merge_branches(&then_type, &else_type, self.span(ast))
    } else {
      TypeArena::NEVER.into()
    }
  }
}
impl InferType for Literal {
  fn infer(&self, _: &mut Typechecker, ast: &AST) -> ExpressionType {
    match self.value(ast) {
      LiteralValue::Boolean(_) => TypeArena::BOOLEAN,
      LiteralValue::Number { .. } => TypeArena::NUMBER,
      LiteralValue::String(_) => TypeArena::STRING,
    }
    .into()
  }
}
impl InferType for Match {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    let value_type = self.value(ast).infer(t, ast).expression();

    // check patterns match the value passes
    for case in self.arms() {
      match &case.pattern {
        Pattern::Identifier(_) | Pattern::Invalid => {}
        Pattern::Literal(literal) => {
          let literal = literal.infer(t, ast).expression();
          if t.types.unify(literal, value_type).is_err() {
            t.problems.push(TypeError::PatternNeverMatches {
              pattern: t.types.type_to_string(literal),
              value: t.types.type_to_string(value_type),
              span: case.pattern.span(ast),
            });
          }
        }
        Pattern::Range(start, end) => {
          if let Some(start) = start {
            let start_type = start.infer(t, ast).expression();
            if t.types.unify(start_type, value_type).is_err() {
              t.problems.push(TypeError::PatternNeverMatches {
                pattern: t.types.type_to_string(start_type),
                value: t.types.type_to_string(value_type),
                span: case.pattern.span(ast),
              });
            }
          }
          if let Some(end) = end {
            let end_type = end.infer(t, ast).expression();
            if t.types.unify(end_type, value_type).is_err() {
              t.problems.push(TypeError::PatternNeverMatches {
                pattern: t.types.type_to_string(end_type),
                value: t.types.type_to_string(value_type),
                span: case.pattern.span(ast),
              });
            }
          }
        }
      }
    }

    // check all cases return the same type
    let mut return_type = t.new_type_var().into();
    for case in self.arms() {
      t.env.enter_scope();

      if let Pattern::Identifier(identifier) = &case.pattern {
        t.env.define_variable(
          identifier.name(ast),
          identifier.span(ast),
          TypeScheme::monomorphic(value_type),
        );
      }

      // check the guard is valid, needs the value to be defined
      if let Some(guard) = case.guard(ast) {
        guard.infer(t, ast);
      }

      let case_type = case.expression(ast).infer(t, ast);
      return_type = t.merge_branches(&return_type, &case_type, self.span(ast));

      t.env.exit_scope(self.span(ast));
    }

    // check the match statement is exhaustive
    match exhaustive::check(&t.types[value_type], self.arms(), ast) {
      exhaustive::Result::Ok => {}
      exhaustive::Result::MissingCases(missing) => {
        t.problems
          .extend(missing.iter().map(|pattern| TypeError::MissingPattern {
            message: pattern.to_string(),
            span: self.span(ast),
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
impl InferType for Unary {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    let left = self.expression(ast).infer(t, ast).expression();
    let span = self.expression(ast).span(ast);

    match self.operator(ast) {
      UnaryOperator::Not => TypeArena::BOOLEAN.into(),
      UnaryOperator::Minus => {
        t.assert_type(left, TypeArena::NUMBER, span);
        TypeArena::NUMBER.into()
      }
    }
  }
}
impl InferType for Variable {
  fn infer(&self, t: &mut Typechecker, ast: &AST) -> ExpressionType {
    if let Some(type_) = t.env.get_variable(self.name(ast)) {
      t.env.mark_variable_use(self.name(ast), self.span(ast));
      t.types.instantiate(type_, t.env.depth()).into()
    } else {
      t.problems.push(TypeError::UndefinedVariable {
        identifier: self.name(ast).to_owned(),
        span: self.span(ast),
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
