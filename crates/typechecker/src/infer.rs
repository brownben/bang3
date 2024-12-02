use crate::{
  TypeChecker, TypeError, VariableKind, exhaustive,
  similarity::similarly_named,
  stdlib::{self, ImportResult, StdlibModule},
  types::{Structure, Type, TypeArena, TypeRef, TypeScheme},
};
use bang_syntax::{
  AST, Span,
  ast::{self as ast, expression::*, statement::*, types::Type as Annotation},
};
use std::collections::BTreeMap;

impl TypeChecker {
  fn check_unused_variables(&mut self) {
    self.problems.extend(
      (self.env.finished_variables())
        .filter(|v| !v.name().starts_with('_'))
        .filter(|v| !v.is_used())
        .filter_map(|variable| match variable.kind {
          VariableKind::Declaration { defined, .. } => Some(TypeError::UnusedVariable {
            identifier: variable.name().to_owned(),
            span: defined,
          }),
          VariableKind::Import { defined, .. } => Some(TypeError::UnusedImport {
            identifier: variable.name().to_owned(),
            span: defined,
          }),
          VariableKind::Builtin { .. } => None,
        }),
    );
  }

  fn new_type_var(&mut self) -> TypeRef {
    self.types.new_type_var()
  }

  pub(crate) fn type_from_annotation(&mut self, ty: &ast::Type, ast: &AST) -> TypeRef {
    let mut variables = BTreeMap::new();
    self.type_from_annotation_inner(ty, ast, &mut variables)
  }

  const PRIMITIVE_NAMES: [&str; 4] = ["number", "string", "boolean", "_"];
  const STRUCTURE_NAMES: [&str; 1] = ["list"];

  fn type_from_annotation_inner<'a>(
    &mut self,
    ty: &ast::Type,
    ast: &'a AST,
    variables: &mut BTreeMap<&'a str, TypeRef>,
  ) -> TypeRef {
    match ty {
      Annotation::Primitive(primitive) => {
        self.primitive_type_from_annotation(primitive.name(ast), primitive.span(ast))
      }
      Annotation::Variable(variable) => {
        let type_ = variables.get(variable.name(ast));
        if let Some(type_) = type_ {
          *type_
        } else {
          let type_var = self.new_type_var();
          variables.insert(variable.name(ast), type_var);
          type_var
        }
      }
      Annotation::Function(function) => {
        let parameter = self.type_from_annotation_inner(function.parameter(ast), ast, variables);
        let return_ = self.type_from_annotation_inner(function.return_(ast), ast, variables);
        self.types.new_type(Type::Function(parameter, return_))
      }
      Annotation::Group(group) => self.type_from_annotation_inner(group.type_(ast), ast, variables),
      Annotation::Structure(parameter) => match parameter.structure(ast) {
        "list" => {
          let type_ = self.type_from_annotation_inner(parameter.parameter(ast), ast, variables);
          self.types.new_type(Type::Structure(Structure::List, type_))
        }
        name if Self::PRIMITIVE_NAMES.contains(&name) => {
          self.problems.push(TypeError::UnexpectedParameter {
            type_: name.to_owned(),
            span: parameter.span(ast),
          });
          self.primitive_type_from_annotation(name, parameter.span(ast))
        }
        name => {
          self.problems.push(TypeError::UnknownTypeAnnotation {
            span: parameter.span(ast),
            did_you_mean: similarly_named(name, Self::STRUCTURE_NAMES),
          });
          TypeArena::UNKNOWN
        }
      },
      Annotation::Invalid(_) => TypeArena::UNKNOWN,
    }
  }

  fn primitive_type_from_annotation(&mut self, primitive: &str, span: Span) -> TypeRef {
    match primitive {
      "number" => TypeArena::NUMBER,
      "string" => TypeArena::STRING,
      "boolean" => TypeArena::BOOLEAN,
      "list" => {
        // If it doesn't have a parameter, make it generic
        let type_ = self.new_type_var();
        self.types.new_type(Type::Structure(Structure::List, type_))
      }
      "_" => TypeArena::NEVER,
      _ => {
        self.problems.push(TypeError::UnknownTypeAnnotation {
          span,
          did_you_mean: similarly_named(primitive, Self::PRIMITIVE_NAMES),
        });
        TypeArena::UNKNOWN
      }
    }
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

  /// Asserts that type `a` is always applicable to type `b`
  /// Ensures that type variables cannot be passed to a concrete type `b`
  fn assert_type_strict(&mut self, a: TypeRef, b: TypeRef, span: Span) {
    if self.types.unify_strict(a, b).is_err() {
      self.problems.push(TypeError::ExpectedDifferentType {
        expected: self.types.type_to_string(a),
        given: self.types.type_to_string(b),
        span,
      });
    }
  }

  /// Merges two branches together, to get the combined [`ExpressionType`]
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
}

pub(crate) trait InferType {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType;
}
pub(crate) trait InferExpression {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> TypeRef;
}

impl InferType for AST {
  fn infer(&self, t: &mut TypeChecker, _ast: &AST) -> ExpressionType {
    if self.root_statements.is_empty() {
      return ExpressionType::NEVER;
    }

    t.env.enter_scope();
    t.env.define_builtin_variables(&mut t.types);

    let (last_statement, statements) = self.root_statements.split_last().unwrap();
    for statement in statements {
      _ = statement.infer(t, self);
    }
    let result = last_statement.infer(t, self);
    let end_span = self.root_statements.last().unwrap().span(self);
    t.env.exit_scope(end_span);

    // Check for unused variables
    t.check_unused_variables();

    result
  }
}

impl InferType for Statement {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    match self {
      Statement::Comment(_) => ExpressionType::NEVER,
      Statement::Expression(expression) => expression.expression(ast).infer(t, ast),
      Statement::Import(import) => ExpressionType::Expression(import.infer(t, ast)),
      Statement::Let(let_) => let_.infer(t, ast),
      Statement::Return(return_) => return_.infer(t, ast),
    }
  }
}
impl InferExpression for Import {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> TypeRef {
    let module = StdlibModule::get(self.module(ast));

    for item in self.items(ast) {
      match module.import_value(&mut t.types, item.name) {
        ImportResult::Value(type_) => t.env.define_import(&item, type_, module.name()),
        ImportResult::ModuleNotFound => {
          t.problems.push(TypeError::ModuleNotFound {
            module: self.module(ast).to_owned(),
            span: self.module_span(ast),
            did_you_mean: similarly_named(self.module(ast), stdlib::MODULES),
          });
        }
        ImportResult::ItemNotFound => {
          t.problems.push(TypeError::ItemNotFound {
            module: self.module(ast).to_owned(),
            item: item.name.to_owned(),
            span: item.span,
            did_you_mean: similarly_named(item.name, module.items().iter().copied()),
          });
        }
      };
    }

    TypeArena::NEVER
  }
}
impl InferType for Let {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    if let Expression::Function(function) = self.value(ast) {
      return let_statement_function(self, function, t, ast);
    }

    let (mut ty, return_type) = match self.value(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, None),
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(ty, return_ty) => (ty, Some(return_ty)),
    };
    if let Some(annotation) = self.annotation(ast) {
      let annotation_type = t.type_from_annotation(annotation, ast);

      if annotation_type != TypeArena::UNKNOWN {
        t.assert_type_strict(annotation_type, ty, self.value(ast).span(ast));
        ty = annotation_type;
      }
    }

    let (identifier, identifier_span) = (self.identifier(ast), self.identifier_span(ast));
    let doc_comment = self.doc_comment(ast).map(|comment| comment.full_text(ast));

    t.env.define_variable(
      identifier,
      identifier_span,
      TypeScheme::monomorphic(ty),
      doc_comment,
    );
    ExpressionType::from(TypeArena::NEVER, return_type)
  }
}
fn let_statement_function(
  let_: &Let,
  function: &Function,
  t: &mut TypeChecker,
  ast: &AST,
) -> ExpressionType {
  let (identifier, identifier_span) = (let_.identifier(ast), let_.identifier_span(ast));
  let doc_comment = let_.doc_comment(ast).map(|comment| comment.full_text(ast));

  let function_type = t.new_type_var();
  t.env.define_variable(
    identifier,
    identifier_span,
    TypeScheme::monomorphic(function_type),
    doc_comment,
  );

  // Infer the function type
  let mut ty = function.infer(t, ast);
  if let Some(annotation) = let_.annotation(ast) {
    let annotation_type = t.type_from_annotation(annotation, ast);

    if annotation_type != TypeArena::UNKNOWN {
      t.assert_type_strict(annotation_type, ty, let_.value(ast).span(ast));
      ty = annotation_type;
    }
  }

  // Update the variable type to be the inferred type
  t.env.update_variable(identifier, t.types.generalize(ty));
  ExpressionType::NEVER
}
impl InferType for Return {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    match self.expression(ast).infer(t, ast) {
      ExpressionType::Expression(ty) | ExpressionType::Return(ty) => ExpressionType::Return(ty),
      ExpressionType::Both(a, b) => {
        t.assert_type(a, b, self.span(ast));
        ExpressionType::Return(b)
      }
    }
  }
}

impl InferType for Expression {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    match self {
      Expression::Binary(binary) => binary.infer(t, ast),
      Expression::Block(block) => block.infer(t, ast),
      Expression::Call(call) => call.infer(t, ast),
      Expression::Comment(comment) => comment.infer(t, ast),
      Expression::FormatString(format_string) => format_string.infer(t, ast),
      Expression::Group(group) => group.infer(t, ast),
      Expression::If(if_) => if_.infer(t, ast),
      Expression::List(list) => list.infer(t, ast),
      Expression::Match(match_) => match_.infer(t, ast),
      Expression::Unary(unary) => unary.infer(t, ast),

      // Expressions which only have an expression, and can never return
      Expression::Function(function) => ExpressionType::Expression(function.infer(t, ast)),
      Expression::Literal(literal) => ExpressionType::Expression(literal.infer(t, ast)),
      Expression::ModuleAccess(module) => ExpressionType::Expression(module.infer(t, ast)),
      Expression::Variable(variable) => ExpressionType::Expression(variable.infer(t, ast)),
      Expression::Invalid(_) => ExpressionType::Expression(TypeArena::UNKNOWN),
    }
  }
}
impl InferType for Binary {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let (left, return_ty) = match self.left(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, None),
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(ty, return_ty) => (ty, Some(return_ty)),
    };
    let (right, return_ty) = match self.right(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, return_ty),
      ExpressionType::Return(ty) => {
        if let Some(return_ty) = return_ty {
          t.assert_type(ty, return_ty, self.right(ast).span(ast));
        }
        return ExpressionType::Return(ty);
      }
      ExpressionType::Both(ty, return_) => {
        if let Some(return_ty) = return_ty {
          t.assert_type(return_, return_ty, self.right(ast).span(ast));
        }
        (ty, Some(return_))
      }
    };

    let expression_ty = match self.operator(ast) {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Remainder => {
        t.assert_type(left, TypeArena::NUMBER, self.left(ast).span(ast));
        t.assert_type(right, TypeArena::NUMBER, self.right(ast).span(ast));

        TypeArena::NUMBER
      }
      BinaryOperator::AddString => {
        t.assert_type(left, TypeArena::STRING, self.left(ast).span(ast));
        t.assert_type(right, TypeArena::STRING, self.right(ast).span(ast));
        TypeArena::STRING
      }
      BinaryOperator::NotEqual | BinaryOperator::Equal => {
        t.assert_type(right, left, self.right(ast).span(ast));
        TypeArena::BOOLEAN
      }
      BinaryOperator::Greater
      | BinaryOperator::GreaterEqual
      | BinaryOperator::Less
      | BinaryOperator::LessEqual => {
        t.assert_type(right, left, self.right(ast).span(ast));
        TypeArena::BOOLEAN
      }
      BinaryOperator::And | BinaryOperator::Or => {
        t.assert_type(right, left, self.right(ast).span(ast));
        right
      }
      BinaryOperator::Pipeline => {
        let callee_type = right;
        let argument_type = left;

        let return_type = t.new_type_var();
        let function_type = t.types.new_type(Type::Function(argument_type, return_type));

        if t.types.unify(function_type, callee_type).is_err() {
          match t.types.function_parameter(callee_type) {
            Some(expected) if t.types.is_never(expected) => {
              t.problems.push(TypeError::ExtraArgument {
                given: t.types.type_to_string(argument_type),
                span: self.left(ast).span(ast),
              });
            }
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

        return_type
      }
      BinaryOperator::InvalidSingleEqual => TypeArena::UNKNOWN,
    };

    ExpressionType::from(expression_ty, return_ty)
  }
}
impl InferType for Block {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    t.env.enter_scope();

    let final_statement = (self.statements(ast).enumerate().rev())
      .find_map(|(id, statment)| matches!(statment, Statement::Expression(_)).then_some(id))
      .unwrap_or(self.len() - 1);

    let mut block_ty = None;
    let mut return_ty = None;

    for (id, statement) in self.statements(ast).enumerate() {
      match statement.infer(t, ast) {
        ExpressionType::Expression(ty) if id == final_statement => block_ty = Some(ty),
        ExpressionType::Expression(_) => {}
        ExpressionType::Return(ty) => {
          t.env.exit_scope(self.span(ast));
          return ExpressionType::Return(ty);
        }
        ExpressionType::Both(ty, return_) => {
          if let Some(return_ty) = return_ty {
            t.assert_type(return_, return_ty, self.span(ast));
          } else {
            return_ty = Some(return_);
          }

          if id == final_statement {
            block_ty = Some(ty);
          }
        }
      }
    }

    t.env.exit_scope(self.span(ast));
    ExpressionType::from(block_ty.unwrap(), return_ty)
  }
}
impl InferType for Call {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let (callee_type, return_ty) = match self.callee(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, None),
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(ty, return_ty) => (ty, Some(return_ty)),
    };
    let (argument_type, return_ty) = match self.argument(ast).map(|arg| arg.infer(t, ast)) {
      Some(ExpressionType::Expression(ty)) => (ty, return_ty),
      Some(ExpressionType::Return(ty)) => {
        if let Some(return_ty) = return_ty {
          t.assert_type(ty, return_ty, self.argument_span(ast));
        }
        return ExpressionType::Return(ty);
      }
      Some(ExpressionType::Both(ty, return_)) => {
        if let Some(return_ty) = return_ty {
          t.assert_type(return_, return_ty, self.argument_span(ast));
        }
        (ty, Some(return_))
      }
      None => (TypeArena::NEVER, None),
    };

    let return_type = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(argument_type, return_type));

    if t.types.unify(function_type, callee_type).is_err() {
      match t.types.function_parameter(callee_type) {
        Some(expected) if t.types.is_never(expected) && self.argument(ast).is_some() => {
          t.problems.push(TypeError::ExtraArgument {
            given: t.types.type_to_string(argument_type),
            span: self.argument(ast).unwrap().span(ast),
          });
        }
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

    // If the unification fails, `return_type` may not be unified with the callee return type
    let expression_ty = if let Some(function_return_type) = t.types.function_return(callee_type) {
      function_return_type
    } else {
      return_type
    };
    ExpressionType::from(expression_ty, return_ty)
  }
}
impl InferType for Comment {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    self.expression(ast).infer(t, ast)
  }
}
impl InferType for FormatString {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let mut return_type = None;

    for expression in self.expressions(ast) {
      match expression.infer(t, ast).get_return() {
        Some(ty) if return_type.is_none() => return_type = Some(ty),
        Some(ty) => t.assert_type(ty, return_type.unwrap(), expression.span(ast)),
        None => {}
      }
    }

    ExpressionType::from(TypeArena::STRING, return_type)
  }
}
impl InferExpression for Function {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> TypeRef {
    t.env.enter_scope();

    let parameter = t.new_type_var();
    let expected_return = t.new_type_var();
    let function_type = t.types.new_type(Type::Function(parameter, expected_return));

    t.env.define_variable(
      self.parameter.name(ast),
      self.parameter.span(ast),
      TypeScheme::monomorphic(parameter),
      None,
    );
    let return_type = match self.body(ast).infer(t, ast) {
      ExpressionType::Expression(ty) | ExpressionType::Return(ty) => ty,
      ExpressionType::Both(expression_type, return_type) => {
        t.assert_type(return_type, expression_type, self.span(ast));
        expression_type
      }
    };
    t.types.unify(return_type, expected_return).unwrap();

    if t.types.is_never(return_type) {
      t.problems.push(TypeError::FunctionReturnsNever {
        span: self.span(ast),
      });
    }

    t.env.exit_scope(self.span(ast));

    // If the parameter is not used, it is equivalent to never
    if let Type::Variable(type_var) = t.types[parameter]
      && let Some(variable) = t.env.finished_variables().last()
      && variable.used.is_empty()
    {
      t.types.link(type_var, TypeArena::NEVER);
    }

    function_type
  }
}
impl InferType for Group {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    self.expression(ast).infer(t, ast)
  }
}
impl InferType for If {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let condition_return_ty = match self.condition(ast).infer(t, ast) {
      ExpressionType::Expression(_) => None,
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(_, ty) => Some(ty),
    };

    let then_type = self.then(ast).infer(t, ast);

    let branch_ty = if let Some(otherwise) = self.otherwise(ast) {
      let else_type = otherwise.infer(t, ast);
      t.merge_branches(&then_type, &else_type, self.span(ast))
    } else {
      ExpressionType::UNKNOWN
    };

    if let Some(condition_return) = condition_return_ty {
      match branch_ty {
        ExpressionType::Expression(ty) => ExpressionType::Both(ty, condition_return),
        ExpressionType::Return(_) => unreachable!(),
        ExpressionType::Both(ty, branch_ty) => {
          t.assert_type(condition_return, branch_ty, self.condition(ast).span(ast));
          ExpressionType::Both(ty, condition_return)
        }
      }
    } else {
      branch_ty
    }
  }
}
impl InferType for List {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let element_type = t.new_type_var();
    let mut return_type = None;

    for item in self.items(ast) {
      match item.infer(t, ast) {
        ExpressionType::Expression(ty) => t.assert_type(ty, element_type, item.span(ast)),
        ExpressionType::Return(ty) => return ExpressionType::Return(ty),
        ExpressionType::Both(expression, return_) => {
          t.assert_type(expression, element_type, item.span(ast));

          match return_type {
            Some(return_type) => t.assert_type(return_, return_type, item.span(ast)),
            None => return_type = Some(return_),
          }
        }
      }
    }

    let list_type = (t.types).new_type(Type::Structure(Structure::List, element_type));
    ExpressionType::from(list_type, return_type)
  }
}
impl InferExpression for Literal {
  fn infer(&self, _: &mut TypeChecker, ast: &AST) -> TypeRef {
    match self.value(ast) {
      LiteralValue::Boolean(_) => TypeArena::BOOLEAN,
      LiteralValue::Number { .. } => TypeArena::NUMBER,
      LiteralValue::String(_) => TypeArena::STRING,
    }
  }
}
impl InferType for Match {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let (value_type, value_return_ty) = match self.value(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, None),
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(expression, return_) => (expression, Some(return_)),
    };

    // check patterns match the value passes
    for case in self.arms() {
      match &case.pattern {
        Pattern::Identifier(_) | Pattern::Invalid => {}
        Pattern::Literal(literal) => {
          let literal = literal.infer(t, ast);
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
            let start_type = start.infer(t, ast);
            if t.types.unify(start_type, value_type).is_err() {
              t.problems.push(TypeError::PatternNeverMatches {
                pattern: t.types.type_to_string(start_type),
                value: t.types.type_to_string(value_type),
                span: case.pattern.span(ast),
              });
            }
          }
          if let Some(end) = end {
            let end_type = end.infer(t, ast);
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
    let mut expression_ty = ExpressionType::Both(t.new_type_var(), t.new_type_var());
    for case in self.arms() {
      t.env.enter_scope();

      if let Pattern::Identifier(identifier) = &case.pattern {
        t.env.define_variable(
          identifier.name(ast),
          identifier.span(ast),
          TypeScheme::monomorphic(value_type),
          None,
        );
      }

      // check the guard is valid, needs the value to be defined
      if let Some(guard) = case.guard(ast) {
        if let Some(_return_ty) = guard.infer(t, ast).get_return() {
          t.problems.push(TypeError::NoReturnFromMatchGuard {
            span: guard.span(ast),
          });
        }
      }

      let case_type = case.expression(ast).infer(t, ast);
      expression_ty = t.merge_branches(&expression_ty, &case_type, self.span(ast));

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

    if let Some(value_return_ty) = value_return_ty {
      match expression_ty {
        ExpressionType::Expression(ty) => ExpressionType::Both(ty, value_return_ty),
        ExpressionType::Return(_) => unreachable!(),
        ExpressionType::Both(ty, expression_ty) => {
          t.assert_type(value_return_ty, expression_ty, self.value(ast).span(ast));
          ExpressionType::Both(ty, expression_ty)
        }
      }
    } else {
      expression_ty
    }
  }
}
impl InferExpression for ModuleAccess {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> TypeRef {
    let module = StdlibModule::get(self.module(ast));

    let existing_import = t.env.variables().find_map(|var| match &var.kind {
      VariableKind::Declaration { .. } | VariableKind::Builtin { .. } => None,
      VariableKind::Import {
        module,
        item,
        alias,
        defined,
        ..
      } => {
        if *module == self.module(ast) && item == self.item(ast) {
          Some((alias, *defined))
        } else {
          None
        }
      }
    });
    if let Some((alias, definition_location)) = existing_import {
      t.problems.push(TypeError::ModuleAccessAlreadyImported {
        path: format!("{}::{}", self.module(ast), self.item(ast)),
        defined_as: alias.clone().unwrap_or(self.item(ast).to_owned()),
        definition_location,
        span: self.span(ast),
      });
    }

    match module.import_value(&mut t.types, self.item(ast)) {
      ImportResult::Value(type_) => t.types.instantiate(type_),
      ImportResult::ModuleNotFound => {
        t.problems.push(TypeError::ModuleNotFound {
          module: self.module(ast).to_owned(),
          span: self.span(ast),
          did_you_mean: similarly_named(self.module(ast), stdlib::MODULES),
        });
        TypeArena::UNKNOWN
      }
      ImportResult::ItemNotFound => {
        t.problems.push(TypeError::ItemNotFound {
          module: self.module(ast).to_owned(),
          item: self.item(ast).to_owned(),
          span: self.span(ast),
          did_you_mean: similarly_named(self.item(ast), module.items().iter().copied()),
        });
        TypeArena::UNKNOWN
      }
    }
  }
}
impl InferType for Unary {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> ExpressionType {
    let (left, return_) = match self.expression(ast).infer(t, ast) {
      ExpressionType::Expression(ty) => (ty, None),
      ExpressionType::Return(ty) => return ExpressionType::Return(ty),
      ExpressionType::Both(expression, return_) => (expression, Some(return_)),
    };

    let span = self.expression(ast).span(ast);
    let expression_type = match self.operator(ast) {
      UnaryOperator::Not => TypeArena::BOOLEAN,
      UnaryOperator::Minus => {
        t.assert_type(left, TypeArena::NUMBER, span);
        TypeArena::NUMBER
      }
    };

    ExpressionType::from(expression_type, return_)
  }
}
impl InferExpression for Variable {
  fn infer(&self, t: &mut TypeChecker, ast: &AST) -> TypeRef {
    if let Some(type_) = t.env.get_variable(self.name(ast)) {
      t.env.mark_variable_use(self.name(ast), self.span(ast));
      t.types.instantiate(type_)
    } else {
      use super::enviroment::Variable;

      t.problems.push(TypeError::UndefinedVariable {
        identifier: self.name(ast).to_owned(),
        span: self.span(ast),
        did_you_mean: similarly_named(self.name(ast), t.env.variables().map(Variable::name)),
      });
      TypeArena::UNKNOWN
    }
  }
}

/// The type of an expression
///
/// An expression can have a regular expression type,
/// or it can return a from a function which may have a different type
#[must_use]
pub(crate) enum ExpressionType {
  Expression(TypeRef),
  Return(TypeRef),
  Both(TypeRef, TypeRef),
}
impl ExpressionType {
  const NEVER: Self = Self::Expression(TypeArena::NEVER);
  const UNKNOWN: Self = Self::Expression(TypeArena::UNKNOWN);

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

  fn from(ty: TypeRef, possible_return: Option<TypeRef>) -> Self {
    if let Some(return_) = possible_return {
      Self::Both(ty, return_)
    } else {
      Self::Expression(ty)
    }
  }
}
