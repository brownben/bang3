//! Definitions of the lint rules
use super::{
  helpers::{ASTEquality, IsConstant},
  Context, LintRule,
};
use bang_parser::ast::{expression::*, statement::*, GetSpan};

pub const RULES: [&dyn LintRule; 11] = [
  &NoConstantConditions,
  &NoNegativeZero,
  &NoSelfAssign,
  &NoSelfComparison,
  &NoTodoComments,
  &NoUnderscoreVariableUse,
  &NoUselessMatch,
  &NoYodaComparison,
  &NoUnneccessaryClosures,
  &NoUselessIf,
  &NoErasingOperations,
];

pub struct NoConstantConditions;
impl LintRule for NoConstantConditions {
  fn name(&self) -> &'static str {
    "No Constant Conditions"
  }
  fn message(&self) -> &'static str {
    "the control flow could be removed, as the condition is always the same"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::If(if_) = &expression
      && if_.condition.is_constant()
    {
      context.add_diagnostic(&Self, if_.condition.span());
    }

    if let Expression::Match(match_) = &expression
      && match_.value.is_constant()
    {
      context.add_diagnostic(&Self, match_.value.span());
    }
  }
}

pub struct NoNegativeZero;
impl LintRule for NoNegativeZero {
  fn name(&self) -> &'static str {
    "No Negative Zero"
  }
  fn message(&self) -> &'static str {
    "negative zero is unnecessary as 0 == -0"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Unary(unary) = &expression
      && unary.operator == UnaryOperator::Minus
      && let Expression::Literal(literal) = &unary.expression
      && let LiteralKind::Number { value, .. } = literal.kind
      && value == 0.0
    {
      context.add_diagnostic(&Self, unary.span());
    }
  }
}

pub struct NoSelfAssign;
impl LintRule for NoSelfAssign {
  fn name(&self) -> &'static str {
    "No Self Assign"
  }
  fn message(&self) -> &'static str {
    "assigning a variable to itself is unnecessary"
  }
  fn visit_statement(&self, context: &mut Context, expression: &Statement) {
    if let Statement::Let(let_) = &expression
      && let Expression::Variable(variable) = &let_.expression.unwrap()
      && let_.identifier.name == variable.name
    {
      context.add_diagnostic(&Self, let_.span());
    }
  }
}

pub struct NoSelfComparison;
impl LintRule for NoSelfComparison {
  fn name(&self) -> &'static str {
    "No Self Comparison"
  }
  fn message(&self) -> &'static str {
    "comparing a variable to itself is unnecessary"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    fn is_comparison(operator: BinaryOperator) -> bool {
      matches!(
        operator,
        BinaryOperator::Equal
          | BinaryOperator::NotEqual
          | BinaryOperator::Greater
          | BinaryOperator::GreaterEqual
          | BinaryOperator::Less
          | BinaryOperator::LessEqual
      )
    }

    if let Expression::Binary(binary) = &expression
      && is_comparison(binary.operator)
      && binary.left.equals(&binary.right)
    {
      context.add_diagnostic(&Self, binary.span());
    }
  }
}

pub struct NoTodoComments;
impl LintRule for NoTodoComments {
  fn name(&self) -> &'static str {
    "No TODO Comments"
  }
  fn message(&self) -> &'static str {
    "line contains TODO, consider resolving the issue"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Comment(comment) = &expression
      && comment.text.trim().starts_with("TODO:")
    {
      context.add_diagnostic(&Self, comment.message_span);
    }
  }
  fn visit_statement(&self, context: &mut Context, statement: &Statement) {
    if let Statement::Comment(comment) = &statement
      && comment.text.trim().starts_with("TODO:")
    {
      context.add_diagnostic(&Self, comment.span());
    }
  }
}

pub struct NoUnderscoreVariableUse;
impl LintRule for NoUnderscoreVariableUse {
  fn name(&self) -> &'static str {
    "No `_` Variable Use"
  }
  fn message(&self) -> &'static str {
    "`_` indicates the variable/ parameter is not used, but it has been used"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Variable(variable) = &expression
      && variable.name == "_"
    {
      context.add_diagnostic(&Self, variable.span());
    }
  }
}

pub struct NoUselessMatch;
impl LintRule for NoUselessMatch {
  fn name(&self) -> &'static str {
    "No Useless Match"
  }
  fn message(&self) -> &'static str {
    "match statement is unnecessary as the first case matches everything"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Match(match_) = &expression
      && let Pattern::Identifier(_) = match_.cases[0].pattern
    {
      context.add_diagnostic(&Self, match_.span());
    }
  }
}

pub struct NoYodaComparison;
impl LintRule for NoYodaComparison {
  fn name(&self) -> &'static str {
    "No Yoda Equality"
  }
  fn message(&self) -> &'static str {
    "it is clearer to have the variable first then the value to compare to"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Binary(binary) = &expression
      && let BinaryOperator::Equal | BinaryOperator::NotEqual = binary.operator
      && let Expression::Variable(_) = binary.right.unwrap()
      && let Expression::Literal(_) = binary.left.unwrap()
    {
      context.add_diagnostic(&Self, binary.span());
    }
  }
}

pub struct NoUnneccessaryClosures;
impl LintRule for NoUnneccessaryClosures {
  fn name(&self) -> &'static str {
    "No Unneccessary Closures"
  }
  fn message(&self) -> &'static str {
    "function could just be passed directly, without being wrapped in another function"
  }

  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::Function(function) = &expression
      && let Expression::Call(call) = &function.body.unwrap()
    {
      if let Some(argument) = &call.argument
        && let Expression::Variable(variable) = argument.unwrap()
        && variable.name == function.parameter.name
      {
        context.add_diagnostic(&Self, function.span());
      }

      if call.argument.is_none() && function.parameter.name.starts_with('_') {
        context.add_diagnostic(&Self, function.span());
      }
    }
  }
}

pub struct NoUselessIf;
impl LintRule for NoUselessIf {
  fn name(&self) -> &'static str {
    "No Useless If"
  }
  fn message(&self) -> &'static str {
    "both branches of the if-else are the same, consider removing the if"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::If(if_) = &expression
      && if_.then.unwrap().equals(&if_.otherwise.unwrap())
    {
      context.add_diagnostic(&Self, if_.span());
    }
  }
}

pub struct NoErasingOperations;
impl LintRule for NoErasingOperations {
  fn name(&self) -> &'static str {
    "No Erasing Operations"
  }
  fn message(&self) -> &'static str {
    "this operation always returns 0"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    fn is_zero(expression: &Expression) -> bool {
      match expression {
        Expression::Literal(literal) => match literal.kind {
          LiteralKind::Number { value, .. } => value == 0.0,
          _ => false,
        },
        _ => false,
      }
    }

    if let Expression::Binary(binary) = &expression
      && binary.operator == BinaryOperator::Multiply
      && (is_zero(&binary.left.unwrap()) || is_zero(&binary.right.unwrap()))
    {
      context.add_diagnostic(&Self, binary.span());
    }

    if let Expression::Binary(binary) = &expression
      && binary.operator == BinaryOperator::Divide
      && is_zero(&binary.left.unwrap())
    {
      context.add_diagnostic(&Self, binary.span());
    }
  }
}
