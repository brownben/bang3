use super::{
  helpers::{ASTEquality, IsConstant},
  Context, LintRule,
};
use crate::ast::{expression::*, statement::*, GetSpan};

pub const RULES: [&dyn LintRule; 5] = [
  &NoConstantConditions,
  &NoNegativeZero,
  &NoSelfAssign,
  &NoSelfComparison,
  &NoYodaComparison,
];

pub struct NoConstantConditions;
impl LintRule for NoConstantConditions {
  fn name(&self) -> &'static str {
    "No Constant Conditions"
  }
  fn message(&self) -> &'static str {
    "the control flow could be removed, as the condition is always true or false"
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
      && let LiteralKind::Number(value) = literal.kind
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
      && let_.identifier == variable.name
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

// TODO: No Unused Variables
