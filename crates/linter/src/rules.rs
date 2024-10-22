//! Definitions of the lint rules
use super::{
  helpers::{is_zero, unwrap, ASTEquality, IsConstant, ReturnAnalysis},
  Context, LintRule,
};
use bang_syntax::{
  ast::{expression::*, statement::*},
  Span, AST,
};
use std::borrow::Cow;

pub const RULES: [&dyn LintRule; 16] = [
  &NoConstantConditions,
  &NoNegativeZero,
  &NoSelfAssign,
  &NoSelfComparison,
  &NoUnderscoreVariableUse,
  &NoUselessMatch,
  &NoYodaComparison,
  &NoUnnecessaryClosures,
  &NoUselessIf,
  &NoErasingOperations,
  &NoConstantStringsInFormatString,
  &NoUnnecessaryReturn,
  &NoUnreachableCode,
  &NoDoubleCondition,
  &NoEmptyImports,
  &NoLossOfPrecision,
];

pub struct NoConstantConditions;
impl LintRule for NoConstantConditions {
  fn name(&self) -> &'static str {
    "No Constant Conditions"
  }
  fn message(&self) -> &'static str {
    "the control flow could be removed, as the condition is always the same"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::If(if_) = &expression
      && if_.condition(ast).is_constant(ast)
    {
      context.add_diagnostic(&Self, if_.condition(ast).span(ast));
    }

    if let Expression::Match(match_) = &expression {
      if match_.value(ast).is_constant(ast) {
        context.add_diagnostic(&Self, match_.value(ast).span(ast));
      }

      for case in match_.arms().filter_map(|case| case.guard(ast)) {
        if case.is_constant(ast) {
          context.add_diagnostic(&Self, case.span(ast));
        }
      }
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Unary(unary) = &expression
      && unary.operator(ast) == UnaryOperator::Minus
      && let Expression::Literal(literal) = &unary.expression(ast)
      && let LiteralValue::Number(value) = literal.value(ast)
      && value == 0.0
    {
      context.add_diagnostic(&Self, unary.span(ast));
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
  fn visit_statement(&self, context: &mut Context, expression: &Statement, ast: &AST) {
    if let Statement::Let(let_) = &expression
      && let Expression::Variable(variable) = unwrap(let_.value(ast), ast)
      && let_.identifier(ast) == variable.name(ast)
    {
      context.add_diagnostic(&Self, let_.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
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
      && is_comparison(binary.operator(ast))
      && binary.left(ast).equals(binary.right(ast), ast)
    {
      context.add_diagnostic(&Self, binary.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Variable(variable) = &expression
      && variable.name(ast) == "_"
    {
      context.add_diagnostic(&Self, variable.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Match(match_) = &expression
      && let Some(first_arm) = match_.arms().next()
      && let Pattern::Identifier(_) = first_arm.pattern
      && first_arm.guard(ast).is_none()
    {
      context.add_diagnostic(&Self, match_.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Binary(binary) = &expression
      && let BinaryOperator::Equal | BinaryOperator::NotEqual = binary.operator(ast)
      && let Expression::Variable(_) = unwrap(binary.right(ast), ast)
      && let Expression::Literal(_) = unwrap(binary.left(ast), ast)
    {
      context.add_diagnostic(&Self, binary.span(ast));
    }
  }
}

pub struct NoUnnecessaryClosures;
impl LintRule for NoUnnecessaryClosures {
  fn name(&self) -> &'static str {
    "No Unnecessary Closures"
  }
  fn message(&self) -> &'static str {
    "function could just be passed directly, without being wrapped in another function"
  }

  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Function(function) = &expression
      && let Expression::Call(call) = unwrap(function.body(ast), ast)
    {
      if let Some(argument) = &call.argument(ast)
        && let Expression::Variable(variable) = unwrap(argument, ast)
        && variable.name(ast) == function.parameter.name(ast)
      {
        context.add_diagnostic(&Self, function.span(ast));
      }

      if call.argument(ast).is_none() && function.parameter.name(ast).starts_with('_') {
        context.add_diagnostic(&Self, function.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::If(if_) = &expression
      && let Some(otherwise) = &if_.otherwise(ast)
      && if_.then(ast).equals(otherwise, ast)
    {
      context.add_diagnostic(&Self, if_.span(ast));
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Binary(binary) = &expression
      && binary.operator(ast) == BinaryOperator::Multiply
      && (is_zero(unwrap(binary.left(ast), ast), ast)
        || is_zero(unwrap(binary.right(ast), ast), ast))
    {
      context.add_diagnostic(&Self, binary.span(ast));
    }

    if let Expression::Binary(binary) = &expression
      && binary.operator(ast) == BinaryOperator::Divide
      && is_zero(unwrap(binary.left(ast), ast), ast)
    {
      context.add_diagnostic(&Self, binary.span(ast));
    }
  }
}

pub struct NoConstantStringsInFormatString;
impl LintRule for NoConstantStringsInFormatString {
  fn name(&self) -> &'static str {
    "No Constant Strings in Format Strings"
  }
  fn message(&self) -> &'static str {
    "can be combined with the rest of the string"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::FormatString(format_string) = &expression {
      for expression in format_string.expressions(ast) {
        if let Expression::Literal(literal) = &expression
          && let LiteralValue::String(_) = literal.value(ast)
        {
          context.add_diagnostic(&Self, literal.span(ast));
        }
      }
    }
  }
}

pub struct NoUnnecessaryReturn;
impl LintRule for NoUnnecessaryReturn {
  fn name(&self) -> &'static str {
    "No Unnecessary Return"
  }
  fn message(&self) -> &'static str {
    "return statement is unnecessary, as a block returns the last expression"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Function(function) = &expression {
      if let Some(return_span) = function.body(ast).ends_with_return(ast) {
        context.add_diagnostic(&Self, return_span);
      }
    }
  }
}

pub struct NoUnreachableCode;
impl LintRule for NoUnreachableCode {
  fn name(&self) -> &'static str {
    "No Unreachable Code"
  }
  fn message(&self) -> &'static str {
    "code after a return will never be executed"
  }
  fn is_unused(&self) -> bool {
    true
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    if let Expression::Block(block) = expression {
      let return_index = block.statements(ast).position(|s| s.always_returns(ast));

      if let Some(return_index) = return_index
        && return_index < block.len() - 1
      {
        let first_unused_statement = &block.statement(return_index + 1, ast);
        let span = Span::new(
          first_unused_statement.span(ast).start,
          block.span(ast).end - 1,
        );

        context.add_diagnostic(&Self, span);
      }
    }
  }
}

pub struct NoDoubleCondition;
impl LintRule for NoDoubleCondition {
  fn name(&self) -> &'static str {
    "No Double Condition"
  }
  fn message(&self) -> &'static str {
    "can be simplified into a single condition"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    fn is_comparison(operator: BinaryOperator) -> bool {
      matches!(
        operator,
        BinaryOperator::Greater
          | BinaryOperator::GreaterEqual
          | BinaryOperator::Less
          | BinaryOperator::LessEqual
      )
    }

    // a == b or a > b
    if let Expression::Binary(binary) = expression
      && binary.operator(ast) == BinaryOperator::Or
      && let Expression::Binary(left) = unwrap(binary.left(ast), ast)
      && let Expression::Binary(right) = unwrap(binary.right(ast), ast)
    {
      // a `op` b or c `op` d
      let (a, b) = (left.left(ast), left.right(ast));
      let (c, d) = (right.left(ast), right.right(ast));

      if (left.operator(ast) == BinaryOperator::Equal && is_comparison(right.operator(ast)))
        && (c.equals(a, ast) || c.equals(b, ast))
        && (d.equals(a, ast) || d.equals(b, ast))
      {
        context.add_diagnostic(&Self, binary.span(ast));
      }

      if (right.operator(ast) == BinaryOperator::Equal && is_comparison(left.operator(ast)))
        && (a.equals(c, ast) || a.equals(d, ast))
        && (b.equals(c, ast) || b.equals(d, ast))
      {
        context.add_diagnostic(&Self, binary.span(ast));
      }
    }
  }
}

pub struct NoEmptyImports;
impl LintRule for NoEmptyImports {
  fn name(&self) -> &'static str {
    "No Empty Imports"
  }
  fn message(&self) -> &'static str {
    "statement can be deleted"
  }
  fn visit_statement(&self, context: &mut Context, statement: &Statement, ast: &AST) {
    if let Statement::Import(import) = statement
      && import.items(ast).count() == 0
    {
      context.add_diagnostic(&Self, import.span(ast));
    }
  }
}

pub struct NoLossOfPrecision;
impl LintRule for NoLossOfPrecision {
  fn name(&self) -> &'static str {
    "No Loss of Precision"
  }
  fn message(&self) -> &'static str {
    "numbers are stored as double-precision floating-point numbers according to the IEEE 754 standard\nwhen the literal is converted to a number, precision will be lost and the value may not be what was intended"
  }
  fn visit_expression(&self, context: &mut Context, expression: &Expression, ast: &AST) {
    fn clean_up_number_literal(raw: &str) -> Cow<str> {
      // If the number is already clean, return it
      if !raw.contains('_') && !raw.starts_with('0') && !raw.starts_with('.') && !raw.ends_with('0')
      {
        return Cow::Borrowed(raw);
      }

      // Remove underscores/ numeric separators
      let value = raw.replace('_', "");

      // Remove leading and trailing zeros
      // Can only remove trailing zeros if they are after a decimal point
      let value = if value.contains('.') {
        value.trim_start_matches('0').trim_end_matches('0')
      } else {
        value.trim_start_matches('0')
      };

      if value.is_empty() {
        return Cow::Borrowed("0");
      }

      // Remove values which start or end with a decimal point
      let value = if value.starts_with('.') {
        format!("0{}", value.trim_end_matches('.'))
      } else {
        value.trim_end_matches('.').to_owned()
      };

      Cow::Owned(value)
    }

    if let Expression::Literal(literal) = &expression
      && let LiteralValue::Number(f64_value) = literal.value(ast)
    {
      let existing = clean_up_number_literal(literal.raw_value(ast));
      let round_tripped = f64_value.to_string();

      if existing != round_tripped {
        context.add_diagnostic(&Self, literal.span(ast));
      }
    }
  }
}
