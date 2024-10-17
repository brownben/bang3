//! Definitions of the lint rules
use super::{
  helpers::{ASTEquality, IsConstant},
  Context, LintRule,
};
use bang_parser::ast::{expression::*, statement::*, GetSpan, Span};

pub const RULES: [&dyn LintRule; 15] = [
  &NoConstantConditions,
  &NoNegativeZero,
  &NoSelfAssign,
  &NoSelfComparison,
  &NoTodoComments,
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

    if let Expression::Match(match_) = &expression {
      if match_.value.is_constant() {
        context.add_diagnostic(&Self, match_.value.span());
      }

      for case in match_.cases.iter().flat_map(|case| &case.guard) {
        if case.is_constant() {
          context.add_diagnostic(&Self, case.span());
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
      && match_.cases[0].guard.is_none()
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

pub struct NoUnnecessaryClosures;
impl LintRule for NoUnnecessaryClosures {
  fn name(&self) -> &'static str {
    "No Unnecessary Closures"
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
      && let Some(otherwise) = &if_.otherwise
      && if_.then.unwrap().equals(otherwise.unwrap())
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
      && (is_zero(binary.left.unwrap()) || is_zero(binary.right.unwrap()))
    {
      context.add_diagnostic(&Self, binary.span());
    }

    if let Expression::Binary(binary) = &expression
      && binary.operator == BinaryOperator::Divide
      && is_zero(binary.left.unwrap())
    {
      context.add_diagnostic(&Self, binary.span());
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    if let Expression::FormatString(format_string) = &expression {
      for expression in &format_string.expressions {
        if let Expression::Literal(literal) = &expression
          && let LiteralKind::String { .. } = literal.kind
        {
          context.add_diagnostic(&Self, literal.span());
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    #[allow(clippy::match_same_arms)]
    fn ends_with_return(expression: &Expression<'_, '_>) -> Option<Span> {
      match expression {
        Expression::Block(block) => block
          .statements
          .iter()
          .rev()
          .find(|statement| !matches!(statement, Statement::Comment(_)))
          .and_then(|statement| match statement {
            Statement::Comment(_) => None,
            Statement::Expression(expression) => ends_with_return(expression),
            Statement::Return(_) => Some(statement.span()),
            Statement::Let(_) => unreachable!(),
          }),

        // Could contain a block with return sensibly at the end
        Expression::Group(group) => ends_with_return(&group.expression),
        Expression::If(if_) => {
          if let Some(otherwise) = &if_.otherwise {
            ends_with_return(otherwise).or(ends_with_return(&if_.then))
          } else {
            None
          }
        }
        Expression::Match(match_) => match_
          .cases
          .iter()
          .find_map(|case| ends_with_return(&case.expression)),

        // Can never end with a return
        Expression::Function(_)
        | Expression::Literal(_)
        | Expression::Variable(_)
        | Expression::Invalid(_) => None,

        // Could feasibly end with a return,
        // but would be contrived code as would skip the primary operation
        Expression::Binary(_)
        | Expression::Comment(_)
        | Expression::Call(_)
        | Expression::FormatString(_)
        | Expression::Unary(_) => None,
      }
    }

    if let Expression::Function(function) = &expression {
      if let Some(return_span) = ends_with_return(&function.body) {
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
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
    fn statement_always_returns(statement: &Statement) -> bool {
      match statement {
        Statement::Return(_) => true,
        Statement::Expression(expression) => always_returns(expression),
        Statement::Let(_) | Statement::Comment(_) => false,
      }
    }
    fn always_returns(expression: &Expression) -> bool {
      #[allow(clippy::match_same_arms)]
      match expression {
        Expression::Block(block) => block.statements.iter().any(statement_always_returns),

        // Could contain a block with return sensibly at the end
        Expression::Group(group) => always_returns(&group.expression),
        Expression::If(if_) => {
          always_returns(&if_.then) && if_.otherwise.as_ref().is_some_and(always_returns)
        }
        Expression::Match(match_) => match_
          .cases
          .iter()
          .all(|case| always_returns(&case.expression)),

        // Can never end with a return
        Expression::Function(_)
        | Expression::Literal(_)
        | Expression::Variable(_)
        | Expression::Invalid(_) => false,

        // Could feasibly end with a return,
        // but would be contrived code as would skip the primary operation
        Expression::Binary(_)
        | Expression::Comment(_)
        | Expression::Call(_)
        | Expression::FormatString(_)
        | Expression::Unary(_) => false,
      }
    }

    if let Expression::Block(block) = expression {
      let return_index = block.statements.iter().position(statement_always_returns);

      if let Some(return_index) = return_index
        && return_index < block.statements.len() - 1
      {
        let first_unused_statement = &block.statements[return_index + 1];
        let span = Span::new(first_unused_statement.span().start, block.span.end - 1);

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
  fn visit_expression(&self, context: &mut Context, expression: &Expression) {
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
      && binary.operator == BinaryOperator::Or
      && let Expression::Binary(left) = &binary.left
      && let Expression::Binary(right) = &binary.right
    {
      if (left.operator == BinaryOperator::Equal && is_comparison(right.operator))
        && (right.left.equals(&left.left) || right.left.equals(&left.right))
        && (right.right.equals(&left.left) || right.right.equals(&left.right))
      {
        context.add_diagnostic(&Self, binary.span());
      }

      if (right.operator == BinaryOperator::Equal && is_comparison(left.operator))
        && (left.left.equals(&right.left) || left.left.equals(&right.right))
        && (left.right.equals(&right.left) || left.right.equals(&right.right))
      {
        context.add_diagnostic(&Self, binary.span());
      }
    }
  }
}
