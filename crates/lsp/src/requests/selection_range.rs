use crate::{
  documents::Document,
  locations::{lsp_range_from_span, span_from_lsp_position},
};
use lsp_types::{self as lsp};

use bang_syntax::{
  AST, Span,
  ast::{Expression, Statement, Type, expression, statement, types},
};

pub fn selection_ranges(file: &Document, positions: &[lsp::Position]) -> Vec<lsp::SelectionRange> {
  positions
    .iter()
    .map(|position| span_from_lsp_position(*position, file))
    .filter_map(|span| file.ast.selection_range(file, span, &mut None))
    .collect()
}

trait SelectionRange {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp::SelectionRange>;
}

impl SelectionRange for AST {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    self
      .root_statements
      .iter()
      .map(|statement| statement.selection_range(file, span, parent))
      .find(Option::is_some)
      .flatten()
  }
}

impl SelectionRange for Statement {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    if !self.span(&file.ast).contains(span) {
      return None;
    }

    let mut range = lsp_selection_range(file, self.span(&file.ast), parent.take());
    let parent = &mut range;

    match self {
      Statement::Comment(_) => range,
      Statement::Expression(expression_stmt) => expression_stmt.selection_range(file, span, parent),
      Statement::Import(_) => range,
      Statement::Let(let_) => let_.selection_range(file, span, parent),
      Statement::Return(return_) => return_.selection_range(file, span, parent),
    }
  }
}
impl SelectionRange for statement::ExpressionStmt {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    (self.expression(&file.ast)).selection_range(file, span, parent)
  }
}
impl SelectionRange for statement::Let {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let value = self.value(&file.ast).selection_range(file, span, parent);
    let annotation = (self.annotation(&file.ast)).selection_range(file, span, parent);

    value.or(annotation).or(parent.take())
  }
}
impl SelectionRange for statement::Return {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let expression = (self.expression(&file.ast)).selection_range(file, span, parent);

    expression.or(parent.take())
  }
}

impl SelectionRange for Expression {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp::SelectionRange> {
    if !self.span(&file.ast).contains(span) {
      return None;
    }

    let mut range = lsp_selection_range(file, self.span(&file.ast), parent.take());
    let parent = &mut range;

    match self {
      Expression::Binary(binary) => binary.selection_range(file, span, parent),
      Expression::Block(block) => block.selection_range(file, span, parent),
      Expression::Call(call) => call.selection_range(file, span, parent),
      Expression::FormatString(format_string) => format_string.selection_range(file, span, parent),
      Expression::Function(function) => function.selection_range(file, span, parent),
      Expression::Group(group) => group.selection_range(file, span, parent),
      Expression::If(if_) => if_.selection_range(file, span, parent),
      Expression::List(list) => list.selection_range(file, span, parent),
      Expression::Match(match_) => match_.selection_range(file, span, parent),
      Expression::Unary(unary) => unary.selection_range(file, span, parent),

      Expression::Comment(_)
      | Expression::Literal(_)
      | Expression::ModuleAccess(_)
      | Expression::Variable(_)
      | Expression::Invalid(_) => range,
    }
  }
}
impl SelectionRange for expression::Binary {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp::SelectionRange> {
    let left = self.left(&file.ast).selection_range(file, span, parent);
    let right = self.right(&file.ast).selection_range(file, span, parent);

    left.or(right).or(parent.take())
  }
}
impl SelectionRange for expression::Block {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp::SelectionRange> {
    self
      .statements(&file.ast)
      .map(|statement| statement.selection_range(file, span, parent))
      .find(Option::is_some)
      .flatten()
      .or(parent.take())
  }
}
impl SelectionRange for expression::Call {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let callee = self.callee(&file.ast).selection_range(file, span, parent);
    let argument = self.argument(&file.ast).selection_range(file, span, parent);

    callee.or(argument).or(parent.take())
  }
}
impl SelectionRange for expression::FormatString {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    self
      .expressions(&file.ast)
      .map(|expression| expression.selection_range(file, span, parent))
      .find(Option::is_some)
      .flatten()
      .or(parent.take())
  }
}
impl SelectionRange for expression::Function {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let body = self.body(&file.ast).selection_range(file, span, parent);

    body.or(parent.take())
  }
}
impl SelectionRange for expression::Group {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let expression = (self.expression(&file.ast)).selection_range(file, span, parent);

    expression.or(parent.take())
  }
}
impl SelectionRange for expression::If {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let condition = (self.condition(&file.ast)).selection_range(file, span, parent);
    let then = self.then(&file.ast).selection_range(file, span, parent);
    let otherwise = (self.otherwise(&file.ast)).selection_range(file, span, parent);

    condition.or(then).or(otherwise).or(parent.take())
  }
}
impl SelectionRange for expression::List {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    (self.items(&file.ast))
      .find_map(|item| item.selection_range(file, span, parent))
      .or(parent.take())
  }
}
impl SelectionRange for expression::Match {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    if let Some(value) = self.value(&file.ast).selection_range(file, span, parent) {
      return Some(value);
    }

    self
      .arms()
      .map(|arm| arm.selection_range(file, span, parent))
      .find(Option::is_some)
      .flatten()
      .or(parent.take())
  }
}
impl SelectionRange for expression::MatchArm {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    if !self.span(&file.ast).contains(span) {
      return None;
    }

    let range = lsp_selection_range(file, self.span(&file.ast), parent.take());

    let pattern = self.pattern.selection_range(file, span, parent);
    let guard = self.guard(&file.ast).selection_range(file, span, parent);
    let body = (self.expression(&file.ast)).selection_range(file, span, parent);

    pattern.or(guard).or(body).or(range)
  }
}
impl SelectionRange for expression::Pattern {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    if self.span(&file.ast).contains(span) {
      lsp_selection_range(file, self.span(&file.ast), parent.take())
    } else {
      None
    }
  }
}
impl SelectionRange for expression::Unary {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let expression = (self.expression(&file.ast)).selection_range(file, span, parent);

    expression.or(parent.take())
  }
}

impl SelectionRange for Type {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    if !self.span(&file.ast).contains(span) {
      return None;
    }

    let mut range = lsp_selection_range(file, self.span(&file.ast), parent.take());
    let parent = &mut range;

    match self {
      Type::Primitive(_) => range,
      Type::Variable(_) => range,
      Type::Function(function) => function.selection_range(file, span, parent),
      Type::Group(group) => group.selection_range(file, span, parent),
      Type::Structure(structure) => structure.selection_range(file, span, parent),
      Type::Invalid(_) => range,
    }
  }
}
impl SelectionRange for types::TypeFunction {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let param = (self.parameter(&file.ast)).selection_range(file, span, parent);
    let return_type = self.return_(&file.ast).selection_range(file, span, parent);

    param.or(return_type).or(parent.take())
  }
}
impl SelectionRange for types::TypeGroup {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    (self.type_(&file.ast).selection_range(file, span, parent)).or(parent.take())
  }
}
impl SelectionRange for types::TypeStructure {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp_types::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    let parameter = (self.parameter(&file.ast)).selection_range(file, span, parent);

    parameter.or(parent.take())
  }
}

impl<T: SelectionRange> SelectionRange for Option<&T> {
  fn selection_range(
    &self,
    file: &Document,
    span: Span,
    parent: &mut Option<lsp::SelectionRange>,
  ) -> Option<lsp_types::SelectionRange> {
    self.and_then(|x| x.selection_range(file, span, parent))
  }
}

#[allow(clippy::unnecessary_wraps)]
fn lsp_selection_range(
  file: &Document,
  span: Span,
  parent: Option<lsp::SelectionRange>,
) -> Option<lsp::SelectionRange> {
  let range = lsp_range_from_span(span, file);

  Some(lsp::SelectionRange {
    range,
    parent: parent.map(Box::new),
  })
}
