use crate::{documents::Document, locations::lsp_range_from_span};
use lsp_types::{self as lsp};

use bang_syntax::{
  ast::{Expression, Statement},
  Span, AST,
};

pub fn folding_ranges(file: &Document) -> Vec<lsp::FoldingRange> {
  let mut ranges = Vec::new();
  let line_index = file.ast.line_index();

  for expression in &file.ast.expressions {
    if let Some(span) = expression_folding_range(expression, &file.ast) {
      if line_index.line(span) != line_index.final_line(span) {
        ranges.push(folding_range_from_span(span, file));
      }
    }
  }

  for statement in file.ast.all_statements() {
    match statement {
      Statement::Import(import) => {
        let span = import.items_span(&file.ast);
        if line_index.line(span) != line_index.final_line(span) {
          let mut range = folding_range_from_span(span, file);
          range.kind = Some(lsp::FoldingRangeKind::Imports);
          ranges.push(range);
        }
      }
      Statement::Comment(comment) => {
        let span = comment.span(&file.ast);
        if line_index.line(span) != line_index.final_line(span) {
          let mut range = folding_range_from_span(span, file);
          range.kind = Some(lsp::FoldingRangeKind::Comment);
          ranges.push(range);
        }
      }
      _ => {}
    }
  }

  ranges
}

fn expression_folding_range(expression: &Expression, ast: &AST) -> Option<Span> {
  match expression {
    Expression::Block(block) => Some(block.span(ast)),
    Expression::Call(call) => Some(call.argument_span(ast)),
    Expression::Group(group) => Some(group.span(ast)),
    Expression::Literal(literal) => Some(literal.span(ast)),
    Expression::Match(match_) => Some(match_.arms_span(ast)),
    _ => None,
  }
}

fn folding_range_from_span(span: Span, file: &Document) -> lsp::FoldingRange {
  let range = lsp_range_from_span(span, file);

  lsp::FoldingRange {
    start_line: range.start.line,
    start_character: Some(range.start.character),
    end_line: range.end.line,
    end_character: Some(range.end.character),
    kind: Some(lsp::FoldingRangeKind::Region),
    collapsed_text: None,
  }
}
