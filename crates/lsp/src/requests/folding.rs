use crate::{documents::Document, locations::lsp_range_from_span};
use lsp_types::{self as lsp};

use bang_syntax::{
  ast::{Expression, Statement},
  parse, Span, AST,
};

pub fn folding_ranges(file: &Document) -> Vec<lsp::FoldingRange> {
  let ast = parse(&file.source);
  let mut ranges = Vec::new();

  for expression in &ast.expressions {
    if let Some(span) = expression_folding_range(expression, &ast) {
      if file.line_index.line(span) != file.line_index.final_line(span) {
        ranges.push(folding_range_from_span(span, file));
      }
    }
  }

  for statement in ast.root_statements.iter().chain(ast.statements.iter()) {
    if let Statement::Import(import) = statement {
      let span = import.items_span(&ast);
      if file.line_index.line(span) != file.line_index.final_line(span) {
        let mut range = folding_range_from_span(span, file);
        range.kind = Some(lsp::FoldingRangeKind::Imports);
        ranges.push(range);
      }
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
