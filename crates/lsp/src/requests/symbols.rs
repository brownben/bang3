use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_parser::ast::{expression::*, statement::*};
use bang_parser::{parse, Allocator};

pub fn document_symbols(file: &Document) -> lsp::DocumentSymbolResponse {
  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  let mut symbols = Vec::new();
  for statement in &ast.statements {
    statement_symbols(statement, file, &mut symbols);
  }

  lsp::DocumentSymbolResponse::Nested(symbols)
}

fn statement_symbols(
  statement: &Statement,
  file: &Document,
  symbols: &mut Vec<lsp::DocumentSymbol>,
) {
  match statement {
    Statement::Comment(_) => {}
    Statement::Expression(expression) => expression_symbols(expression, file, symbols),
    Statement::Let(let_) => {
      let mut children = Vec::new();
      expression_symbols(&let_.expression, file, &mut children);

      if let_.identifier.name.is_empty() {
        return;
      }

      symbols.push(
        #[allow(deprecated, reason = "struct doesn't impl Default")]
        lsp::DocumentSymbol {
          name: let_.identifier.name.to_owned(),
          detail: None,
          kind: if matches!(&let_.expression, Expression::Function(_)) {
            lsp::SymbolKind::FUNCTION
          } else if is_screaming_snake_case(let_.identifier.name) {
            lsp::SymbolKind::CONSTANT
          } else {
            lsp::SymbolKind::VARIABLE
          },
          range: lsp_range_from_span(let_.span, file),
          selection_range: lsp_range_from_span(let_.identifier.span, file),
          children: Some(children),
          tags: None,
          deprecated: None,
        },
      );
    }
  }
}

fn expression_symbols(
  expression: &Expression,
  file: &Document,
  symbols: &mut Vec<lsp::DocumentSymbol>,
) {
  match expression {
    Expression::Binary(binary) => {
      expression_symbols(&binary.left, file, symbols);
      expression_symbols(&binary.right, file, symbols);
    }
    Expression::Block(block) => {
      for statement in &block.statements {
        statement_symbols(statement, file, symbols);
      }
    }
    Expression::Call(call) => {
      expression_symbols(&call.expression, file, symbols);
      if let Some(argument) = &call.argument {
        expression_symbols(argument, file, symbols);
      }
    }
    Expression::Comment(comment) => expression_symbols(&comment.expression, file, symbols),
    Expression::FormatString(format_string) => {
      for expression in &format_string.expressions {
        expression_symbols(expression, file, symbols);
      }
    }
    Expression::Function(function) => expression_symbols(&function.body, file, symbols),
    Expression::Group(group) => expression_symbols(&group.expression, file, symbols),
    Expression::If(if_) => {
      expression_symbols(&if_.condition, file, symbols);
      expression_symbols(&if_.then, file, symbols);
      expression_symbols(&if_.otherwise, file, symbols);
    }
    Expression::Match(match_) => {
      expression_symbols(&match_.value, file, symbols);
      for case in &match_.cases {
        expression_symbols(&case.expression, file, symbols);
      }
    }
    Expression::Unary(unary) => expression_symbols(&unary.expression, file, symbols),
    Expression::Literal(_) | Expression::Variable(_) | Expression::Invalid(_) => {}
  }
}
