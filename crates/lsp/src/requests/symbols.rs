use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_syntax::ast::{expression::*, statement::*};
use bang_syntax::{parse, AST};

pub fn document_symbols(file: &Document) -> lsp::DocumentSymbolResponse {
  let ast = parse(&file.source);

  let mut symbols = Vec::new();
  for statement in &ast.root_statements {
    statement_symbols(statement, file, &ast, &mut symbols);
  }

  lsp::DocumentSymbolResponse::Nested(symbols)
}

fn statement_symbols(
  statement: &Statement,
  file: &Document,
  ast: &AST,
  symbols: &mut Vec<lsp::DocumentSymbol>,
) {
  match statement {
    Statement::Comment(_) | Statement::Import(_) => {}
    Statement::Expression(expression) => {
      expression_symbols(expression.expression(ast), file, ast, symbols);
    }
    Statement::Let(let_) => {
      let mut children = Vec::new();
      expression_symbols(let_.value(ast), file, ast, &mut children);

      if let_.identifier(ast).is_empty() {
        return;
      }

      symbols.push(
        #[allow(deprecated, reason = "struct doesn't impl Default")]
        lsp::DocumentSymbol {
          name: let_.identifier(ast).to_owned(),
          detail: None,
          kind: if matches!(&let_.value(ast), Expression::Function(_)) {
            lsp::SymbolKind::FUNCTION
          } else if is_screaming_snake_case(let_.identifier(ast)) {
            lsp::SymbolKind::CONSTANT
          } else {
            lsp::SymbolKind::VARIABLE
          },
          range: lsp_range_from_span(let_.span(ast), file),
          selection_range: lsp_range_from_span(let_.identifier_span(ast), file),
          children: Some(children),
          tags: None,
          deprecated: None,
        },
      );
    }
    Statement::Return(return_) => expression_symbols(return_.expression(ast), file, ast, symbols),
  }
}

fn expression_symbols(
  expression: &Expression,
  file: &Document,
  ast: &AST,
  symbols: &mut Vec<lsp::DocumentSymbol>,
) {
  match expression {
    Expression::Binary(binary) => {
      expression_symbols(binary.left(ast), file, ast, symbols);
      expression_symbols(binary.right(ast), file, ast, symbols);
    }
    Expression::Block(block) => {
      for statement in block.statements(ast) {
        statement_symbols(statement, file, ast, symbols);
      }
    }
    Expression::Call(call) => {
      expression_symbols(call.callee(ast), file, ast, symbols);
      if let Some(argument) = call.argument(ast) {
        expression_symbols(argument, file, ast, symbols);
      }
    }
    Expression::Comment(comment) => expression_symbols(comment.expression(ast), file, ast, symbols),
    Expression::FormatString(format_string) => {
      for expression in format_string.expressions(ast) {
        expression_symbols(expression, file, ast, symbols);
      }
    }
    Expression::Function(function) => expression_symbols(function.body(ast), file, ast, symbols),
    Expression::Group(group) => expression_symbols(group.expression(ast), file, ast, symbols),
    Expression::If(if_) => {
      expression_symbols(if_.condition(ast), file, ast, symbols);
      expression_symbols(if_.then(ast), file, ast, symbols);
      if let Some(otherwise) = if_.otherwise(ast) {
        expression_symbols(otherwise, file, ast, symbols);
      }
    }
    Expression::Match(match_) => {
      expression_symbols(match_.value(ast), file, ast, symbols);
      for case in match_.arms() {
        expression_symbols(case.expression(ast), file, ast, symbols);
      }
    }
    Expression::Unary(unary) => expression_symbols(unary.expression(ast), file, ast, symbols),
    Expression::Literal(_)
    | Expression::ModuleAccess(_)
    | Expression::Variable(_)
    | Expression::Invalid(_) => {}
  }
}
