use super::variables::find_variable;
use crate::documents::Document;
use crate::locations::span_from_lsp_position;
use lsp_types as lsp;

use bang_stdlib::MODULES;
use bang_syntax::{AST, Span, ast::expression};

pub fn hover(file: &Document, position: lsp::Position) -> Option<lsp::Hover> {
  let position = span_from_lsp_position(position, file);

  if let Some(module_access) = in_module_access_item(&file.ast, position) {
    return hover_module_access_item(&file.ast, module_access);
  }

  let typechecker = file.typechecker();
  let variable = find_variable(position, typechecker)?;

  let variable_name = &variable.name();
  let variable_type = &variable.get_type_info().unwrap().string;

  let mut contents = type_code_block(variable_name, variable_type);
  if let Some(documentation) = variable.documentation() {
    contents.push_str("---\n");
    contents.push_str(documentation);
  }

  Some(lsp::Hover {
    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
      kind: lsp_types::MarkupKind::Markdown,
      value: contents,
    }),
    range: None,
  })
}

fn hover_module_access_item(
  ast: &AST,
  module_access: &expression::ModuleAccess,
) -> Option<lsp::Hover> {
  let module_name = module_access.module(ast);
  let item_name = module_access.item(ast);

  let module = MODULES.iter().find(|module| module.name() == module_name)?;

  let mut contents = type_code_block(item_name, module.type_of(item_name).unwrap_or(""));
  if let Some(documentation) = module.docs(item_name) {
    contents.push_str("---\n");
    contents.push_str(documentation);
  }

  Some(lsp::Hover {
    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
      kind: lsp_types::MarkupKind::Markdown,
      value: contents,
    }),
    range: None,
  })
}

fn type_code_block(variable_name: &str, variable_type: &str) -> String {
  let mut code_block = String::new();

  code_block.push_str("```bang\n");
  code_block.push_str("let ");
  code_block.push_str(variable_name);
  code_block.push_str(": ");
  code_block.push_str(variable_type);
  code_block.push_str("\n```\n");

  code_block
}

fn in_module_access_item(ast: &AST, position: Span) -> Option<&expression::ModuleAccess> {
  for expression in &ast.expressions {
    if let expression::Expression::ModuleAccess(module_access) = expression
      && module_access.item_span(ast).contains(position)
    {
      return Some(module_access);
    }
  }

  None
}
