use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::collections::HashMap;

use bang_syntax::{ast::expression, Span, AST};
use bang_typechecker::{import_type_info, TypeChecker, Variable};

pub fn hover(file: &Document, position: lsp::Position) -> Option<lsp::Hover> {
  let position = span_from_lsp_position(position, file);

  if let Some(module_access) = in_module_access_item(&file.ast, position) {
    return Some(hover_module_access_item(&file.ast, module_access));
  }

  let typechecker = file.typechecker();
  let variable = find_variable(position, typechecker)?;

  let variable_name = &variable.name;
  let variable_type = &variable.get_type_info().unwrap().string;

  Some(lsp::Hover {
    contents: lsp::HoverContents::Scalar(lsp::MarkedString::from_language_code(
      "bang".to_owned(),
      format!("let {variable_name}: {variable_type}",),
    )),
    range: None,
  })
}

fn hover_module_access_item(ast: &AST, module_access: &expression::ModuleAccess) -> lsp::Hover {
  let item_name = module_access.item(ast);
  let type_ = import_type_info(module_access.module(ast), item_name);

  lsp::Hover {
    contents: lsp::HoverContents::Scalar(lsp::MarkedString::from_language_code(
      "bang".to_owned(),
      format!("let {item_name}: {}", type_.string),
    )),
    range: None,
  }
}

pub fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = span_from_lsp_position(position, file);

  let typechecker = file.typechecker();
  let declaration = find_variable(position, typechecker)?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(declaration.span(), file),
  ))
}

pub fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = span_from_lsp_position(position, file);

  let typechecker = file.typechecker();
  let declaration = find_variable(position, typechecker)?;

  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location::new(file.id.clone(), lsp_range_from_span(*span, file)))
    .collect();

  Some(references)
}

pub fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = span_from_lsp_position(position, file);

  let typechecker = file.typechecker();

  let Some(declaration) = find_variable(position, typechecker) else {
    return lsp::WorkspaceEdit::default();
  };

  let mut text_edits = Vec::new();

  if declaration.is_import
    && let Some(alias) = declaration.alias
  {
    text_edits.push(lsp::TextEdit {
      range: lsp_range_from_span(alias, file),
      new_text: new_name.to_owned(),
    });
  } else if declaration.is_import {
    let mut span = declaration.span();
    span.start = span.end;

    text_edits.push(lsp::TextEdit {
      range: lsp_range_from_span(span, file),
      new_text: format!(" as {new_name}"),
    });
  } else {
    text_edits.push(lsp::TextEdit {
      range: lsp_range_from_span(declaration.span(), file),
      new_text: new_name.to_owned(),
    });
  };

  for used in &declaration.used {
    text_edits.push(lsp::TextEdit {
      range: lsp_range_from_span(*used, file),
      new_text: new_name.to_owned(),
    });
  }

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}

fn find_variable(position: Span, typechecker: &TypeChecker) -> Option<&Variable> {
  typechecker
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      var.defined.contains(position)
        || var.alias.is_some_and(|x| x.contains(position))
        || var.used.iter().any(|used| used.contains(position))
    })
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
