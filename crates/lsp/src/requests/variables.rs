use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::collections::HashMap;

use bang_syntax::Span;
use bang_typechecker::{TypeChecker, Variable};

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

pub fn find_variable(position: Span, typechecker: &TypeChecker) -> Option<&Variable> {
  typechecker
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      var.defined.contains(position)
        || var.alias.is_some_and(|x| x.contains(position))
        || var.used.iter().any(|used| used.contains(position))
    })
}
