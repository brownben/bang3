use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::{collections::HashMap, iter};

use bang_parser::{parse, Allocator, GetSpan};
use bang_typechecker::get_enviroment;

pub fn hover(file: &Document, position: lsp::Position) -> Option<lsp::Hover> {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let variable = variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      var.defined.contains(position) || var.used.iter().any(|used| used.contains(position))
    })?;

  Some(lsp::Hover {
    contents: lsp::HoverContents::Scalar(lsp::MarkedString::from_language_code(
      "bang-types".to_owned(),
      variable.type_info.as_ref().unwrap().string.clone(),
    )),
    range: None,
  })
}

pub fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let declaration = variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(declaration.span(), file),
  ))
}

pub fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let declaration = variables
    .defined_variables()
    .find(|variable| variable.span().contains(position))?;

  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location::new(file.id.clone(), lsp_range_from_span(*span, file)))
    .collect();

  Some(references)
}

pub fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let Some(declaration) = variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      var.defined.contains(position) || var.used.iter().any(|used| used.contains(position))
    })
  else {
    return lsp::WorkspaceEdit::default();
  };

  let text_edits = iter::once(&declaration.span())
    .chain(declaration.used.iter())
    .map(|span| lsp::TextEdit {
      range: lsp_range_from_span(*span, file),
      new_text: new_name.to_owned(),
    })
    .collect();

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}
