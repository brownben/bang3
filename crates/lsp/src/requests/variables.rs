use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::collections::HashMap;

use bang_syntax::{parse, Span};
use bang_typechecker::{get_enviroment, Enviroment, Variable};

pub fn hover(file: &Document, position: lsp::Position) -> Option<lsp::Hover> {
  let position = span_from_lsp_position(position, file);

  let ast = parse(&file.source);
  let variables = get_enviroment(&ast);
  let variable = find_variable(position, &variables)?;

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

  let ast = parse(&file.source);
  let variables = get_enviroment(&ast);
  let declaration = find_variable(position, &variables)?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(declaration.span(), file),
  ))
}

pub fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = span_from_lsp_position(position, file);

  let ast = parse(&file.source);
  let variables = get_enviroment(&ast);

  let declaration = find_variable(position, &variables)?;
  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location::new(file.id.clone(), lsp_range_from_span(*span, file)))
    .collect();

  Some(references)
}

pub fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = span_from_lsp_position(position, file);

  let ast = parse(&file.source);
  let variables = get_enviroment(&ast);

  let Some(declaration) = find_variable(position, &variables) else {
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

fn find_variable(position: Span, variables: &Enviroment) -> Option<&Variable> {
  variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      var.defined.contains(position)
        || var.alias.is_some_and(|x| x.contains(position))
        || var.used.iter().any(|used| used.contains(position))
    })
}
