use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::collections::HashMap;

use bang_syntax::Span;
use bang_typechecker::{TypeChecker, Variable, VariableKind};

pub fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = span_from_lsp_position(position, file);

  let typechecker = file.typechecker();
  let declaration = find_variable(position, typechecker)?;
  let span = declaration.span()?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(span, file),
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

  match declaration.kind {
    // Builtins cannot be renamed
    VariableKind::Builtin { .. } => return lsp::WorkspaceEdit::default(),
    // Regular rename of variable
    VariableKind::Declaration { defined, .. } => text_edits.push(lsp::TextEdit {
      range: lsp_range_from_span(defined, file),
      new_text: new_name.to_owned(),
    }),
    // If an import has an alias, rename the alias
    VariableKind::Import { alias_span, .. } if alias_span.is_some() => {
      text_edits.push(lsp::TextEdit {
        range: lsp_range_from_span(alias_span.unwrap(), file),
        new_text: new_name.to_owned(),
      });
    }
    // If an import has no alias, add an alias
    VariableKind::Import { mut defined, .. } => {
      defined.start = defined.end;

      text_edits.push(lsp::TextEdit {
        range: lsp_range_from_span(defined, file),
        new_text: format!(" as {new_name}"),
      });
    }
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
    .variables()
    .filter(|variable| variable.is_active(position))
    .find(|var| {
      let in_declaration = match var.kind {
        VariableKind::Builtin { .. } => false,
        VariableKind::Declaration { defined, .. } => defined.contains(position),
        VariableKind::Import {
          defined,
          alias_span,
          ..
        } => defined.contains(position) || alias_span.is_some_and(|x| x.contains(position)),
      };

      in_declaration || var.used.iter().any(|x| x.contains(position))
    })
}
