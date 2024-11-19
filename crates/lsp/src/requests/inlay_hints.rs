use super::variables::find_variable;
use crate::documents::Document;
use crate::locations::{lsp_position_from_span_end, span_from_lsp_range};
use lsp_types as lsp;

use bang_syntax::ast::Statement;

pub fn inlay_hints(file: &Document, range: lsp::Range) -> Vec<lsp::InlayHint> {
  let range = span_from_lsp_range(range, file);

  file
    .ast
    .all_statements()
    .filter_map(|statement| match statement {
      Statement::Let(declaration) => Some(declaration),
      _ => None,
    })
    .filter(|declaration| range.contains(declaration.span(&file.ast)))
    .filter(|declaration| declaration.annotation(&file.ast).is_none())
    .filter_map(|declaration| {
      let typechecker = file.typechecker();
      let variable = find_variable(declaration.identifier_span(&file.ast), typechecker)?;
      let type_info = variable.get_type_info()?;

      Some((declaration, &type_info.string))
    })
    .map(|(declaration, ty)| lsp::InlayHint {
      position: lsp_position_from_span_end(declaration.identifier_span(&file.ast), file),
      label: lsp::InlayHintLabel::String(format!(": {ty}")),
      kind: Some(lsp::InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: Some(false),
      padding_right: Some(false),
      data: None,
    })
    .collect()
}
