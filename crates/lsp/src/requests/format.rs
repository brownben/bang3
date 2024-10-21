use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_formatter::{format, FormatterConfig};
use bang_syntax::parse;

pub fn format_file(file: &Document) -> Option<Vec<lsp::TextEdit>> {
  let ast = parse(&file.source);

  if !ast.is_formattable() {
    // don't format if the file is not valid
    return None;
  }

  let config = FormatterConfig::default(); // TODO: request parameter options
  let new_text = format(&ast, config);

  Some(vec![lsp::TextEdit {
    range: lsp_range_from_span(file.line_index.file_span(), file),
    new_text,
  }])
}
