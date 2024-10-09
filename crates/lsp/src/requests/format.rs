use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_formatter::{format, FormatterConfig};
use bang_parser::{parse, Allocator};

pub fn format_file(file: &Document) -> Option<Vec<lsp::TextEdit>> {
  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  if !ast.is_valid() {
    // don't format if the file is not valid
    return None;
  }

  let config = FormatterConfig::default(); // TODO: request parameter options
  let new_text = format(&ast, config);

  Some(vec![lsp::TextEdit {
    range: lsp_range_from_span(file.line_index.get_file_span(), file),
    new_text,
  }])
}
