use crate::documents::Document;
use bang_syntax::Span;

pub fn span_from_lsp_position(position: lsp_types::Position, file: &Document) -> Span {
  let line_index = &file.line_index;
  let line = usize::try_from(position.line).unwrap() + 1;

  let start = if line_index.is_ascii {
    // if ascii, utf8 and utf16 are the same
    return line_index.span_from_position(line - 1, position.character);
  } else {
    let line_start = line_index.get_line_start(line - 1);
    let line_source = line_index.line_span(line).source_text(&file.source);

    line_start + byte_offset_from_utf16_offset(position.character, line_source)
  };

  Span { start, end: start }
}

fn byte_offset_from_utf16_offset(utf16_position: u32, string: &str) -> u32 {
  let mut byte_offset = 0;
  let mut utf16_offset = 0u32;

  for char in string.chars() {
    if utf16_offset >= utf16_position {
      break;
    }

    byte_offset += u32::try_from(char.len_utf8()).unwrap();
    utf16_offset += u32::try_from(char.len_utf16()).unwrap();
  }

  byte_offset
}

pub fn lsp_range_from_span(span: Span, file: &Document) -> lsp_types::Range {
  let lines = &file.line_index;

  let start_line = lines.line(span) - 1;
  let start_char = span.start - lines.get_line_start(start_line);

  let end_line = lines.final_line(span) - 1;
  let end_char = span.end - lines.get_line_start(end_line);

  let (start_char, end_char) = if lines.is_ascii {
    (start_char, end_char)
  } else {
    let start_line_source = lines.line_span(start_line + 1).source_text(&file.source);
    let end_line_source = lines.line_span(end_line + 1).source_text(&file.source);

    (
      byte_offset_to_utf16(start_char, start_line_source),
      byte_offset_to_utf16(end_char, end_line_source),
    )
  };

  let start = lsp_types::Position::new(start_line.try_into().unwrap(), start_char);
  let end = lsp_types::Position::new(end_line.try_into().unwrap(), end_char);

  lsp_types::Range::new(start, end)
}

fn byte_offset_to_utf16(position: u32, string: &str) -> u32 {
  let position: usize = usize::try_from(position).unwrap().min(string.len());

  string[..position]
    .encode_utf16()
    .count()
    .try_into()
    .unwrap()
}

#[cfg(test)]
mod test {
  use super::{byte_offset_from_utf16_offset, byte_offset_to_utf16, Span};

  fn utf16_slice(string: &str, start: usize, end: usize) -> String {
    String::from_utf16_lossy(
      &string
        .encode_utf16()
        .skip(start)
        .take(end - start)
        .collect::<Vec<_>>(),
    )
  }

  #[test]
  fn byte_offset_to_utf16_constructed_span() {
    let source = "let string = '🏃'";

    let span = Span { start: 4, end: 10 };
    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "string");
    assert_eq!(utf16_slice(source, start, end), "string");

    let span = Span { start: 13, end: 19 };
    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "'🏃'");
    assert_eq!(utf16_slice(source, start, end), "'🏃'");
  }

  #[test]
  fn byte_offset_to_utf16_unknown_character_span() {
    let source = "let string = '🏃' &";

    let ast = bang_syntax::parse(source);
    let span = ast.errors.first().unwrap().span();

    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "&");
    assert_eq!(utf16_slice(source, start, end), "&");
  }

  #[test]
  fn byte_offset_to_utf16_unused_variable_span() {
    let source = "let string = '🏃'";

    let ast = bang_syntax::parse(source);
    let errors = bang_typechecker::typecheck(&ast);
    let span = errors.first().unwrap().span();

    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "string");
    assert_eq!(utf16_slice(source, start, end), "string");
  }

  #[test]
  fn byte_offset_from_utf16_offset_constructed_span() {
    let source = "let var = '🏃' ++ string";

    let start = byte_offset_from_utf16_offset(18, &source);
    let end = byte_offset_from_utf16_offset(24, &source);
    let span = Span { start, end };

    assert_eq!(span.source_text(source), "string");
  }
}
