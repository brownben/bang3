use crate::documents::Document;
use bang_syntax::Span;

pub fn span_from_lsp_position(position: lsp_types::Position, file: &Document) -> Span {
  let line_index = &file.ast.line_index();
  let line = usize::try_from(position.line).unwrap() + 1;

  let start = if line_index.is_ascii {
    // if ascii, utf8 and utf16 are the same
    return line_index.span_from_position(line - 1, position.character);
  } else {
    let line_start = line_index.get_line_start(line - 1);
    let line_source = line_index.line_span(line).source_text(&file.ast.source);

    line_start + byte_offset_from_utf16_offset(position.character, line_source)
  };

  Span { start, end: start }
}

pub fn span_from_lsp_range(range: lsp_types::Range, file: &Document) -> Span {
  let start = span_from_lsp_position(range.start, file);
  let end = span_from_lsp_position(range.end, file);

  start.merge(end)
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

pub fn lsp_position_from_span_start(span: Span, file: &Document) -> lsp_types::Position {
  let lines = &file.ast.line_index();

  let line = lines.line(span) - 1;
  let char = span.start - lines.get_line_start(line);

  let char = if lines.is_ascii {
    char
  } else {
    let line_source = lines.line_span(line + 1).source_text(&file.ast.source);
    byte_offset_to_utf16(char, line_source)
  };

  lsp_types::Position::new(line.try_into().unwrap(), char)
}
pub fn lsp_position_from_span_end(span: Span, file: &Document) -> lsp_types::Position {
  let lines = &file.ast.line_index();

  let line = lines.final_line(span) - 1;
  let char = span.end - lines.get_line_start(line);

  let char = if lines.is_ascii {
    char
  } else {
    let line_source = lines.line_span(line + 1).source_text(&file.ast.source);
    byte_offset_to_utf16(char, line_source)
  };

  lsp_types::Position::new(line.try_into().unwrap(), char)
}

pub fn lsp_range_from_span(span: Span, file: &Document) -> lsp_types::Range {
  let start = lsp_position_from_span_start(span, file);
  let end = lsp_position_from_span_end(span, file);

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

    let ast = bang_syntax::parse(source.to_owned());
    let span = ast.errors.first().unwrap().span();

    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "&");
    assert_eq!(utf16_slice(source, start, end), "&");
  }

  #[test]
  fn byte_offset_to_utf16_unused_variable_span() {
    let source = "let string = '🏃'";

    let ast = bang_syntax::parse(source.to_owned());
    let typechecker = bang_typechecker::TypeChecker::check(&ast);
    let errors = typechecker.problems();
    let span = errors.first().unwrap().span();

    let start = byte_offset_to_utf16(span.start, source).try_into().unwrap();
    let end = byte_offset_to_utf16(span.end, source).try_into().unwrap();
    assert_eq!(span.source_text(source), "string");
    assert_eq!(utf16_slice(source, start, end), "string");
  }

  #[test]
  fn byte_offset_from_utf16_offset_constructed_span() {
    let source = "let var = '🏃' ++ string";

    let start = byte_offset_from_utf16_offset(18, source);
    let end = byte_offset_from_utf16_offset(24, source);
    let span = Span { start, end };

    assert_eq!(span.source_text(source), "string");
  }
}
