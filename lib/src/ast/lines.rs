//! Get the line number of a span.
//!
//! Based on `LineIndex` from [Ruff](https://github.com/astral-sh/ruff)

use super::Span;

type FilePosition = u32;
type LineNumber = usize;

/// Index for looking up the line number from source positions
///
/// Line numbers given start at 1
#[must_use]
#[derive(Debug)]
pub struct LineIndex {
  line_starts: Vec<FilePosition>,
  file_length: FilePosition,
}
impl LineIndex {
  /// Create a new `LineIndex` from a source string.
  ///
  /// # Panics
  ///
  /// Panics if the source string is longer than `u32::MAX` bytes.
  #[allow(clippy::cast_possible_truncation, reason = "source.len() < u32::MAX")]
  pub fn from_source(source: &str) -> Self {
    assert!(source.len() < u32::MAX as usize);

    let mut line_starts = Vec::with_capacity(source.len() / 50);
    line_starts.push(0);

    for (index, character) in source.as_bytes().iter().enumerate() {
      if *character == b'\n' {
        line_starts.push(index as FilePosition + 1);
      }
    }

    Self {
      line_starts,
      file_length: source.len() as FilePosition,
    }
  }

  /// Get the line number which a `Span` starts on
  #[must_use]
  pub fn get_line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.start) {
      Ok(line) => line + 1,
      Err(line) => line,
    }
  }

  /// Get the line number which a `Span` ends on
  #[must_use]
  pub fn get_final_line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.end) {
      Ok(line) => line + 1,
      Err(line) => line,
    }
  }

  /// Get the line and character offset for the start of a `Span`
  #[must_use]
  pub fn get_offset(&self, span: Span) -> (LineNumber, u32) {
    let line = self.get_line(span);
    let character = span.start - self.line_starts[line - 1];

    (line, character)
  }

  /// Get the line and character offset for the end of a `Span`
  #[must_use]
  pub fn get_final_offset(&self, span: Span) -> (LineNumber, u32) {
    let line = self.get_final_line(span);
    let character = span.end - self.line_starts[line - 1];

    (line, character)
  }

  /// Change a line number and a character into a byte offset
  #[must_use]
  pub fn to_offset(&self, line: LineNumber, character: u32) -> u32 {
    self.line_starts[line] + character
  }

  /// Get a `Span` for a given line number
  ///
  /// # Panics
  /// Panics if the line number doesn't exist
  pub fn line_span(&self, line: LineNumber) -> Span {
    let start = self.line_starts[line - 1];
    let end = *self.line_starts.get(line).unwrap_or(&self.file_length);

    Span::new(start, end)
  }

  /// Get a `Span` for the entire file
  pub fn get_file_span(&self) -> Span {
    Span {
      start: 0,
      end: self.file_length,
    }
  }
}
