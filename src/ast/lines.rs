//! Get the line number of a span.
//!
//! Based on LineIndex from [Ruff](https://github.com/astral-sh/ruff)

use super::Span;

type FilePosition = u32;
type LineNumber = u16;

pub struct LineIndex {
  line_starts: Vec<FilePosition>,
}
impl LineIndex {
  pub fn from_source(source: &str) -> Self {
    let mut line_starts = Vec::with_capacity(source.len() / 50);
    line_starts.push(0);

    for (index, character) in source.as_bytes().iter().enumerate() {
      if *character == b'\n' {
        line_starts.push(index as FilePosition + 1);
      }
    }

    Self { line_starts }
  }

  pub fn get_line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.start) {
      Ok(line) => line as LineNumber,
      Err(line) => line as LineNumber,
    }
  }

  pub fn get_final_line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.end) {
      Ok(line) => line as LineNumber,
      Err(line) => line as LineNumber,
    }
  }

  pub fn line_span(&self, line: LineNumber) -> Span {
    let start = self.line_starts[line as usize - 1];
    let end = self.line_starts[line as usize];

    Span::new(start, end)
  }
}
