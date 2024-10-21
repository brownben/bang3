//! Source positions and related helper functions.
//!
//! [`Span`] based on [oxc_span](https://github.com/web-infra-dev/oxc)
//! [`LineIndex`] based on [Ruff](https://github.com/astral-sh/ruff)

/// Represents a span of the source code
#[must_use]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
  /// The byte index of the start of the span
  pub start: u32,
  /// The byte index of the end of the span
  pub end: u32,
}

impl Span {
  /// Create a new `Span` from a start and end position
  #[inline]
  pub const fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }

  /// Combine two `Span`s into one
  pub fn merge(self, other: Self) -> Self {
    if self == Self::default() {
      other
    } else if other == Self::default() {
      self
    } else {
      Self::new(self.start.min(other.start), self.end.max(other.end))
    }
  }

  /// Check if a `Span` contains another `Span`
  #[must_use]
  pub const fn contains(self, other: Self) -> bool {
    self.start <= other.start && other.end <= self.end
  }

  /// Get the source text for a `Span` from a source string
  #[must_use]
  pub fn source_text(self, source_text: &str) -> &str {
    let start = self.start as usize;
    let end = self.end as usize;

    &source_text[start..end]
  }
}

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

  /// Is the whole source file ASCII?
  pub is_ascii: bool,
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
      is_ascii: source.is_ascii(),
    }
  }

  /// Get the line number which a `Span` starts on
  #[must_use]
  pub fn line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.start) {
      Ok(line) => line + 1,
      Err(line) => line,
    }
  }

  /// Get the line number which a `Span` ends on
  #[must_use]
  pub fn final_line(&self, span: Span) -> LineNumber {
    match self.line_starts.binary_search(&span.end) {
      Ok(line) => line + 1,
      Err(line) => line,
    }
  }

  /// Change a line number and a character into a byte offset
  pub fn span_from_position(&self, line: LineNumber, character: u32) -> Span {
    let start = self.line_starts[line] + character;

    Span::new(start, start)
  }

  /// Get the byte offset of a given line number
  #[must_use]
  pub fn get_line_start(&self, line: LineNumber) -> u32 {
    self.line_starts[line]
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
  pub fn file_span(&self) -> Span {
    Span {
      start: 0,
      end: self.file_length,
    }
  }
}
