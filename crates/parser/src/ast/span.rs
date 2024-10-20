//! Source positions and related helper functions.
//!
//! Based on [oxc_span](https://github.com/web-infra-dev/oxc)

/// Represents a span of the source code
#[must_use]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

  /// Combine span with a possible span
  pub fn merge_option(self, other: Option<Self>) -> Self {
    if let Some(other) = other {
      self.merge(other)
    } else {
      self
    }
  }

  /// Combine two `Span`s into one
  pub fn merge_end(self, other: Self) -> Self {
    if self == Self::default() {
      other
    } else if other == Self::default() {
      self
    } else {
      Self::new(self.start, self.end)
    }
  }

  /// Get the source text for a `Span` from a source string
  #[must_use]
  pub fn source_text(self, source_text: &str) -> &str {
    let start = self.start as usize;
    let end = self.end as usize;

    &source_text[start..end]
  }

  /// Check if a `Span` contains another `Span`
  #[must_use]
  pub fn contains(self, other: Self) -> bool {
    self.start <= other.start && other.end <= self.end
  }
}

#[allow(clippy::module_name_repetitions)]
/// Get a `Span` for part of an AST or from an error
pub trait GetSpan {
  /// Get the `Span` associated with this AST node or diagnostic
  fn span(&self) -> Span;
}
