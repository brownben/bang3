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
    Self::new(self.start.min(other.start), self.end.max(other.end))
  }

  /// Get the source text for a `Span` from a source string
  #[must_use]
  pub fn source_text(self, source_text: &str) -> &str {
    let start = self.start as usize;
    let end = self.end as usize;

    &source_text[start..end]
  }
}

#[allow(clippy::module_name_repetitions)]
/// Get a `Span` for part of an AST or from an error
pub trait GetSpan {
  /// Get the `Span` associated with this AST node or diagnostic
  fn span(&self) -> Span;
}
