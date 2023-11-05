//! Source positions and related helper functions.
//!
//! Based on [oxc_span](https://github.com/web-infra-dev/oxc)

#[must_use]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
  pub start: u32,
  pub end: u32,
}

impl Span {
  #[inline]
  pub const fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }

  #[must_use]
  pub fn size(self) -> u32 {
    debug_assert!(self.start <= self.end);
    self.end - self.start
  }

  pub fn merge(self, other: Self) -> Self {
    Self::new(self.start.min(other.start), self.end.max(other.end))
  }

  #[must_use]
  pub fn source_text(self, source_text: &str) -> &str {
    &source_text[self.start as usize..self.end as usize]
  }
}

pub trait GetSpan {
  fn span(&self) -> Span;
}
