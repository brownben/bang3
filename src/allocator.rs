//! # Allocator
//! Wrapper around arena allocator library

use std::ops;

/// Arena allocator
#[must_use]
#[derive(Debug, Default)]
pub struct Allocator(bumpalo::Bump);
impl Allocator {
  /// Create a new allocator
  pub fn new() -> Self {
    Self(bumpalo::Bump::new())
  }
}
impl ops::Deref for Allocator {
  type Target = bumpalo::Bump;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

pub type Box<'allocator, T> = bumpalo::boxed::Box<'allocator, T>;
pub type Vec<'allocator, T> = bumpalo::collections::Vec<'allocator, T>;
