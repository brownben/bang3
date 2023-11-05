//! Wrapper around arena allocator library

use std::ops;

#[derive(Debug, Default)]
pub struct Allocator(bumpalo::Bump);
impl Allocator {
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
