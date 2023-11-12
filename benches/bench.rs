#![feature(test)]
#![feature(decl_macro)]

extern crate test;

use bang::Allocator;
use test::{black_box, Bencher};

macro example_benchmark($name:ident) {
  mod $name {
    use super::*;

    #[bench]
    fn parse(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));

      b.iter(|| bang::parse(black_box(source), &allocator));
    }
  }
}

example_benchmark!(fibonacci);
example_benchmark!(syntax);
