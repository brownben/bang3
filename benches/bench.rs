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

    #[bench]
    fn lint(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
      let ast = bang::parse(source, &allocator).unwrap();

      b.iter(|| bang::lint(black_box(&ast)));
    }

    #[bench]
    fn format(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
      let ast = bang::parse(source, &allocator).unwrap();

      b.iter(|| bang::format(black_box(&ast), bang::FormatterConfig::default()));
    }
  }
}

example_benchmark!(fibonacci);
example_benchmark!(syntax);
