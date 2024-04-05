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
    fn compile(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
      let ast = bang::parse(source, &allocator).unwrap();

      b.iter(|| bang::compile(black_box(&ast)));
    }

    #[bench]
    fn run(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
      let ast = bang::parse(source, &allocator).unwrap();
      let chunk = bang::compile(&ast).unwrap();

      b.iter(|| {
        let mut vm = bang::VM::new();
        vm.run(black_box(&chunk))
      });
    }

    #[bench]
    fn parse_compile_run(b: &mut Bencher) {
      b.iter(|| {
        let allocator = Allocator::new();
        let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
        let ast = bang::parse(black_box(source), &allocator).unwrap();
        let chunk = bang::compile(&ast).unwrap();
        let mut vm = bang::VM::new();
        vm.run(&chunk)
      });
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

    #[bench]
    fn typecheck(b: &mut Bencher) {
      let allocator = Allocator::new();
      let source = include_str!(concat!("../examples/", stringify!($name), ".bang"));
      let ast = bang::parse(source, &allocator).unwrap();

      b.iter(|| bang::typecheck(black_box(&ast)));
    }
  }
}

example_benchmark!(fibonacci);
example_benchmark!(syntax);
