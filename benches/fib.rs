use bang::Allocator;
use codspeed_bencher_compat::{benchmark_group, benchmark_main, Bencher};
use std::hint::black_box;

fn parse(b: &mut Bencher) {
  let allocator = Allocator::new();
  let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));

  b.iter(|| bang::parse(black_box(source), &allocator));
}

fn compile(b: &mut Bencher) {
  let allocator = Allocator::new();
  let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));
  let ast = bang::parse(source, &allocator).unwrap();

  b.iter(|| bang::compile(black_box(&ast)));
}

fn run(b: &mut Bencher) {
  let allocator = Allocator::new();
  let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));
  let ast = bang::parse(source, &allocator).unwrap();
  let chunk = bang::compile(&ast).unwrap();

  b.iter(|| {
    let mut vm = bang::VM::new();
    vm.run(black_box(&chunk))
  });
}

fn parse_compile_run(b: &mut Bencher) {
  b.iter(|| {
    let allocator = Allocator::new();
    let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));
    let ast = bang::parse(black_box(source), &allocator).unwrap();
    let chunk = bang::compile(&ast).unwrap();
    let mut vm = bang::VM::new();
    vm.run(&chunk)
  });
}

fn lint(b: &mut Bencher) {
  let allocator = Allocator::new();
  let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));
  let ast = bang::parse(source, &allocator).unwrap();

  b.iter(|| bang::lint(black_box(&ast)));
}

fn format(b: &mut Bencher) {
  let allocator = Allocator::new();
  let source = include_str!(concat!("../examples/", "fibonacci", ".bang"));
  let ast = bang::parse(source, &allocator).unwrap();

  b.iter(|| bang::format(black_box(&ast), bang::FormatterConfig::default()));
}

benchmark_group!(
  benches,
  parse,
  compile,
  run,
  parse_compile_run,
  lint,
  format
);
benchmark_main!(benches);
