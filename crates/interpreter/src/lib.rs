//! # Bytecode Interpreter
//! Bytecode compiler and virtual machine for running Bang code.

#![feature(let_chains)]
#![feature(strict_provenance)]
#![feature(decl_macro)]
#![feature(negative_impls)]
#![allow(unsafe_code)]

mod bytecode;
mod compiler;
mod value;
mod vm;

/// More efficient datastructures than in standard library
pub(crate) mod collections {
  pub use rustc_hash::FxHashMap as HashMap;
  pub use smallvec::SmallVec;
  pub use smartstring::alias::String;
}

#[cfg(test)]
mod test;

/// Compile an AST into a bytecode chunk
///
/// # Examples
/// ```
/// use bang_parser::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let chunk = bang_interpreter::compile(&ast, &allocator).unwrap();
/// ```
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile<'s: 'a, 'a>(
  ast: &bang_parser::AST<'s, '_>,
  allocator: &bang_parser::Allocator,
) -> Result<Chunk, CompileError> {
  let mut compiler = compiler::Compiler::new(allocator);
  compiler.compile(ast)?;
  let chunk = compiler.finish();
  Ok(chunk)
}
pub use bytecode::Chunk;
pub use compiler::CompileError;
pub use value::{FastNativeFunction, Value};
pub use vm::{RuntimeError, VM};
