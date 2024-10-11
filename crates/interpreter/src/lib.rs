//! # Bytecode Interpreter
//! Bytecode compiler and virtual machine for running Bang code.

#![feature(let_chains)]
#![feature(strict_provenance)]
#![feature(decl_macro)]
#![feature(negative_impls)]
#![allow(unsafe_code)]

mod bytecode;
mod compiler;
mod object;
mod value;
mod vm;

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
/// let chunk = bang_interpreter::compile(&ast).unwrap();
/// ```
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile(ast: &bang_parser::AST<'_, '_>) -> Result<Chunk, CompileError> {
  let mut compiler = compiler::Compiler::new();
  compiler.compile(ast)?;
  let chunk = compiler.finish();
  Ok(chunk)
}
pub use bang_gc::HeapSize;
pub use bytecode::{Chunk, ChunkBuilder, OpCode};
pub use compiler::CompileError;
pub use value::Value;
pub use vm::{RuntimeError, VM};
