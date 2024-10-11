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
  compiler::Compiler::compile(ast)
}

/// Compile an expression into a bytecode chunk, which returns the value of the expression
///
/// Useful for a REPL or other interactive environments where you want to evaluate an expression
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile_expression(
  expression: &bang_parser::ast::Expression,
) -> Result<Chunk, CompileError> {
  compiler::Compiler::compile_expression(expression)
}

pub use bang_gc::HeapSize;
pub use bytecode::{Chunk, ChunkBuilder, OpCode};
pub use compiler::CompileError;
pub use value::Value;
pub use vm::{RuntimeError, VM};
