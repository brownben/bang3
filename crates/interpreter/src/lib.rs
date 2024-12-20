//! # Bytecode Interpreter
//! Bytecode compiler and virtual machine for running Bang code.

#![feature(let_chains)]
#![allow(unsafe_code)]

mod bytecode;
mod compiler;
mod object;
pub mod stdlib;
mod value;
mod vm;

/// Compile an AST into a bytecode chunk
///
/// # Examples
/// ```
/// use bang_syntax::parse;
/// let ast = parse("5 + 3".to_owned());
/// let chunk = bang_interpreter::compile(&ast).unwrap();
/// ```
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile(ast: &bang_syntax::AST) -> Result<Chunk, CompileError> {
  compiler::Compiler::compile(ast)
}

/// Compile an expression into a bytecode chunk, which returns the value of the expression
///
/// Useful for a REPL or other interactive environments where you want to evaluate an expression
///
/// # Errors
/// If there is a problem constructing the bytecode
pub fn compile_expression(
  expression: &bang_syntax::ast::Expression,
  ast: &bang_syntax::AST,
) -> Result<Chunk, CompileError> {
  compiler::Compiler::compile_expression(expression, ast)
}

pub use bang_gc::HeapSize;
pub use bytecode::{Chunk, ChunkBuilder, OpCode};
pub use compiler::CompileError;
pub use stdlib::{EmptyContext, StandardContext};
pub use value::Value;
pub use vm::{Config, RuntimeError, VM};
