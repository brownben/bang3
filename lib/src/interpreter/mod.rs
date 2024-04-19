#![allow(unsafe_code)] // allow unsafe code for optimisations

mod alloc;
mod bytecode;
mod compiler;
mod value;
mod vm;

#[cfg(test)]
mod test;

pub use bytecode::Chunk;
pub use compiler::{CompileError, Compiler};
pub use value::{FastNativeFunction, Value};
pub use vm::{RuntimeError, VM};
