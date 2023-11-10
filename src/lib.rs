#![feature(let_chains)]

mod allocator;
mod ast;
mod linter;
mod parser;

pub use allocator::Allocator;
pub use ast::AST;
pub use linter::{Diagnostic, Linter};
pub use parser::{ParseError, Parser};
