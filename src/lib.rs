#![feature(let_chains)]

mod allocator;
mod ast;
mod linter;
mod parser;

pub use allocator::Allocator;
pub use ast::AST;
pub use linter::Diagnostic;
pub use parser::ParseError;

pub fn parse<'source, 'ast>(
  source: &'source str,
  allocator: &'ast Allocator,
) -> Result<AST<'source, 'ast>, ParseError> {
  parser::Parser::new(source, allocator).parse()
}

pub fn lint(ast: &AST) -> Vec<Diagnostic> {
  linter::Linter::new().check(ast)
}
