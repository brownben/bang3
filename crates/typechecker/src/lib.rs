//! # Typechecker
//! Typechecks Bang source code
#![feature(let_chains)]
#![deny(unsafe_code)]

mod enviroment;
mod error;
mod exhaustive;
mod infer;
mod stdlib;
mod types;

#[cfg(test)]
mod test;

use bang_syntax::AST;
pub use enviroment::{Enviroment, Variable, VariableKind};
pub use error::Problem as TypeError;
use infer::TypeChecker;
pub use stdlib::import_type_info;

/// Runs the typechecker against a given AST, returns a list of errors found
///
/// # Examples
/// ```
/// use bang_syntax::parse;
/// use bang_typechecker::typecheck;
///
/// let source = "5 + 3";
/// let ast = parse(source);
/// let typecheck_result = typecheck(&ast);
/// ```
#[must_use]
pub fn typecheck(ast: &AST) -> Vec<TypeError> {
  let mut checker = TypeChecker::new();
  checker.check_ast(ast);
  checker.check_unused_variables();
  checker.problems
}

/// Gets the enviroment of defined variables for a given AST
#[must_use]
pub fn get_enviroment(ast: &AST) -> Enviroment {
  let mut checker = TypeChecker::new();
  checker.check_ast(ast);
  checker.env.add_static_type_info(&mut checker.types);
  checker.env
}
