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

use bang_parser::AST;
pub use enviroment::{Enviroment, Variable, VariableKind};
pub use error::Problem as TypeError;
use infer::Typechecker;
pub use stdlib::import_type_info;
use types::{Type, TypeArena, TypeRef, TypeScheme};

/// Runs the typechecker against a given AST, returns a list of errors found
///
/// # Examples
/// ```
/// use bang_parser::{parse, Allocator};
/// use bang_typechecker::typecheck;
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let typecheck_result = typecheck(&ast);
/// ```
#[must_use]
pub fn typecheck(ast: &AST) -> Vec<TypeError> {
  let mut checker = Typechecker::new();

  checker.check_ast(ast);

  for variable in checker.env.defined_variables() {
    checker.types.type_to_string(variable.type_());
  }

  checker.problems
}

/// Gets the enviroment of defined variables for a given AST
#[must_use]
pub fn get_enviroment(ast: &AST) -> Enviroment {
  let mut checker = Typechecker::new();

  checker.check_ast(ast);
  checker.env.add_static_type_info(&mut checker.types);

  checker.env
}
