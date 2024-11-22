//! # Typechecker
//! Typechecks Bang source code
#![feature(let_chains)]
#![deny(unsafe_code)]

mod enviroment;
mod error;
mod exhaustive;
mod infer;
mod similarity;
mod stdlib;
mod types;

#[cfg(test)]
mod test;

use bang_syntax::AST;
use enviroment::{BuiltinVariable, Enviroment};
use infer::InferType;
use types::TypeArena;

pub use enviroment::{StaticTypeInfo, Variable, VariableKind};
pub use error::Problem as TypeError;
pub use stdlib::StdlibModule;

/// Type checker to infer types of Bang code, and check it is correct
pub struct TypeChecker {
  pub(crate) types: TypeArena,
  pub(crate) env: Enviroment,
  pub(crate) problems: Vec<TypeError>,
}
impl TypeChecker {
  /// Creates a new typechecker
  fn new() -> Self {
    let mut types = TypeArena::new();
    let mut env = Enviroment::new();

    env.define_builtin_variables(&mut types);

    Self {
      types,
      env,
      problems: Vec::new(),
    }
  }

  /// Creates a new typechecker, and checks the given AST for type errors
  ///
  /// # Examples
  /// ```
  /// use bang_syntax::parse;
  /// use bang_typechecker::TypeChecker;
  ///
  /// let source = "5 + 3".to_owned();
  /// let ast = parse(source);
  ///
  /// let typechecker = TypeChecker::check(&ast);
  /// assert!(typechecker.problems().is_empty());
  /// ```
  pub fn check(ast: &AST) -> Self {
    let mut typechecker = Self::new();
    ast.infer(&mut typechecker, ast);
    typechecker
  }

  /// Gets the list of problems found in the AST
  #[must_use]
  pub fn problems(&self) -> &[TypeError] {
    &self.problems
  }

  /// Returns an iterator over all the variables defined
  pub fn defined_variables(&self) -> impl Iterator<Item = &Variable> {
    self
      .env
      .defined_variables()
      .inspect(|variable| variable.add_static_type_info(&self.types))
  }

  /// Returns an iterator over all the builtin variables
  pub fn builtin_variables(&self) -> impl Iterator<Item = &BuiltinVariable> {
    self.env.builtin_variables()
  }
}
