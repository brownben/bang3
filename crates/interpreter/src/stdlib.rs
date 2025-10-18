//! # Standard library
//!
//! Definitions for modules in the standard library.

use crate::{Value, object::NativeFunction};
use std::fmt;

/// A context is the environment in which the VM runs.
///
/// It is responsible for defining the global functions which can be called,
/// and for importing values from modules.
pub trait Context: fmt::Debug {
  /// Defines the constants which should be registered globally for the VM
  fn global_constants(&self) -> Vec<(&'static str, Value)> {
    Vec::new()
  }

  /// Defines the functions which should be registered globally for the VM
  fn global_functions(&self) -> Vec<NativeFunction> {
    Vec::new()
  }

  /// Import a value from a module
  fn import_value(&self, module: &str, item: &str) -> ImportResult {
    let (_module, _item) = (module, item);
    ImportResult::ModuleNotFound
  }
}

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Constant(Value),
  /// A constant string which was requested
  ConstantString(&'static str),
  /// A native function which was requested
  NativeFunction(NativeFunction),
  /// The module was not found
  ModuleNotFound,
  /// The item was not found in the module
  ItemNotFound,
}
impl<T: Into<Value>> From<T> for ImportResult {
  fn from(value: T) -> Self {
    Self::Constant(value.into())
  }
}

/// An empty context, where nothing can be imported and has no global function
#[derive(Clone, Debug)]
pub struct EmptyContext;
impl Context for EmptyContext {}
