use crate::{object::NativeFunction, Value};
use bang_gc::Heap;

use std::fmt;

/// A context is the environment in which the VM runs.
///
/// It is responsible for defining the global functions which can be called,
/// and for importing values from modules.
pub trait Context: fmt::Debug {
  /// Defines the functions which should be registered globally for the VM
  fn global_functions(&self) -> Vec<NativeFunction> {
    Vec::new()
  }

  /// Import a value from a module
  fn import_value(&self, heap: &mut Heap, module: &str, item: &str) -> ImportResult {
    let (_heap, _module, _item) = (heap, module, item);
    ImportResult::ModuleNotFound
  }
}

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Value(Value),
  /// The module was not found
  ModuleNotFound,
  /// The item was not found in the module
  ItemNotFound,
}
impl<T: Into<Value>> From<T> for ImportResult {
  fn from(value: T) -> Self {
    ImportResult::Value(value.into())
  }
}

/// An empty context, where nothing can be imported and has no global function
#[derive(Clone, Debug)]
pub struct Empty;
impl Context for Empty {}
