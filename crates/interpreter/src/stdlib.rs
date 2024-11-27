//! # Standard library
//!
//! Definitions for modules in the standard library.

use crate::{VM, Value, object::NativeFunction};
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
  fn import_value(&self, vm: &mut VM, module: &str, item: &str) -> ImportResult {
    let (_vm, _module, _item) = (vm, module, item);
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
pub struct EmptyContext;
impl Context for EmptyContext {}

/// A context which provides the standard library
#[derive(Clone, Debug)]
pub struct StandardContext;
impl Context for StandardContext {
  fn global_functions(&self) -> Vec<NativeFunction> {
    vec![
      #[allow(clippy::print_stdout)]
      NativeFunction::new("print", |vm, arg| {
        println!("{}", arg.display(vm));
        Ok(arg)
      }),
      NativeFunction::new("type", |vm, arg| {
        let type_string = arg.get_type(vm);
        Ok(vm.allocate_string(type_string))
      }),
    ]
  }

  fn import_value(&self, vm: &mut VM, module: &str, item: &str) -> ImportResult {
    match module {
      "maths" => maths(vm, item),
      "string" => string(vm, item),
      _ => ImportResult::ModuleNotFound,
    }
  }
}

mod macros;

/// The names of all the modules in the standard library
pub const MODULES: [&str; 2] = ["maths", "string"];

mod maths;
pub use maths::{MATHS_ITEMS, maths, maths_docs};

mod string;
pub use string::{STRING_ITEMS, string, string_docs};
