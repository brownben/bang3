//! # Standard library
//!
//! Definitions for modules in the standard library.

use crate::{
  Value,
  object::{self, NONE, NativeFunction},
  vm::{ErrorKind, VM},
};
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
  fn global_constants(&self) -> Vec<(&'static str, Value)> {
    vec![("None", NONE)]
  }
  fn global_functions(&self) -> Vec<NativeFunction> {
    vec![
      #[allow(clippy::print_stdout)]
      NativeFunction::new("print", |vm, arg| {
        if arg == Value::NULL {
          println!();
        } else {
          println!("{}", arg.display(vm));
        }
        Ok(arg)
      }),
      NativeFunction::new("type", |vm, arg| {
        let type_string = arg.get_type(vm);
        Ok(vm.allocate_string(type_string))
      }),
      NativeFunction::new("panic", |vm, arg| {
        let message = if arg == Value::NULL {
          String::new()
        } else {
          arg.display(vm)
        };
        Err(ErrorKind::Custom {
          title: "Panic",
          message,
        })
      }),
      NativeFunction::new("Some", |vm, arg| {
        Ok(Value::from_object(
          vm.heap.allocate(arg),
          object::SOME_TYPE_ID,
        ))
      }),
    ]
  }

  fn import_value(&self, vm: &mut VM, module: &str, item: &str) -> ImportResult {
    match module {
      "maths" => maths(vm, item),
      "string" => string(vm, item),
      "list" => list(vm, item),
      "option" => option(vm, item),
      "iter" => iter(vm, item),
      _ => ImportResult::ModuleNotFound,
    }
  }
}

mod macros;

/// The names of all the modules in the standard library
pub const MODULES: [&str; 5] = ["maths", "string", "list", "option", "iter"];

mod maths;
pub use maths::{MATHS_ITEMS, maths, maths_docs, maths_types};

mod string;
pub use string::{STRING_ITEMS, string, string_docs, string_types};

mod list;
pub use list::{LIST_ITEMS, list, list_docs, list_types};

mod option;
pub use option::{OPTION_ITEMS, option, option_docs, option_types};

mod iter;
pub use iter::{ITER_ITEMS, iter, iter_docs, iter_types};
