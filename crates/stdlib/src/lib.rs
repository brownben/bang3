//! # Standard library
//!
//! Definitions for modules in the standard library.

use bang_interpreter::{
  Context, ErrorKind, ImportResult, Value,
  object::{NativeFunction, SOME_TYPE_ID},
};

mod macros;
mod modules;

/// A context which provides the standard library
#[derive(Clone, Debug)]
pub struct StandardContext;
impl Context for StandardContext {
  fn global_constants(&self) -> Vec<(&'static str, Value)> {
    vec![("None", Value::NONE)]
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
      NativeFunction::new("Some", |vm, arg| Ok(vm.allocate_value(arg, SOME_TYPE_ID))),
    ]
  }

  fn import_value(&self, module: &str, item: &str) -> ImportResult {
    match module {
      "maths" => modules::maths(item),
      "string" => modules::string(item),
      "list" => modules::list(item),
      "option" => modules::option(item),
      "iter" => modules::iter(item),
      "assert" => modules::assert(item),
      _ => ImportResult::ModuleNotFound,
    }
  }
}

/// Information about a module in the standard library
pub trait StdlibModule {
  /// The name of the module
  fn name(&self) -> &'static str;
  /// The items in the module
  fn items(&self) -> &'static [&'static str];
  /// Get the documentation for an item in the module
  fn docs(&self, item: &str) -> Option<&'static str>;
  /// Get the type of an item in the module
  fn type_of(&self, item: &str) -> Option<&'static str>;
}

/// The modules in bang's standard library
pub const MODULES: [&dyn StdlibModule; 6] = [
  &modules::StringModule,
  &modules::MathsModule,
  &modules::ListModule,
  &modules::OptionModule,
  &modules::IterModule,
  &modules::AssertModule,
];
/// The names of all the modules in the standard library
pub const MODULE_NAMES: [&str; 6] = ["string", "maths", "list", "option", "iter", "assert"];
