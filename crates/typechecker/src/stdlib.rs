use crate::{
  enviroment::{StaticTypeInfo, VariableType},
  types::{Type, TypeArena, TypeRef, TypeScheme},
};

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Value(TypeScheme),
  /// The module was not found
  ModuleNotFound,
  /// The item was not found in the module
  ItemNotFound,
}
impl From<TypeRef> for ImportResult {
  fn from(type_: TypeRef) -> Self {
    ImportResult::Value(TypeScheme::monomorphic(type_))
  }
}

/// Get type and doc information for a module in the standard library.
#[must_use]
pub struct StdlibModule(&'static str);
impl StdlibModule {
  /// Gets a module by name.
  pub fn get(module: &str) -> Self {
    Self(match module {
      "maths" => "maths",
      "string" => "string",
      _ => "",
    })
  }

  /// Get the name of the module.
  #[must_use]
  pub fn name(&self) -> &'static str {
    self.0
  }

  /// Get the type of a value imported from a module.
  pub(crate) fn import_value(&self, types: &mut TypeArena, item: &str) -> ImportResult {
    match self.name() {
      "maths" => maths_module(types, item),
      "string" => string_module(types, item),
      _ => ImportResult::ModuleNotFound,
    }
  }

  /// Get the [`StaticTypeInfo`] for a value imported from a module.
  #[must_use]
  pub fn type_info(&self, item: &str) -> StaticTypeInfo {
    let mut types = TypeArena::new();

    let result = if let ImportResult::Value(type_) = self.import_value(&mut types, item) {
      type_.type_
    } else {
      TypeArena::UNKNOWN
    };

    StaticTypeInfo {
      string: types.type_to_string(result),
      kind: match types[result] {
        Type::Function(_, _) => VariableType::Function,
        _ => VariableType::Variable,
      },
    }
  }

  /// Get the documentation of a value imported from a module.
  #[must_use]
  pub fn docs(&self, item: &str) -> Option<&'static str> {
    match self.name() {
      "maths" => bang_interpreter::stdlib::maths_docs(item),
      "string" => bang_interpreter::stdlib::string_docs(item),
      _ => None,
    }
  }

  /// Get a list of items in a module
  #[must_use]
  pub fn items(&self) -> &'static [&'static str] {
    match self.name() {
      "maths" => &bang_interpreter::stdlib::MATHS_ITEMS,
      "string" => &bang_interpreter::stdlib::STRING_ITEMS,
      _ => &[],
    }
  }
}

/// The names of all the modules in the standard library
pub const MODULES: [&str; 2] = ["maths", "string"];

fn maths_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let number_to_number = types.new_type(Type::Function(TypeArena::NUMBER, TypeArena::NUMBER));
  let number_number_to_number = types.new_type(Type::Function(TypeArena::NUMBER, number_to_number));

  match item {
    "PI" | "E" | "INFINITY" | "NAN" => TypeArena::NUMBER.into(),

    "floor" | "ceil" | "round" | "abs" | "sqrt" | "cbrt" | "sin" | "cos" | "tan" | "asin"
    | "acos" | "atan" | "sinh" | "cosh" | "tanh" | "asinh" | "acosh" | "atanh" | "isNan"
    | "exp" | "ln" | "radiansToDegrees" | "degreesToRadians" => number_to_number.into(),

    "pow" | "log" => number_number_to_number.into(),

    _ => ImportResult::ItemNotFound,
  }
}

fn string_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let string_to_number = types.new_type(Type::Function(TypeArena::STRING, TypeArena::NUMBER));
  let string_to_bool = types.new_type(Type::Function(TypeArena::STRING, TypeArena::BOOLEAN));
  let string_to_string = types.new_type(Type::Function(TypeArena::STRING, TypeArena::STRING));
  let string_string_to_bool = types.new_type(Type::Function(TypeArena::STRING, string_to_bool));

  match item {
    "NEW_LINE" | "TAB" | "CARRIAGE_RETURN" => TypeArena::STRING.into(),

    "length" | "byteLength" => string_to_number.into(),
    "isEmpty" | "isAscii" => string_to_bool.into(),
    "toLowercase" | "toUppercase" | "trim" | "trimStart" | "trimEnd" => string_to_string.into(),
    "contains" | "startsWith" | "endsWith" => string_string_to_bool.into(),

    _ => ImportResult::ItemNotFound,
  }
}

#[cfg(test)]
mod test {
  use super::{ImportResult, MODULES, TypeArena, maths_module, string_module};
  use bang_interpreter::stdlib::{MATHS_ITEMS, STRING_ITEMS};

  #[test]
  fn all_items_present() {
    let mut types = TypeArena::new();

    for module in MODULES {
      assert!(bang_interpreter::stdlib::MODULES.contains(&module));
    }

    for item in STRING_ITEMS {
      let ty = string_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }

    for item in MATHS_ITEMS {
      let ty = maths_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }
  }
}
