use crate::{
  enviroment::{StaticTypeInfo, VariableType},
  types::{Structure, Type, TypeArena, TypeRef, TypeScheme},
};
pub use bang_interpreter::stdlib::MODULES;

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
#[non_exhaustive]
pub enum StdlibModule {
  /// The `maths` module
  Maths,
  /// The `string` module
  String,
  /// The `list` module
  List,
  /// The module is unknown
  Unknown,
}
impl StdlibModule {
  /// Gets a module by name.
  pub fn get(module: &str) -> Self {
    match module {
      "maths" => Self::Maths,
      "string" => Self::String,
      "list" => Self::List,
      _ => Self::Unknown,
    }
  }

  /// Get the name of the module.
  #[must_use]
  pub fn name(&self) -> &'static str {
    match self {
      Self::Maths => "maths",
      Self::String => "string",
      Self::List => "list",
      Self::Unknown => "",
    }
  }

  /// Get the type of a value imported from a module.
  pub(crate) fn import_value(&self, types: &mut TypeArena, item: &str) -> ImportResult {
    match self {
      Self::Maths => maths_module(types, item),
      Self::String => string_module(types, item),
      Self::List => list_module(types, item),
      Self::Unknown => ImportResult::ModuleNotFound,
    }
  }

  /// Get the [`StaticTypeInfo`] for a value imported from a module.
  #[must_use]
  pub fn type_info(&self, item: &str) -> StaticTypeInfo {
    let mut types = TypeArena::new();

    let result = if let ImportResult::Value(type_) = self.import_value(&mut types, item) {
      type_.raw_type()
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
    match self {
      Self::Maths => bang_interpreter::stdlib::maths_docs(item),
      Self::String => bang_interpreter::stdlib::string_docs(item),
      Self::List => bang_interpreter::stdlib::list_docs(item),
      Self::Unknown => None,
    }
  }

  /// Get a list of items in a module
  #[must_use]
  pub fn items(&self) -> &'static [&'static str] {
    match self {
      Self::Maths => &bang_interpreter::stdlib::MATHS_ITEMS,
      Self::String => &bang_interpreter::stdlib::STRING_ITEMS,
      Self::List => &bang_interpreter::stdlib::LIST_ITEMS,
      Self::Unknown => &[],
    }
  }
}

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
  let generic = types.new_type(Type::Quantified(0));
  let x_to_string = types.new_type(Type::Function(generic, TypeArena::STRING));
  let string_to_number = types.new_type(Type::Function(TypeArena::STRING, TypeArena::NUMBER));
  let string_to_bool = types.new_type(Type::Function(TypeArena::STRING, TypeArena::BOOLEAN));
  let string_to_string = types.new_type(Type::Function(TypeArena::STRING, TypeArena::STRING));
  let string_string_to_bool = types.new_type(Type::Function(TypeArena::STRING, string_to_bool));
  let string_string_to_string = types.new_type(Type::Function(TypeArena::STRING, string_to_string));
  let string_string_string_to_string =
    types.new_type(Type::Function(TypeArena::STRING, string_string_to_string));

  match item {
    "NEW_LINE" | "TAB" | "CARRIAGE_RETURN" => TypeArena::STRING.into(),

    "from" | "toString" => ImportResult::Value(TypeScheme::new(x_to_string, 1)),
    "length" | "byteLength" => string_to_number.into(),
    "isEmpty" | "isAscii" => string_to_bool.into(),
    "toLowercase" | "toUppercase" | "trim" | "trimStart" | "trimEnd" => string_to_string.into(),
    "contains" | "startsWith" | "endsWith" => string_string_to_bool.into(),
    "replaceAll" | "replaceOne" => string_string_string_to_string.into(),

    _ => ImportResult::ItemNotFound,
  }
}

fn list_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let generic = types.new_type(Type::Quantified(0));
  let generic_list = types.new_type(Type::Structure(Structure::List, generic));

  let list_to_number = types.new_type(Type::Function(generic_list, TypeArena::NUMBER));
  let list_to_bool = types.new_type(Type::Function(generic_list, TypeArena::BOOLEAN));
  let x_to_list_to_bool = types.new_type(Type::Function(generic, list_to_bool));

  let generic_into_import_result = |item| ImportResult::Value(TypeScheme::new(item, 1));

  match item {
    "length" => generic_into_import_result(list_to_number),
    "isEmpty" => generic_into_import_result(list_to_bool),
    "contains" => generic_into_import_result(x_to_list_to_bool),

    _ => ImportResult::ItemNotFound,
  }
}

#[cfg(test)]
mod test {
  use super::{ImportResult, MODULES, TypeArena, list_module, maths_module, string_module};
  use bang_interpreter::stdlib::{LIST_ITEMS, MATHS_ITEMS, STRING_ITEMS};

  #[test]
  fn all_items_present() {
    let mut types = TypeArena::new();

    for module in MODULES {
      assert!(bang_interpreter::stdlib::MODULES.contains(&module));
    }
    assert_eq!(MODULES.len(), bang_interpreter::stdlib::MODULES.len());

    for item in STRING_ITEMS {
      let ty = string_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }

    for item in MATHS_ITEMS {
      let ty = maths_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }

    for item in LIST_ITEMS {
      let ty = list_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }
  }
}
