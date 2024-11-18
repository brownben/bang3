use crate::{
  enviroment::{StaticTypeInfo, VariableKind},
  types::{Type, TypeArena, TypeRef, TypeScheme},
};

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Value {
    type_: TypeScheme,
    module: &'static str,
  },
  /// The module was not found
  ModuleNotFound,
  /// The item was not found in the module
  ItemNotFound,
}
impl From<(&'static str, TypeRef)> for ImportResult {
  fn from((module, type_): (&'static str, TypeRef)) -> Self {
    ImportResult::Value {
      type_: TypeScheme::monomorphic(type_),
      module,
    }
  }
}

/// Get the type of a value imported from a module.
pub fn import_value(types: &mut TypeArena, module: &str, item: &str) -> ImportResult {
  match module {
    "maths" => maths_module(types, item),
    "string" => string_module(types, item),
    _ => ImportResult::ModuleNotFound,
  }
}

/// Get the documentation of a value imported from a module.
#[must_use]
pub fn import_docs(module: &str, item: &str) -> Option<&'static str> {
  use bang_interpreter::stdlib::{maths_docs, string_docs};

  match module {
    "maths" => maths_docs(item),
    "string" => string_docs(item),
    _ => None,
  }
}

/// Get the [`StaticTypeInfo`] for a value imported from a module.
#[must_use]
pub fn import_type_info(module: &str, item: &str) -> StaticTypeInfo {
  let mut types = TypeArena::new();

  let result = match module {
    "maths" => maths_module(&mut types, item),
    "string" => string_module(&mut types, item),
    _ => ImportResult::ModuleNotFound,
  };
  let result = if let ImportResult::Value { type_, .. } = result {
    type_.type_
  } else {
    TypeArena::UNKNOWN
  };

  StaticTypeInfo {
    string: types.type_to_string(result),
    kind: match types[result] {
      Type::Function(_, _) => VariableKind::Function,
      _ => VariableKind::Variable,
    },
  }
}

/// The names of all the modules in the standard library
pub const MODULES: [&str; 2] = ["maths", "string"];

fn maths_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let number_to_number = types.new_type(Type::Function(TypeArena::NUMBER, TypeArena::NUMBER));
  let number_number_to_number = types.new_type(Type::Function(TypeArena::NUMBER, number_to_number));

  let type_ref = match item {
    "PI" | "E" | "INFINITY" | "NAN" => TypeArena::NUMBER,

    "floor" | "ceil" | "round" | "abs" | "sqrt" | "cbrt" | "sin" | "cos" | "tan" | "asin"
    | "acos" | "atan" | "sinh" | "cosh" | "tanh" | "asinh" | "acosh" | "atanh" | "isNan"
    | "exp" | "ln" | "radiansToDegrees" | "degreesToRadians" => number_to_number,

    "pow" | "log" => number_number_to_number,

    _ => return ImportResult::ItemNotFound,
  };

  ImportResult::from(("maths", type_ref))
}

fn string_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let string_to_number = types.new_type(Type::Function(TypeArena::STRING, TypeArena::NUMBER));
  let string_to_bool = types.new_type(Type::Function(TypeArena::STRING, TypeArena::BOOLEAN));
  let string_to_string = types.new_type(Type::Function(TypeArena::STRING, TypeArena::STRING));
  let string_string_to_bool = types.new_type(Type::Function(TypeArena::STRING, string_to_bool));

  let type_ref = match item {
    "NEW_LINE" | "TAB" | "CARRIAGE_RETURN" => TypeArena::STRING,

    "length" | "byteLength" => string_to_number,
    "isEmpty" | "isAscii" => string_to_bool,
    "toLowercase" | "toUppercase" | "trim" | "trimStart" | "trimEnd" => string_to_string,
    "contains" | "startsWith" | "endsWith" => string_string_to_bool,

    _ => return ImportResult::ItemNotFound,
  };

  ImportResult::from(("string", type_ref))
}

#[cfg(test)]
mod test {
  use super::{maths_module, string_module, ImportResult, TypeArena, MODULES};
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
