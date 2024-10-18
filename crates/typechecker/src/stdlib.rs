use crate::{
  enviroment::{StaticTypeInfo, VariableKind},
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
  fn from(value: TypeRef) -> Self {
    ImportResult::Value(TypeScheme::monomorphic(value))
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

/// Get the [`StaticTypeInfo`] for a value imported from a module.
#[must_use]
pub fn import_type_info(module: &str, item: &str) -> StaticTypeInfo {
  let mut types = TypeArena::new();

  let result = match module {
    "maths" => maths_module(&mut types, item),
    "string" => string_module(&mut types, item),
    _ => ImportResult::ModuleNotFound,
  };
  let result = if let ImportResult::Value(value) = result {
    value.type_
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

fn maths_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let number_to_number = types.new_type(Type::Function(TypeArena::NUMBER, TypeArena::NUMBER));

  match item {
    "PI" | "E" | "INFINITY" => TypeArena::NUMBER.into(),

    "floor" | "ceil" | "round" | "abs" | "sqrt" | "cbrt" | "sin" | "cos" | "tan" | "asin"
    | "acos" | "atan" | "sinh" | "cosh" | "tanh" | "asinh" | "acosh" | "atanh" | "isNan"
    | "exp" | "ln" | "radiansToDegrees" | "degreesToRadians" => number_to_number.into(),

    _ => ImportResult::ItemNotFound,
  }
}

fn string_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let string_to_number = types.new_type(Type::Function(TypeArena::STRING, TypeArena::NUMBER));
  let string_to_bool = types.new_type(Type::Function(TypeArena::STRING, TypeArena::BOOLEAN));
  let string_to_string = types.new_type(Type::Function(TypeArena::STRING, TypeArena::STRING));

  match item {
    "NEW_LINE" | "TAB" | "CARRIAGE_RETURN" => TypeArena::STRING.into(),

    "length" => string_to_number.into(),
    "isEmpty" | "isAscii" => string_to_bool.into(),
    "toLowercase" | "toUppercase" => string_to_string.into(),

    _ => ImportResult::ItemNotFound,
  }
}

#[cfg(test)]
mod test {
  use super::{maths_module, string_module, ImportResult, TypeArena};
  use bang_interpreter::stdlib::{MATHS_ITEMS, STRING_ITEMS};

  #[test]
  fn all_items_present() {
    let mut types = TypeArena::new();

    for item in STRING_ITEMS {
      let ty = string_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value(_)));
    }

    for item in MATHS_ITEMS {
      let ty = maths_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value(_)));
    }
  }
}
