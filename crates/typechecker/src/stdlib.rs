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
  /// The `option` module
  Option,
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
      "option" => Self::Option,
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
      Self::Option => "option",
      Self::Unknown => "",
    }
  }

  /// Get the type of a value imported from a module.
  pub(crate) fn import_value(&self, types: &mut TypeArena, item: &str) -> ImportResult {
    match self {
      Self::Maths => maths_module(types, item),
      Self::String => string_module(types, item),
      Self::List => list_module(types, item),
      Self::Option => option_module(types, item),
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
      Self::Option => bang_interpreter::stdlib::option_docs(item),
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
      Self::Option => &bang_interpreter::stdlib::OPTION_ITEMS,
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

fn option_module(types: &mut TypeArena, item: &str) -> ImportResult {
  let generic = types.new_type(Type::Quantified(0));
  let generic_b = types.new_type(Type::Quantified(1));
  let generic_option = types.new_type(Type::Structure(Structure::Option, generic));
  let generic_option_b = types.new_type(Type::Structure(Structure::Option, generic_b));
  let generic_option_option = types.new_type(Type::Structure(Structure::Option, generic_option));

  let x_to_option_x = types.new_type(Type::Function(generic, generic_option));
  let x_option_to_bool = types.new_type(Type::Function(generic_option, TypeArena::BOOLEAN));
  let option_x_to_x = types.new_type(Type::Function(generic_option, generic));
  let x_to_option_x_to_x = types.new_type(Type::Function(generic, option_x_to_x));
  let option_option_x_to_option_x =
    types.new_type(Type::Function(generic_option_option, generic_option));

  let a_to_b = types.new_type(Type::Function(generic, generic_b));
  let option_a_to_option_b = types.new_type(Type::Function(generic_option, generic_option_b));
  let a_to_b_to_option_a_to_option_b = types.new_type(Type::Function(a_to_b, option_a_to_option_b));

  let generic_into_import_result = |item| ImportResult::Value(TypeScheme::new(item, 1));

  match item {
    "None" => generic_into_import_result(generic_option),
    "Some" => generic_into_import_result(x_to_option_x),
    "isNone" | "isSome" => generic_into_import_result(x_option_to_bool),
    "unwrap" => generic_into_import_result(option_x_to_x),
    "unwrapOr" => generic_into_import_result(x_to_option_x_to_x),
    "flatten" => generic_into_import_result(option_option_x_to_option_x),
    "map" => ImportResult::Value(TypeScheme::new(a_to_b_to_option_a_to_option_b, 2)),
    _ => ImportResult::ItemNotFound,
  }
}

#[cfg(test)]
mod test {
  use super::{ImportResult, MODULES, TypeArena};
  use super::{list_module, maths_module, option_module, string_module};
  use bang_interpreter::stdlib::{LIST_ITEMS, MATHS_ITEMS, OPTION_ITEMS, STRING_ITEMS};
  use bang_interpreter::stdlib::{list_docs, maths_docs, option_docs, string_docs};
  use indoc::indoc;

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

    for item in OPTION_ITEMS {
      let ty = option_module(&mut types, item);
      assert!(matches!(ty, ImportResult::Value { .. }));
    }
  }

  #[test]
  fn check_example_in_docs_typecheck() {
    for item in STRING_ITEMS {
      typecheck_docs("string", item, string_docs(item));
    }

    for item in MATHS_ITEMS {
      typecheck_docs("maths", item, maths_docs(item));
    }

    for item in LIST_ITEMS {
      typecheck_docs("list", item, list_docs(item));
    }

    for item in OPTION_ITEMS {
      typecheck_docs("option", item, option_docs(item));
    }
  }

  /// Gets an example from a docstring if it exists
  fn get_doc_example(docs: &str) -> String {
    docs
      .lines()
      .skip_while(|line| !line.trim_end().ends_with("```bang"))
      .skip(1)
      .take_while(|line| !line.trim_end().ends_with("```"))
      .fold(String::new(), |a, b| a + b + "\n")
      .trim_end()
      .to_string()
  }

  /// Typechecks a docstring example
  fn typecheck_docs(module: &str, item: &str, docs: Option<&'static str>) {
    let doc_example = get_doc_example(docs.unwrap());

    let ast = bang_syntax::parse(doc_example);
    if !ast.is_valid() {
      panic!(
        indoc! {"
          `{}::{}`'s documentation example could not be parsed

          ```bang
          {}
          ```
        "},
        module, item, ast.source
      );
    }

    let type_checker = crate::TypeChecker::check(&ast);
    if !type_checker.problems().is_empty() {
      let errors = (type_checker.problems.iter())
        .map(|error| error.full_message())
        .fold(String::new(), |a, b| a + b.as_str() + "\n\n");

      panic!(
        indoc! {"
          `{}::{}`'s documentation example has a type error

          ```bang
          {}
          ```

          Errors:

          {}
        "},
        module,
        item,
        ast.source,
        errors.trim_end()
      );
    }
  }
}
