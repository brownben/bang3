use crate::{
  enviroment::{StaticTypeInfo, VariableType},
  types::{Type, TypeArena, TypeRef, TypeScheme},
};
pub use bang_interpreter::stdlib::MODULES;
use bang_interpreter::stdlib::{list_types, maths_types, option_types, string_types};

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
    let type_annotation = match self {
      Self::Maths => maths_types(item),
      Self::String => string_types(item),
      Self::List => list_types(item),
      Self::Option => option_types(item),
      Self::Unknown => return ImportResult::ModuleNotFound,
    };
    let Some(type_annotation) = type_annotation else {
      return ImportResult::ItemNotFound;
    };

    let ast = bang_syntax::parse_type(type_annotation.to_owned());
    let item_type = types.type_from_annotation(ast.get_type(), &ast);
    ImportResult::Value(types.generalize(item_type.unwrap_or(TypeArena::UNKNOWN)))
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
