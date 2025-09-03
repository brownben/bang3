use std::ops;

use crate::{
  enviroment::{StaticTypeInfo, VariableType},
  types::{Type, TypeArena, TypeRef, TypeScheme},
};
pub use bang_stdlib::MODULE_NAMES;

/// The result of importing a value from a module.
#[derive(Clone)]
pub enum ImportResult {
  /// The value which was requested
  Value(TypeScheme),
  /// The item was not found in the module
  ItemNotFound,
}
impl From<TypeRef> for ImportResult {
  fn from(type_: TypeRef) -> Self {
    Self::Value(TypeScheme::monomorphic(type_))
  }
}

/// Get type and doc information for a module in the standard library.
pub struct StdlibModule(&'static dyn bang_stdlib::StdlibModule);
impl StdlibModule {
  /// Gets a module by name.
  pub fn get(module_name: &str) -> Option<Self> {
    bang_stdlib::MODULES
      .into_iter()
      .find(|module| module.name() == module_name)
      .map(Self)
  }

  /// Get the name of the module.
  #[must_use]
  pub fn name(&self) -> &'static str {
    self.0.name()
  }

  /// Get the type of a value imported from a module.
  pub(crate) fn import_value(&self, types: &mut TypeArena, item: &str) -> ImportResult {
    let Some(type_annotation) = self.0.type_of(item) else {
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
}
impl ops::Deref for StdlibModule {
  type Target = dyn bang_stdlib::StdlibModule;

  fn deref(&self) -> &Self::Target {
    self.0
  }
}
