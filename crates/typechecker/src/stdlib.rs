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

#[cfg(test)]
mod test {
  use bang_interpreter::stdlib::{LIST_ITEMS, MATHS_ITEMS, OPTION_ITEMS, STRING_ITEMS};
  use bang_interpreter::stdlib::{list_docs, maths_docs, option_docs, string_docs};
  use indoc::indoc;

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
