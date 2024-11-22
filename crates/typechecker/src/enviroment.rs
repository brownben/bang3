//! Tracks variables, their types, and where they are defined and used

use crate::stdlib::StdlibModule;
use crate::types::{Type, TypeArena, TypeRef, TypeScheme};
use bang_syntax::{ast::statement::ImportItem, Span};
use std::cell::OnceCell;

/// Holds variables and where they are defined and used
#[derive(Debug)]
pub struct Enviroment {
  variables: Vec<Variable>,
  finished_variables: Vec<Variable>,
  builtin_variables: Vec<BuiltinVariable>,
  depth: u32,
}
impl Enviroment {
  pub(crate) fn new() -> Self {
    Self {
      variables: Vec::new(),
      finished_variables: Vec::new(),
      builtin_variables: Vec::new(),
      depth: 0,
    }
  }

  pub(crate) fn define_builtin_variables(&mut self, types: &mut TypeArena) {
    let generic = types.new_type(Type::Quantified(0));

    let print_function = TypeScheme {
      number_quantified_vars: 1,
      type_: types.new_type(Type::Function(generic, generic)),
    };
    let x_string_function = TypeScheme {
      number_quantified_vars: 1,
      type_: types.new_type(Type::Function(generic, TypeArena::STRING)),
    };

    self.builtin_variables.push(BuiltinVariable {
      name: "print",
      type_: print_function,
      type_info: StaticTypeInfo::new_function("(a' -> a')"),
    });
    self.builtin_variables.push(BuiltinVariable {
      name: "type",
      type_: x_string_function,
      type_info: StaticTypeInfo::new_function("(a' -> string)"),
    });
  }

  pub(crate) fn enter_scope(&mut self) {
    self.depth += 1;
  }
  pub(crate) fn exit_scope(&mut self, end_span: Span) {
    while let Some(Variable { depth, .. }) = self.variables.last()
      && *depth == self.depth
    {
      let mut finished_var = self.variables.pop().unwrap();
      finished_var.active = finished_var.active.merge(end_span);
      self.finished_variables.push(finished_var);
    }

    self.depth -= 1;
  }

  pub(crate) fn define_variable(
    &mut self,
    variable_name: &str,
    variable_span: Span,
    type_: TypeScheme,
    documentation: Option<String>,
  ) {
    self.variables.push(Variable {
      name: variable_name.to_owned(),
      defined: variable_span,
      used: Vec::new(),
      active: variable_span,

      module: None,
      alias: None,

      depth: self.depth,
      type_,

      type_info: OnceCell::new(),
      documentation,
    });
  }

  pub(crate) fn define_import(
    &mut self,
    item: &ImportItem,
    type_: TypeScheme,
    module: &'static str,
  ) {
    let name = if let Some(alias) = &item.alias {
      alias
    } else {
      item.name
    };

    self.variables.push(Variable {
      name: name.to_owned(),
      defined: item.span,
      used: Vec::new(),
      active: item.span,

      module: Some(module),
      alias: item.alias_span,

      depth: self.depth,
      type_,

      type_info: OnceCell::new(),
      documentation: None,
    });
  }

  pub(crate) fn update_variable(&mut self, identifier: &str, type_: TypeScheme) {
    let variable = self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name == identifier)
      .unwrap();

    variable.type_ = type_;
  }

  pub(crate) fn get_variable(&mut self, identifier: &str) -> Option<TypeScheme> {
    for variable in self.variables.iter().rev() {
      if variable.name == identifier {
        return Some(variable.type_);
      }
    }

    for variable in &self.builtin_variables {
      if variable.name == identifier {
        return Some(variable.type_);
      }
    }

    None
  }

  pub(crate) fn mark_variable_use(&mut self, identifier: &str, span: Span) {
    let mut variable = self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name == identifier);

    if let Some(variable) = &mut variable {
      variable.used.push(span);
    }
  }

  /// An iterator over all active variables in the current scope
  pub(crate) fn variables(&self) -> impl Iterator<Item = &Variable> {
    self.variables.iter()
  }

  /// An iterator over all the variables that are defined
  pub(crate) fn defined_variables(&self) -> impl Iterator<Item = &Variable> {
    self.finished_variables.iter()
  }

  /// An iterator over all the builtin variables that are defined
  pub(crate) fn builtin_variables(&self) -> impl Iterator<Item = &BuiltinVariable> {
    self.builtin_variables.iter()
  }
}

/// A variable which is defined and all the times it is used
#[derive(Debug)]
pub struct Variable {
  /// the identifier of the variable
  pub name: String,
  /// the span where the variable was defined
  pub defined: Span,
  /// the spans where the variable was used
  pub used: Vec<Span>,
  /// the span where the variable is active and can be used
  active: Span,

  /// the module the variable is imported from if it is imported
  pub module: Option<&'static str>,
  /// the location of the alias, if the import is aliased
  pub alias: Option<Span>,

  /// the depth of the variable, used to track the scope of the variable
  depth: u32,
  /// the type of the variable
  type_: TypeScheme,

  /// the type of the variable, as static independant information
  /// is not automatically added, using [`Enviroment::add_static_type_info`]
  type_info: OnceCell<StaticTypeInfo>,
  /// documentation from a doc comment for the variable
  documentation: Option<String>,
}
impl Variable {
  /// Checks if the variable is used
  pub(crate) fn is_used(&self) -> bool {
    !self.used.is_empty()
  }

  /// Is the variable defined from an import
  #[must_use]
  pub fn is_import(&self) -> bool {
    self.module.is_some()
  }

  /// Is the variable active at the given position
  #[must_use]
  pub fn is_active(&self, position: Span) -> bool {
    self.active.contains(position)
  }

  pub(crate) fn type_(&self) -> TypeRef {
    self.type_.type_
  }

  pub(crate) fn add_static_type_info(&self, types: &TypeArena) {
    _ = self.type_info.set(StaticTypeInfo {
      string: types.type_to_string(self.type_()),
      kind: match types[self.type_.type_] {
        Type::Function(_, _) => VariableKind::Function,
        _ => VariableKind::Variable,
      },
    });
  }

  /// The information about the type of the variable
  pub fn get_type_info(&self) -> Option<&StaticTypeInfo> {
    self.type_info.get()
  }

  /// The documentation for a variable (it's doc comment)
  pub fn documentation(&self) -> Option<&str> {
    if let Some(module) = self.module {
      StdlibModule::get(module).docs(&self.name)
    } else {
      self.documentation.as_deref()
    }
  }

  /// The location of the variable definition
  pub fn span(&self) -> Span {
    self.defined
  }
}

/// A variable which is builtin and defined by the runtime
#[derive(Debug)]
pub struct BuiltinVariable {
  /// the identifier of the variable
  pub name: &'static str,
  /// the type of the variable
  type_: TypeScheme,
  /// the type of the variable, as static independant information
  /// is not automatically added, using [`Enviroment::add_static_type_info`]
  pub type_info: StaticTypeInfo,
}

/// The static information about the type of a variable
/// (including the string representation of the type)
#[derive(Debug, Clone)]
pub struct StaticTypeInfo {
  /// The string representation of the type
  pub string: String,
  /// The kind of the variable
  pub kind: VariableKind,
}
impl StaticTypeInfo {
  pub(crate) fn new_function(string: &str) -> Self {
    Self {
      string: string.to_owned(),
      kind: VariableKind::Function,
    }
  }
}
/// The type of a variable
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableKind {
  /// A regular variable
  Variable,
  /// A function
  Function,
}
