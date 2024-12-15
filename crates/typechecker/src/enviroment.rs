//! Tracks variables, their types, and where they are defined and used

use crate::stdlib::StdlibModule;
use crate::types::{Structure, Type, TypeArena, TypeRef, TypeScheme};
use bang_syntax::{Span, ast::statement::ImportItem};
use indoc::indoc;
use std::cell::OnceCell;

/// Holds variables and where they are defined and used
#[derive(Debug)]
pub struct Enviroment {
  variables: Vec<Variable>,
  finished_variables: Vec<Variable>,
  depth: u32,
}
impl Enviroment {
  pub(crate) fn new() -> Self {
    Self {
      variables: Vec::new(),
      finished_variables: Vec::new(),
      depth: 0,
    }
  }

  pub(crate) fn define_builtin_variables(&mut self, types: &mut TypeArena) {
    let generic_a = types.new_type(Type::Quantified(0));
    let generic_b = types.new_type(Type::Quantified(1));
    let string = TypeArena::STRING;

    let print_function = TypeScheme::new(types.new_type(Type::Function(generic_a, generic_a)), 1);
    let type_function = TypeScheme::new(types.new_type(Type::Function(generic_a, string)), 1);
    let panic_function = TypeScheme::new(types.new_type(Type::Function(generic_a, generic_b)), 2);

    let option = types.new_type(Type::Structure(Structure::Option, generic_a));
    let none = TypeScheme::new(option, 1);
    let some = TypeScheme::new(types.new_type(Type::Function(generic_a, option)), 1);

    self.variables.push(Variable {
      kind: VariableKind::Builtin {
        name: "print",
        documentation: Some(indoc! {"
          Prints a value to stdout.

          Returns the value so it can be used inplace to inspect values.

          ## Example
          ```
          print('Hello, world!') // 'Hello, world!' written to stdout
          let x = print(10) // x = 10, and '10' written to stdout
          ```
        "}),
      },

      used: Vec::new(),
      active: None,

      depth: self.depth,
      type_: print_function,

      type_info: OnceCell::from(StaticTypeInfo::new_function("^a => ^a")),
    });
    self.variables.push(Variable {
      kind: VariableKind::Builtin {
        name: "type",
        documentation: Some(indoc! {"
          Gets the type of a value as a string

          ## Example
          ```
          type(10) // 'number'
          type('Hello, world!') // 'string'
          type(true) // 'boolean'
          type(x => x) // 'function'
          ```
        "}),
      },

      used: Vec::new(),
      active: None,

      depth: self.depth,
      type_: type_function,

      type_info: OnceCell::from(StaticTypeInfo::new_function("^a => string")),
    });
    self.variables.push(Variable {
      kind: VariableKind::Builtin {
        name: "panic",
        documentation: Some(indoc! {"
          Stops all execution and displays an error with the given value as the message.

          ## Example
          ```
          panic('This is an error')
          // The interpreter has stopped executing
          ```
        "}),
      },

      used: Vec::new(),
      active: None,

      depth: self.depth,
      type_: panic_function,

      type_info: OnceCell::from(StaticTypeInfo::new_function("^a => ^b")),
    });
    self.variables.push(Variable {
      kind: VariableKind::Builtin {
        name: "None",
        documentation: Some(indoc! {"
          option::None

          No value
          ```
        "}),
      },

      used: Vec::new(),
      active: None,

      depth: self.depth,
      type_: none,

      type_info: OnceCell::from(StaticTypeInfo::new_variable("option<^a>")),
    });
    self.variables.push(Variable {
      kind: VariableKind::Builtin {
        name: "Some",
        documentation: Some(indoc! {"
          option::Some

          Some value
          ```
        "}),
      },

      used: Vec::new(),
      active: None,

      depth: self.depth,
      type_: some,

      type_info: OnceCell::from(StaticTypeInfo::new_variable("^a => option<^a>")),
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
      finished_var.active = finished_var.active.map(|span| span.merge(end_span));
      self.finished_variables.push(finished_var);
    }

    self.depth -= 1;
  }

  pub(crate) fn define_variable(
    &mut self,
    variable_name: &str,
    variable_span: Span,
    type_: TypeRef,
    documentation: Option<String>,
  ) {
    self.variables.push(Variable {
      kind: VariableKind::Declaration {
        name: variable_name.to_owned(),
        defined: variable_span,
        documentation,
      },

      used: Vec::new(),
      active: Some(variable_span),

      depth: self.depth,
      type_: TypeScheme::monomorphic(type_),

      type_info: OnceCell::new(),
    });
  }

  pub(crate) fn define_import(
    &mut self,
    item: &ImportItem,
    type_: TypeScheme,
    module: &'static str,
  ) {
    self.variables.push(Variable {
      kind: VariableKind::Import {
        module,
        item: item.name.to_owned(),
        alias: item.alias.map(ToOwned::to_owned),
        alias_span: item.alias_span,
        defined: item.span,
      },

      used: Vec::new(),
      active: Some(item.span),

      depth: self.depth,
      type_,

      type_info: OnceCell::new(),
    });
  }

  pub(crate) fn update_variable(&mut self, identifier: &str, type_: TypeScheme) {
    let variable = self
      .variables
      .iter_mut()
      .rev()
      .find(|variable| variable.name() == identifier)
      .unwrap();

    variable.type_ = type_;
  }

  pub(crate) fn get_variable(&mut self, identifier: &str) -> Option<TypeScheme> {
    for variable in self.variables.iter().rev() {
      if variable.name() == identifier {
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
      .find(|variable| variable.name() == identifier);

    if let Some(variable) = &mut variable {
      variable.used.push(span);
    }
  }

  /// An iterator over all active variables in the current scope
  pub(crate) fn variables(&self) -> impl DoubleEndedIterator<Item = &Variable> {
    self.variables.iter()
  }

  /// An iterator over all the variables that are defined
  pub(crate) fn finished_variables(&self) -> impl DoubleEndedIterator<Item = &Variable> {
    self.finished_variables.iter()
  }
}

/// A variable which is defined and all the times it is used
#[derive(Debug)]
pub struct Variable {
  /// the kind of the variable, either builtin, declaration, or import
  pub kind: VariableKind,

  /// the spans where the variable was used
  pub used: Vec<Span>,
  /// the span where the variable is active and can be used
  ///
  /// `None` if the variable can always be used
  active: Option<Span>,

  /// the depth of the variable, used to track the scope of the variable
  depth: u32,
  /// the type of the variable
  type_: TypeScheme,

  /// the type of the variable, as static independant information
  /// is not automatically added, using [`Enviroment::add_static_type_info`]
  type_info: OnceCell<StaticTypeInfo>,
}

/// Is a variable defined by a builtin, a declaration, or an import
#[derive(Debug)]
pub enum VariableKind {
  /// a variable that is defined by the user, in a `let` statement
  Declaration {
    /// the identifier of the variable
    name: String,

    /// documentation from a doc comment for the variable
    documentation: Option<String>,

    /// the span where the variable was defined
    defined: Span,
  },
  /// a variable that is defined by the runtime
  Builtin {
    /// the identifier of the variable
    name: &'static str,
    /// documentation for the builtin
    documentation: Option<&'static str>,
  },
  /// a variable that is defined from an import statement
  Import {
    /// the module the item is imported from
    module: &'static str,
    /// the name of the item imported
    item: String,
    /// the alias of the imported item
    alias: Option<String>,
    /// the span of the alias
    alias_span: Option<Span>,

    /// the span where the variable was imported
    defined: Span,
  },
}

impl Variable {
  /// Checks if the variable is used
  pub(crate) fn is_used(&self) -> bool {
    !self.used.is_empty()
  }

  /// Is the variable active at the given position
  #[must_use]
  pub fn is_active(&self, position: Span) -> bool {
    self.active.is_none_or(|active| active.contains(position))
  }

  pub(crate) fn type_(&self) -> TypeRef {
    self.type_.raw_type()
  }

  pub(crate) fn add_static_type_info(&self, types: &TypeArena) {
    _ = self.type_info.set(StaticTypeInfo {
      string: types.type_to_string(self.type_()),
      kind: match types[self.type_.raw_type()] {
        Type::Function(arg, _) if types.is_never(arg) => VariableType::FunctionNoArgs,
        Type::Function(_, _) => VariableType::Function,
        _ => VariableType::Variable,
      },
    });
  }

  /// The information about the type of the variable
  pub fn get_type_info(&self) -> Option<&StaticTypeInfo> {
    self.type_info.get()
  }

  /// The name of a variable
  pub fn name(&self) -> &str {
    match &self.kind {
      VariableKind::Declaration { name, .. } => name,
      VariableKind::Builtin { name, .. } => name,
      VariableKind::Import {
        alias: Some(alias), ..
      } => alias,
      VariableKind::Import { item, .. } => item,
    }
  }

  /// The documentation for a variable (it's doc comment)
  pub fn documentation(&self) -> Option<&str> {
    match &self.kind {
      VariableKind::Declaration { documentation, .. } => documentation.as_deref(),
      VariableKind::Builtin { documentation, .. } => documentation.as_deref(),
      VariableKind::Import { module, item, .. } => {
        StdlibModule::get(module).and_then(|module| module.docs(item))
      }
    }
  }

  /// The location of the variable definition
  pub fn span(&self) -> Option<Span> {
    match self.kind {
      VariableKind::Declaration { defined, .. } => Some(defined),
      VariableKind::Builtin { .. } => None,
      VariableKind::Import { defined, .. } => Some(defined),
    }
  }
}

/// The static information about the type of a variable
/// (including the string representation of the type)
#[derive(Debug, Clone)]
pub struct StaticTypeInfo {
  /// The string representation of the type
  pub string: String,
  /// The kind of the variable
  pub kind: VariableType,
}
impl StaticTypeInfo {
  pub(crate) fn new_function(string: &str) -> Self {
    Self {
      string: string.to_owned(),
      kind: VariableType::Function,
    }
  }
  pub(crate) fn new_variable(string: &str) -> Self {
    Self {
      string: string.to_owned(),
      kind: VariableType::Variable,
    }
  }
}
/// An overview of the variable type, is it a function or a standard value
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableType {
  /// A regular variable
  Variable,
  /// A function
  Function,
  /// A function which expects no arguments
  FunctionNoArgs,
}
impl VariableType {
  /// Is the variable a function?
  #[must_use]
  pub fn is_function(&self) -> bool {
    match self {
      VariableType::Variable => false,
      VariableType::Function | VariableType::FunctionNoArgs => true,
    }
  }
}
