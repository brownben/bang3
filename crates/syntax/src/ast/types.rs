//! # Types

use crate::{
  ast::{AST, TokenIdx, TypeIdx},
  span::Span,
};

/// An type annotation for a variable
#[must_use]
#[derive(Debug)]
pub enum Type {
  /// A primitive type, e.g. `string`, `number`, `boolean`, `never`
  Primitive(TypePrimitive),
  /// A type variable, e.g. `^a`
  Variable(TypeVariable),
  /// A function call, e.g. `number => number`
  Function(TypeFunction),
  /// An type in parentheses, e.g. `(number)`
  Group(TypeGroup),
  /// A type with a parameter for what it contains, e.g. `list<number>`
  Structure(TypeStructure),
  /// An invalid type
  Invalid(TypeInvalid),
}
impl Type {
  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    match self {
      Self::Primitive(primitive) => primitive.span(ast),
      Self::Variable(variable) => variable.span(ast),
      Self::Function(function) => function.span(ast),
      Self::Group(group) => group.span(ast),
      Self::Structure(structure) => structure.span(ast),
      Self::Invalid(invalid) => invalid.span(ast),
    }
  }
}

/// A primitive type, e.g. `string`, `number`, `boolean`, `never`
#[derive(Debug)]
pub struct TypePrimitive {
  pub(crate) token: TokenIdx,
}
impl TypePrimitive {
  /// The name of the primitive
  pub fn name<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.token)
  }

  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}
impl From<TokenIdx> for TypePrimitive {
  fn from(token: TokenIdx) -> Self {
    Self { token }
  }
}

/// A type variable, e.g. `^a`
#[derive(Clone, Debug)]
pub struct TypeVariable {
  pub(crate) caret: TokenIdx,
  pub(crate) name: TokenIdx,
}
impl TypeVariable {
  /// The name of the variable
  #[must_use]
  pub fn name<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.name)
  }

  /// The location of the variable
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.caret]).merge(ast[self.name].into())
  }
}

/// A function, e.g. `x => x + 1`
#[derive(Debug)]
pub struct TypeFunction {
  pub(crate) parameter: TypeIdx,
  pub(crate) return_: TypeIdx,
}
impl TypeFunction {
  /// The parameter type of the function
  pub fn parameter<'a>(&self, ast: &'a AST) -> &'a Type {
    &ast[self.parameter]
  }
  /// The return type of the function
  pub fn return_<'a>(&self, ast: &'a AST) -> &'a Type {
    &ast[self.return_]
  }

  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    ast[self.parameter]
      .span(ast)
      .merge(ast[self.return_].span(ast))
  }
}

/// An type in parentheses, e.g. `(number)`
#[derive(Debug)]
pub struct TypeGroup {
  pub(crate) start: TokenIdx,
  pub(crate) type_: TypeIdx,
  pub(crate) end: Option<TokenIdx>,
}
impl TypeGroup {
  /// The type_ within the parentheses
  pub fn type_<'a>(&self, ast: &'a AST) -> &'a Type {
    &ast[self.type_]
  }

  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    let start = Span::from(ast[self.start]);

    if let Some(end) = self.end {
      start.merge(ast[end].into())
    } else {
      start.merge(self.type_(ast).span(ast))
    }
  }
}

/// A type with a parameter for what it contains, e.g. `list<number>`
#[derive(Debug)]
pub struct TypeStructure {
  pub(crate) structure: TokenIdx,
  pub(crate) opening: TokenIdx,
  pub(crate) parameter: TypeIdx,
  pub(crate) closing: Option<TokenIdx>,
}
impl TypeStructure {
  /// The type of the structure
  pub fn structure<'a>(&self, ast: &'a AST) -> &'a str {
    ast.get_token_text(self.structure)
  }
  /// The parameter of the structure
  pub fn parameter<'a>(&self, ast: &'a AST) -> &'a Type {
    &ast[self.parameter]
  }

  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    let start = Span::from(ast[self.structure]);

    if let Some(end) = self.closing {
      start.merge(ast[end].into())
    } else {
      start.merge(self.parameter(ast).span(ast))
    }
  }
  /// The location of the parameter including brackets
  pub fn parameter_span(&self, ast: &AST) -> Span {
    let start = Span::from(ast[self.opening]);

    if let Some(end) = self.closing {
      start.merge(ast[end].into())
    } else {
      start.merge(self.parameter(ast).span(ast))
    }
  }
}

/// An invalid type
#[derive(Debug)]
pub struct TypeInvalid {
  pub(crate) token: TokenIdx,
}
impl TypeInvalid {
  /// The location of the type
  pub fn span(&self, ast: &AST) -> Span {
    Span::from(ast[self.token])
  }
}

impl From<TypePrimitive> for Type {
  fn from(primitive: TypePrimitive) -> Self {
    Self::Primitive(primitive)
  }
}
impl From<TypeVariable> for Type {
  fn from(variable: TypeVariable) -> Self {
    Self::Variable(variable)
  }
}
impl From<TypeFunction> for Type {
  fn from(function: TypeFunction) -> Self {
    Self::Function(function)
  }
}
impl From<TypeGroup> for Type {
  fn from(group: TypeGroup) -> Self {
    Self::Group(group)
  }
}
impl From<TypeStructure> for Type {
  fn from(parameter: TypeStructure) -> Self {
    Self::Structure(parameter)
  }
}
impl From<TypeInvalid> for Type {
  fn from(invalid: TypeInvalid) -> Self {
    Self::Invalid(invalid)
  }
}
