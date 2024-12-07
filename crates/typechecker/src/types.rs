use crate::{TypeError, similarity::similarly_named};
use bang_syntax::{AST, Span, ast::Type as Annotation};
use std::{
  collections::{BTreeMap, btree_map},
  fmt, iter, ops,
};

#[derive(Debug)]
pub struct TypeArena {
  types: Vec<Type>,
  type_vars: Vec<TypeVar>,
}
impl TypeArena {
  pub const BOOLEAN: TypeRef = TypeRef(0);
  pub const NUMBER: TypeRef = TypeRef(1);
  pub const STRING: TypeRef = TypeRef(2);
  pub const NEVER: TypeRef = TypeRef(3);
  pub const UNKNOWN: TypeRef = TypeRef(4);

  pub fn new() -> Self {
    Self {
      types: vec![
        Type::Primitive(PrimitiveType::Boolean),
        Type::Primitive(PrimitiveType::Number),
        Type::Primitive(PrimitiveType::String),
        Type::Primitive(PrimitiveType::Never),
        Type::Primitive(PrimitiveType::Unknown),
      ],
      type_vars: Vec::new(),
    }
  }

  pub fn new_type(&mut self, ty: Type) -> TypeRef {
    let ref_ = TypeRef(u32::try_from(self.types.len()).unwrap());
    self.types.push(ty);
    ref_
  }

  pub fn new_type_var(&mut self) -> TypeRef {
    let ref_ = TypeVarRef(u32::try_from(self.type_vars.len()).unwrap());
    self.type_vars.push(TypeVar {
      link: TypeVarLink::NoLink,
    });
    self.new_type(Type::Variable(ref_))
  }

  pub fn get_type_var(&self, type_var_ref: TypeVarRef) -> &TypeVar {
    &self.type_vars[usize::try_from(type_var_ref.0).unwrap()]
  }
  pub fn get_type_var_mut(&mut self, type_var_ref: TypeVarRef) -> &mut TypeVar {
    &mut self.type_vars[usize::try_from(type_var_ref.0).unwrap()]
  }

  /// Unifies types `a` and `b`, returning `Ok` if they are unifiable
  pub fn unify(&mut self, a: TypeRef, b: TypeRef) -> Result<(), ()> {
    self.unify_inner(a, b, false)
  }

  /// Unifies types `a` and `b`, returning `Ok` if they are unifiable
  /// `a` must always match `b`, for example a type variable can't be unified with a concrete type.
  pub fn unify_strict(&mut self, a: TypeRef, b: TypeRef) -> Result<(), ()> {
    self.unify_inner(a, b, true)
  }

  fn unify_inner(&mut self, a: TypeRef, b: TypeRef, strict: bool) -> Result<(), ()> {
    let a = self.normalize(a);
    let b = self.normalize(b);

    match (self[a], self[b]) {
      (Type::Primitive(a), Type::Primitive(b)) if a == b => {}

      (Type::Function(a_param, a_return), Type::Function(b_param, b_return)) => {
        self.unify_inner(a_param, b_param, strict)?;
        self.unify_inner(a_return, b_return, strict)?;
      }

      (Type::Structure(structure_a, a_param), Type::Structure(structure_b, b_param)) => {
        if structure_a != structure_b {
          return Err(());
        }
        self.unify_inner(a_param, b_param, strict)?;
      }

      (Type::Variable(var1), Type::Variable(var2)) => {
        if var1 == var2 {
          return Ok(());
        }

        self.link(var1, b);
      }
      (Type::Variable(var), _) if !strict => self.link(var, b),
      (_, Type::Variable(var)) => self.link(var, a),

      (_, _) => return Err(()),
    };

    Ok(())
  }

  fn normalize(&mut self, type_ref: TypeRef) -> TypeRef {
    if let Type::Variable(type_var_ref) = self[type_ref] {
      match self.get_type_var(type_var_ref).link {
        TypeVarLink::Link(type_ref) => {
          let ty_normalized = self.normalize(type_ref);
          self.get_type_var_mut(type_var_ref).link = TypeVarLink::Link(ty_normalized);
          ty_normalized
        }
        TypeVarLink::NoLink => type_ref,
      }
    } else {
      type_ref
    }
  }

  /// Sets `var` to have type `ty`
  pub(crate) fn link(&mut self, var: TypeVarRef, ty: TypeRef) {
    self.get_type_var_mut(var).link = TypeVarLink::Link(ty);
  }

  fn has_quantified_vars(&self, ty: TypeRef) -> bool {
    match self[ty] {
      Type::Primitive(_) | Type::Variable(_) => false,
      Type::Function(parameter, return_type) => {
        self.has_quantified_vars(parameter) || self.has_quantified_vars(return_type)
      }
      Type::Structure(_, parameter) => self.has_quantified_vars(parameter),
      Type::Quantified(_) => true,
    }
  }

  /// Replace all the Quantified types with new type variables
  pub fn instantiate(&mut self, scheme: TypeScheme) -> TypeRef {
    if scheme.number_quantified_vars == 0 {
      // No quantified variables, so just return the type.
      return scheme.type_;
    }

    // Create a new type var for each quantified variable
    let substitutes: Vec<TypeRef> = (0..=(scheme.number_quantified_vars))
      .map(|_| self.new_type_var())
      .collect();

    self.replace_quantified_vars(scheme.type_, &substitutes)
  }

  fn replace_quantified_vars(&mut self, type_: TypeRef, subs: &[TypeRef]) -> TypeRef {
    if !self.has_quantified_vars(type_) {
      return type_;
    }

    match self[type_] {
      Type::Function(parameter, return_type) => {
        let parameter = self.replace_quantified_vars(parameter, subs);
        let return_type = self.replace_quantified_vars(return_type, subs);
        self.new_type(Type::Function(parameter, return_type))
      }
      Type::Structure(kind, parameter) => {
        let parameter = self.replace_quantified_vars(parameter, subs);
        self.new_type(Type::Structure(kind, parameter))
      }
      Type::Quantified(idx) => subs[usize::try_from(idx).unwrap()],
      Type::Primitive(_) | Type::Variable(_) => {
        unreachable!("doesn't contain quantified var");
      }
    }
  }

  pub fn generalize(&mut self, type_: TypeRef) -> TypeScheme {
    let mut generalized_type_vars = BTreeMap::new();
    let mut type_var_id = 0;

    for type_var in self.type_vars_in(type_) {
      if let btree_map::Entry::Vacant(e) = generalized_type_vars.entry(type_var) {
        e.insert(type_var_id);
        type_var_id += 1;
      }
    }

    TypeScheme {
      number_quantified_vars: u32::try_from(generalized_type_vars.len()).unwrap(),
      type_: self.replace_type_vars(type_, &generalized_type_vars),
    }
  }

  fn type_vars_in(&mut self, ty: TypeRef) -> Box<dyn Iterator<Item = TypeVarRef>> {
    let norm_ty = self.normalize(ty);
    match self[norm_ty] {
      Type::Primitive(_) => Box::new(iter::empty()),
      Type::Variable(var) => Box::new(iter::once(var)),
      Type::Function(parameter, return_type) => Box::new(
        self
          .type_vars_in(parameter)
          .chain(self.type_vars_in(return_type)),
      ),
      Type::Structure(_, parameter) => Box::new(self.type_vars_in(parameter)),
      Type::Quantified(_) => unreachable!("type in inference has no quantified vars"),
    }
  }

  fn replace_type_vars(&mut self, ty: TypeRef, subs: &BTreeMap<TypeVarRef, usize>) -> TypeRef {
    let ty = self.normalize(ty);

    match self[ty] {
      Type::Primitive(_) => ty,

      Type::Function(parameter, return_type) => {
        let parameter = self.replace_type_vars(parameter, subs);
        let return_type = self.replace_type_vars(return_type, subs);
        self.new_type(Type::Function(parameter, return_type))
      }

      Type::Variable(var) => match subs.get(&var) {
        Some(idx) => self.new_type(Type::Quantified((*idx).try_into().unwrap())),
        None => ty,
      },

      Type::Structure(kind, parameter) => {
        let parameter = self.replace_type_vars(parameter, subs);
        self.new_type(Type::Structure(kind, parameter))
      }

      Type::Quantified(_) => unreachable!("type in inference has no quantified vars"),
    }
  }

  pub fn type_to_string(&self, type_ref: TypeRef) -> String {
    let type_ = self[type_ref];

    match type_ {
      Type::Primitive(primitive) => match primitive {
        PrimitiveType::Boolean => "boolean".to_owned(),
        PrimitiveType::Number => "number".to_owned(),
        PrimitiveType::String => "string".to_owned(),
        PrimitiveType::Never => "never".to_owned(),
        PrimitiveType::Unknown => "unknown".to_owned(),
      },
      Type::Function(argument, return_type) => {
        let mut argument_ = self.type_to_string(argument);
        let return_ = self.type_to_string(return_type);

        if argument_ == "never" {
          "_".clone_into(&mut argument_);
        }

        if matches!(self[argument], Type::Function(_, _)) {
          format!("({argument_}) => {return_}")
        } else {
          format!("{argument_} => {return_}")
        }
      }
      Type::Quantified(idx) => format!("^{}", integer_to_identifier(idx)),
      Type::Structure(kind, parameter) => format!("{kind}<{}>", self.type_to_string(parameter)),
      Type::Variable(type_var_ref) => {
        let type_var = self.get_type_var(type_var_ref);
        match type_var.link {
          TypeVarLink::Link(type_ref) => self.type_to_string(type_ref),
          TypeVarLink::NoLink => format!("^{}", integer_to_identifier(type_var_ref.0)),
        }
      }
    }
  }

  /// Returns the parameter of a function type, or None if the type is not a function
  pub fn function_parameter(&self, type_ref: TypeRef) -> Option<TypeRef> {
    if let Type::Function(parameter, _) = self[type_ref] {
      Some(parameter)
    } else {
      None
    }
  }
  /// Returns the return type of a function type, or None if the type is not a function
  pub fn function_return(&self, type_ref: TypeRef) -> Option<TypeRef> {
    if let Type::Function(_, return_type) = self[type_ref] {
      Some(return_type)
    } else {
      None
    }
  }

  /// Is the type ref point to the never type?
  pub fn is_never(&self, type_ref: TypeRef) -> bool {
    self[type_ref] == Type::Primitive(PrimitiveType::Never)
  }

  pub(crate) fn type_from_annotation(
    &mut self,
    ty: &Annotation,
    ast: &AST,
  ) -> Result<TypeRef, TypeError> {
    let mut variables = BTreeMap::new();
    self.type_from_annotation_inner(ty, ast, &mut variables)
  }

  const PRIMITIVE_NAMES: [&str; 4] = ["number", "string", "boolean", "_"];
  const STRUCTURE_NAMES: [&str; 2] = ["list", "option"];

  fn type_from_annotation_inner<'a>(
    &mut self,
    ty: &Annotation,
    ast: &'a AST,
    variables: &mut BTreeMap<&'a str, TypeRef>,
  ) -> Result<TypeRef, TypeError> {
    match ty {
      Annotation::Primitive(primitive) => {
        self.primitive_type_from_annotation(primitive.name(ast), primitive.span(ast))
      }
      Annotation::Variable(variable) => {
        let type_ = variables.get(variable.name(ast));
        if let Some(type_) = type_ {
          Ok(*type_)
        } else {
          let type_var = self.new_type_var();
          variables.insert(variable.name(ast), type_var);
          Ok(type_var)
        }
      }
      Annotation::Function(function) => {
        let parameter = self.type_from_annotation_inner(function.parameter(ast), ast, variables)?;
        let return_ = self.type_from_annotation_inner(function.return_(ast), ast, variables)?;
        Ok(self.new_type(Type::Function(parameter, return_)))
      }
      Annotation::Group(group) => self.type_from_annotation_inner(group.type_(ast), ast, variables),
      Annotation::Structure(parameter) => match parameter.structure(ast) {
        "list" => {
          let param = self.type_from_annotation_inner(parameter.parameter(ast), ast, variables)?;
          Ok(self.new_type(Type::Structure(Structure::List, param)))
        }
        "option" => {
          let param = self.type_from_annotation_inner(parameter.parameter(ast), ast, variables)?;
          Ok(self.new_type(Type::Structure(Structure::Option, param)))
        }
        "iterator" => {
          let param = self.type_from_annotation_inner(parameter.parameter(ast), ast, variables)?;
          Ok(self.new_type(Type::Structure(Structure::Iterator, param)))
        }
        name if Self::PRIMITIVE_NAMES.contains(&name) => Err(TypeError::UnexpectedParameter {
          type_: name.to_owned(),
          span: parameter.span(ast),
        }),
        name => Err(TypeError::UnknownTypeAnnotation {
          span: parameter.span(ast),
          did_you_mean: similarly_named(name, Self::STRUCTURE_NAMES),
        }),
      },
      Annotation::Invalid(_) => Ok(TypeArena::UNKNOWN),
    }
  }

  fn primitive_type_from_annotation(
    &mut self,
    primitive: &str,
    span: Span,
  ) -> Result<TypeRef, TypeError> {
    match primitive {
      "number" => Ok(TypeArena::NUMBER),
      "string" => Ok(TypeArena::STRING),
      "boolean" => Ok(TypeArena::BOOLEAN),
      "_" => Ok(TypeArena::NEVER),
      "list" => {
        // If it doesn't have a parameter, make it generic
        let type_ = self.new_type_var();
        Ok(self.new_type(Type::Structure(Structure::List, type_)))
      }
      "option" => {
        // If it doesn't have a parameter, make it generic
        let type_ = self.new_type_var();
        Ok(self.new_type(Type::Structure(Structure::Option, type_)))
      }
      "iterator" => {
        // If it doesn't have a parameter, make it generic
        let type_ = self.new_type_var();
        Ok(self.new_type(Type::Structure(Structure::Iterator, type_)))
      }
      _ => Err(TypeError::UnknownTypeAnnotation {
        span,
        did_you_mean: similarly_named(primitive, Self::PRIMITIVE_NAMES),
      }),
    }
  }
}
impl ops::Index<TypeRef> for TypeArena {
  type Output = Type;

  fn index(&self, index: TypeRef) -> &Self::Output {
    if let Type::Variable(type_var_ref) = self.types[usize::try_from(index.0).unwrap()]
      && let TypeVarLink::Link(type_ref) = self.get_type_var(type_var_ref).link
    {
      return &self[type_ref];
    }

    &self.types[usize::try_from(index.0).unwrap()]
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
  /// A primitive value, e.g. booleans, number, strings
  Primitive(PrimitiveType),
  /// Function type, e.g. `(number -> number)`
  Function(TypeRef, TypeRef),
  /// A type variable, e.g. `'a`
  Variable(TypeVarRef),
  /// A structures type e.g. `list<number>`
  Structure(Structure, TypeRef),
  /// A quantified type variable
  /// This is only used by types stored in the enviroment. It is replaced with new
  /// type variables when the type is used.
  Quantified(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
  Boolean,
  Number,
  String,
  Never,
  Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeVar {
  link: TypeVarLink,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeVarLink {
  Link(TypeRef),
  NoLink,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Structure {
  List,
  Option,
  Iterator,
}
impl fmt::Display for Structure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Structure::List => write!(f, "list"),
      Structure::Option => write!(f, "option"),
      Structure::Iterator => write!(f, "iterator"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVarRef(u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeScheme {
  number_quantified_vars: u32,
  type_: TypeRef,
}
impl TypeScheme {
  pub fn new(type_: TypeRef, number_quantified_vars: u32) -> Self {
    TypeScheme {
      number_quantified_vars,
      type_,
    }
  }

  pub fn monomorphic(type_: TypeRef) -> Self {
    TypeScheme {
      number_quantified_vars: 0,
      type_,
    }
  }

  pub(crate) fn raw_type(self) -> TypeRef {
    self.type_
  }
}

/// Converts an integer to a string in the form `a` or `ab`
///
/// Only works for integers less than 26^2 (676)
fn integer_to_identifier(integer: u32) -> String {
  fn to_char(integer: u32) -> char {
    char::from_u32(u32::from('a') + integer).unwrap()
  }

  if integer > 26 {
    format!("{}{}", to_char(integer / 26), to_char(integer % 26))
  } else {
    to_char(integer).to_string()
  }
}
