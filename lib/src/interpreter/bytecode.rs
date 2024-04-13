use crate::{ast::Span, collections::String};
use std::{fmt, mem, ptr};

/// A chunk of bytecode
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Chunk {
  name: String,
  code: Vec<u8>,
  pub(crate) constants: Vec<ConstantValue>,
  strings: Vec<String>,
  debug_info: DebugInfo,
}
impl Chunk {
  pub(crate) fn new(name: String) -> Self {
    Self {
      name,
      code: Vec::with_capacity(512),
      constants: Vec::with_capacity(32),
      strings: Vec::with_capacity(16),
      debug_info: DebugInfo::new(),
    }
  }

  pub(crate) fn finalize(&mut self) {
    self.debug_info.finalize();
  }
  pub(crate) fn len(&self) -> usize {
    self.code.len()
  }

  pub(crate) fn add_opcode(&mut self, opcode: OpCode, span: Span) {
    self.code.push(opcode as u8);
    self.debug_info.add(span);
  }
  pub(crate) fn add_value(&mut self, value: u8, span: Span) {
    self.code.push(value);
    self.debug_info.add(span);
  }
  pub(crate) fn add_long_value(&mut self, value: u16, span: Span) {
    let [a, b] = u16::to_le_bytes(value);
    self.code.push(a);
    self.code.push(b);
    self.debug_info.add(span);
    self.debug_info.add(span);
  }
  pub(crate) fn add_number(&mut self, value: f64, span: Span) {
    self.add_opcode(OpCode::Number, span);
    self.code.extend_from_slice(&value.to_le_bytes());

    for _ in 0..8 {
      self.debug_info.add(span);
    }
  }
  pub(crate) fn add_constant(&mut self, value: ConstantValue) -> usize {
    if let Some(constant_position) = self.constants.iter().position(|x| x == &value) {
      constant_position
    } else {
      let constant_position = self.constants.len();
      self.constants.push(value);
      constant_position
    }
  }
  pub(crate) fn add_string(&mut self, string: &str) -> usize {
    let string_position = self.strings.len();
    self.strings.push(string.into());
    string_position
  }
  pub(crate) fn replace_long_value(&mut self, position: usize, value: u16) {
    let [a, b] = u16::to_le_bytes(value);
    self.code[position] = a;
    self.code[position + 1] = b;
  }

  #[inline]
  pub(crate) fn get(&self, position: usize) -> OpCode {
    // SAFETY: Assume bytecode is valid, so bytecode at the position exists
    debug_assert!(position < self.code.len());

    OpCode::from(*unsafe { self.code.get_unchecked(position) })
  }
  #[inline]
  pub(crate) fn get_value(&self, position: usize) -> u8 {
    // SAFETY: Assume bytecode is valid, so position exists
    debug_assert!(position < self.code.len());

    unsafe { *self.code.get_unchecked(position) }
  }
  #[inline]
  pub(crate) fn get_long_value(&self, position: usize) -> u16 {
    u16::from_le_bytes([self.get_value(position), self.get_value(position + 1)])
  }
  #[inline]
  pub(crate) fn get_number(&self, position: usize) -> f64 {
    f64::from_le_bytes([
      self.get_value(position),
      self.get_value(position + 1),
      self.get_value(position + 2),
      self.get_value(position + 3),
      self.get_value(position + 4),
      self.get_value(position + 5),
      self.get_value(position + 6),
      self.get_value(position + 7),
    ])
  }
  #[inline]
  pub(crate) fn get_constant(&self, pointer: usize) -> &ConstantValue {
    // SAFETY: Assume bytecode is valid, so constant exists at pointer
    debug_assert!(pointer < self.constants.len());

    unsafe { self.constants.get_unchecked(pointer) }
  }
  #[inline]
  pub(crate) fn get_string(&self, pointer: usize) -> &String {
    // SAFETY: Assume bytecode is valid, so constant exists at pointer
    debug_assert!(pointer < self.strings.len());

    unsafe { self.strings.get_unchecked(pointer) }
  }

  pub(crate) fn get_span(&self, opcode_position: usize) -> Span {
    self.debug_info.get(opcode_position)
  }

  pub(crate) fn get_pointer(&self) -> *const Self {
    ptr::from_ref::<Self>(self)
  }

  pub(crate) fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.name.is_empty() {
      f.write_str("<function>")
    } else {
      write!(f, "<function {}>", self.name)
    }
  }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpCode {
  // Constants
  Constant,
  ConstantLong,
  Number,
  True,
  False,
  Null,

  // Numeric Operations
  Add,
  Subtract,
  Multiply,
  Divide,
  Remainder,
  Negate,
  AddString,

  // Equalities
  Not,
  Equals,
  NotEquals,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  // Variables
  DefineGlobal,
  GetGlobal,
  GetLocal,

  // Functions
  Call,
  Return,

  // Closures
  Allocate,
  Closure,
  GetUpvalue,
  GetAllocatedValue,
  GetAllocatedPointer,

  // VM Operations
  Pop,
  PopBelow,
  Peek,
  Jump,
  JumpIfFalse,
  Halt,
}
impl OpCode {
  #[inline]
  pub fn length(self) -> usize {
    match self {
      OpCode::Number => 9,
      OpCode::ConstantLong | OpCode::Jump | OpCode::JumpIfFalse => 3,
      OpCode::Constant
      | OpCode::DefineGlobal
      | OpCode::GetGlobal
      | OpCode::GetLocal
      | OpCode::GetUpvalue
      | OpCode::PopBelow
      | OpCode::Allocate
      | OpCode::Closure
      | OpCode::GetAllocatedValue
      | OpCode::GetAllocatedPointer => 2,
      _ => 1,
    }
  }
}
impl From<u8> for OpCode {
  #[inline]
  fn from(value: u8) -> Self {
    // SAFETY: Assume bytecode is valid, so value is a valid OpCode
    debug_assert!(value <= OpCode::Halt as u8);

    unsafe { mem::transmute(value) }
  }
}

/// A constant value which is stored in a [Chunk]
#[derive(Clone)]
pub enum ConstantValue {
  String(crate::collections::String),
  Function(Chunk),
}
impl From<&str> for ConstantValue {
  fn from(value: &str) -> Self {
    Self::String(value.into())
  }
}
impl From<String> for ConstantValue {
  fn from(value: String) -> Self {
    Self::String(value)
  }
}
impl From<Chunk> for ConstantValue {
  fn from(value: Chunk) -> Self {
    Self::Function(value)
  }
}
impl PartialEq<ConstantValue> for ConstantValue {
  fn eq(&self, other: &ConstantValue) -> bool {
    match (self, other) {
      (Self::String(a), Self::String(b)) => a == b,
      _ => false,
    }
  }
}
impl fmt::Debug for ConstantValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::String(value) => write!(f, "'{value:?}'"),
      Self::Function(func) => func.display(f),
    }
  }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct DebugInfo {
  spans: Vec<(Span, u16)>,
  last: Span,
  repeated: u16,
}
impl DebugInfo {
  fn new() -> Self {
    Self {
      spans: Vec::new(),
      last: Span::default(),
      repeated: 0,
    }
  }

  fn add(&mut self, span: Span) {
    if span == Span::default() || span == self.last {
      self.repeated += 1;
    } else {
      self.spans.push((self.last, self.repeated));
      self.last = span;
      self.repeated = 1;
    }
  }

  fn finalize(&mut self) {
    if self.repeated > 0 {
      self.spans.push((self.last, self.repeated));
      self.last = Span::default();
      self.repeated = 0;
    }
  }

  fn get(&self, opcode_position: usize) -> Span {
    let length = self.spans.len();
    let mut count = 0;
    let mut line = 0;

    while line < length {
      let (_, repeated) = self.spans[line];
      count += repeated;

      if usize::from(count) > opcode_position {
        break;
      }

      line += 1;
    }

    self.spans[line].0
  }
}

impl fmt::Display for Chunk {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "      ╭─[Bytecode: {}]", self.name)?;

    let mut position: usize = 0;
    while position < self.len() {
      write!(f, " {position:0>4} │ ")?;
      match self.get(position) {
        OpCode::Constant => {
          let constant_location = self.get_value(position + 1);
          let constant = self.get_constant(constant_location.into());
          write!(f, "Constant {constant:?} ({constant_location})")
        }
        OpCode::ConstantLong => {
          let constant_location = self.get_long_value(position + 1);
          let constant = self.get_constant(constant_location.into());
          write!(f, "ConstantLong {constant:?} ({constant_location})")
        }
        OpCode::Number => {
          let number = self.get_number(position + 1);
          write!(f, "Number {number:?}")
        }
        OpCode::DefineGlobal => {
          let string_location = self.get_value(position + 1);
          let string = self.get_string(string_location.into());
          write!(f, "DefineGlobal '{string}' ({string_location})")
        }
        OpCode::GetGlobal => {
          let string_location = self.get_value(position + 1);
          let string = self.get_string(string_location.into());
          write!(f, "GetGlobal '{string}' ({string_location})")
        }
        OpCode::GetLocal => {
          let local_position = self.get_value(position + 1);
          write!(f, "GetLocal ({local_position})")
        }
        OpCode::PopBelow => {
          let count = self.get_value(position + 1);
          write!(f, "PopBelow ({count})")
        }
        OpCode::Jump => {
          let jump_by = self.get_long_value(position + 1);
          let jump_location = position + usize::from(jump_by) + 3;
          write!(f, "Jump {jump_by} ({jump_location:0>4})")
        }
        OpCode::JumpIfFalse => {
          let jump_by = self.get_long_value(position + 1);
          let jump_location = position + usize::from(jump_by) + 3;
          write!(f, "JumpIfFalse {jump_by} ({jump_location:0>4})")
        }
        OpCode::Allocate => {
          let local_position = self.get_value(position + 1);
          write!(f, "Allocate ({local_position})")
        }
        OpCode::GetUpvalue => {
          let upvalue_position = self.get_value(position + 1);
          write!(f, "GetUpvalue ({upvalue_position})")
        }
        OpCode::GetAllocatedValue => {
          let local_position = self.get_value(position + 1);
          write!(f, "GetAllocatedValue ({local_position})")
        }
        OpCode::GetAllocatedPointer => {
          let local_position = self.get_value(position + 1);
          write!(f, "GetAllocatedPointer ({local_position})")
        }

        code => write!(f, "{code:?}"),
      }?;
      writeln!(f)?;
      position += self.get(position).length();
    }

    writeln!(f, "──────╯")
  }
}
