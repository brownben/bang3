use super::Value;
use std::{fmt, mem};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpCode {
  // Constants
  Constant,
  ConstantLong,
  True,
  False,

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
  Closure,
  GetUpvalue,
  GetAllocated,

  // VM Operations
  Pop,
  PopBelow,
  Jump,
  JumpIfFalse,
  Halt,
}
impl OpCode {
  #[inline]
  pub fn length(self) -> usize {
    match self {
      OpCode::ConstantLong | OpCode::Jump | OpCode::JumpIfFalse => 3,
      OpCode::Constant
      | OpCode::DefineGlobal
      | OpCode::GetGlobal
      | OpCode::GetLocal
      | OpCode::GetUpvalue
      | OpCode::GetAllocated
      | OpCode::PopBelow => 2,
      _ => 1,
    }
  }
}
impl From<u8> for OpCode {
  #[inline]
  fn from(value: u8) -> Self {
    // SAFETY: Assume bytecode is valid, so value is a valid OpCode
    unsafe { mem::transmute(value) }
  }
}

/// A chunk of bytecode
#[derive(Debug, Default)]
pub struct Chunk {
  code: Vec<u8>,
  constants: Vec<Value>,
  strings: Vec<String>,
}
impl Chunk {
  pub(crate) fn new() -> Self {
    Self {
      code: Vec::with_capacity(512),
      constants: Vec::with_capacity(32),
      strings: Vec::with_capacity(16),
    }
  }
  pub(crate) fn len(&self) -> usize {
    self.code.len()
  }

  pub(crate) fn add_opcode(&mut self, opcode: OpCode) {
    self.code.push(opcode as u8);
  }
  pub(crate) fn add_value(&mut self, value: u8) {
    self.code.push(value);
  }
  pub(crate) fn add_long_value(&mut self, value: u16) {
    let [a, b] = u16::to_le_bytes(value);
    self.code.push(a);
    self.code.push(b);
  }
  pub(crate) fn add_constant(&mut self, value: Value) -> usize {
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
    // SAFETY: Assume bytecode is valid, so position exists and OpCode is valid
    OpCode::from(*unsafe { self.code.get_unchecked(position) })
  }
  #[inline]
  pub(crate) fn get_value(&self, position: usize) -> u8 {
    // SAFETY: Assume bytecode is valid, so position exists
    unsafe { *self.code.get_unchecked(position) }
  }
  #[inline]
  pub(crate) fn get_long_value(&self, position: usize) -> u16 {
    u16::from_le_bytes([self.get_value(position), self.get_value(position + 1)])
  }
  #[inline]
  pub(crate) fn get_constant(&self, pointer: usize) -> Value {
    // SAFETY: Assume bytecode is valid, so constant exists at pointer
    unsafe { self.constants.get_unchecked(pointer) }.clone()
  }
  #[inline]
  pub(crate) fn get_string(&self, pointer: usize) -> &String {
    // SAFETY: Assume bytecode is valid, so constant exists at pointer
    unsafe { self.strings.get_unchecked(pointer) }
  }
}

impl fmt::Display for Chunk {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "      ╭─[Bytecode]")?;

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
        OpCode::GetUpvalue => {
          let upvalue_position = self.get_value(position + 1);

          write!(f, "GetUpvalue ({upvalue_position})")
        }
        OpCode::GetAllocated => {
          let local_position = self.get_value(position + 1);

          write!(f, "GetAllocated ({local_position})")
        }

        code => write!(f, "{code:?}"),
      }?;
      writeln!(f)?;
      position += self.get(position).length();
    }

    writeln!(f, "──────╯")
  }
}
