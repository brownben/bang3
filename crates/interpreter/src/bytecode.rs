use bang_syntax::Span;
use rustc_hash::FxHashMap as HashMap;
use smartstring::alias::String as SmartString;
use std::{fmt, hash, mem, ptr};

/// A chunk of bytecode
#[derive(Clone, Debug, Default, PartialEq, Hash)]
#[must_use]
pub struct Chunk {
  pub(crate) name: SmartString,
  code: Vec<u8>,
  constants: Vec<ConstantValue>,
  symbols: Vec<SmartString>,
  debug_info: DebugInfo,
}
impl Chunk {
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
  pub(crate) fn get_symbol(&self, pointer: usize) -> &SmartString {
    // SAFETY: Assume bytecode is valid, so constant exists at pointer
    debug_assert!(pointer < self.symbols.len());

    unsafe { self.symbols.get_unchecked(pointer) }
  }

  pub(crate) fn get_span(&self, opcode_position: usize) -> Span {
    self.debug_info.get(opcode_position)
  }

  pub(crate) fn as_ptr(&self) -> *const Self {
    ptr::from_ref::<Self>(self)
  }

  pub(crate) fn display(&self) -> String {
    if self.name.is_empty() {
      "<function>".to_owned()
    } else {
      format!("<function {}>", self.name)
    }
  }
}

/// Create a bytecode chunk
///
/// SAFETY: Only bytecode generated by the compiler is guaranteed to be correct and not crash the VM
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
pub struct ChunkBuilder<'source> {
  pub(crate) name: &'source str,
  code: Vec<u8>,
  constants: Vec<ConstantValue>,
  constants_map: HashMap<ConstantValue, usize>,
  symbols: Vec<&'source str>,
  symbols_map: HashMap<&'source str, usize>,
  debug_info: DebugInfoBuilder,
}
impl<'source> ChunkBuilder<'source> {
  /// Create a new `ChunkBuilder`
  pub fn new(name: &'source str) -> Self {
    Self {
      name,
      code: Vec::with_capacity(256),
      constants: Vec::with_capacity(16),
      constants_map: HashMap::default(),
      symbols: Vec::with_capacity(16),
      symbols_map: HashMap::default(),
      debug_info: DebugInfoBuilder::new(),
    }
  }

  /// Create the bytecode `Chunk`
  pub fn finalize(self) -> Chunk {
    Chunk {
      name: self.name.into(),
      code: self.code,
      constants: self.constants,
      symbols: self.symbols.into_iter().map(SmartString::from).collect(),
      debug_info: self.debug_info.finalize(),
    }
  }

  pub(crate) fn len(&self) -> usize {
    self.code.len()
  }

  /// Add an opcode to the Chunk
  pub fn add_opcode(&mut self, opcode: OpCode, span: Span) {
    self.code.push(opcode as u8);
    self.debug_info.add(span);
  }
  /// Add an argument for an opcode
  pub fn add_value(&mut self, value: u8, span: Span) {
    self.code.push(value);
    self.debug_info.add(span);
  }
  /// Add a long argument for an opcode
  pub fn add_long_value(&mut self, value: u16, span: Span) {
    let [a, b] = u16::to_le_bytes(value);
    self.code.push(a);
    self.code.push(b);
    self.debug_info.add(span);
    self.debug_info.add(span);
  }
  /// Add a constant number
  pub fn add_number(&mut self, value: f64, span: Span) {
    self.add_opcode(OpCode::Number, span);
    self.code.extend_from_slice(&value.to_le_bytes());

    for _ in 0..8 {
      self.debug_info.add(span);
    }
  }
  /// Add a constant value
  pub fn add_constant(&mut self, value: impl Into<ConstantValue>) -> usize {
    let value = value.into();

    if let Some(constant_position) = self.constants_map.get(&value) {
      *constant_position
    } else {
      let constant_position = self.constants.len();
      self.constants.push(value.clone());
      self.constants_map.insert(value, constant_position);
      constant_position
    }
  }
  /// Add a symbol (global variable/ module name)
  pub fn add_symbol(&mut self, name: &'source str) -> usize {
    if let Some(symbol_position) = self.symbols_map.get(name) {
      *symbol_position
    } else {
      let symbol_position = self.symbols.len();
      self.symbols.push(name);
      self.symbols_map.insert(name, symbol_position);
      symbol_position
    }
  }
  /// Replace a long argument at the given `position`
  pub fn replace_long_value(&mut self, position: usize, value: u16) {
    let [a, b] = u16::to_le_bytes(value);
    self.code[position] = a;
    self.code[position + 1] = b;
  }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// An operation of the bytecode
pub enum OpCode {
  // Constants
  /// Load a constant. Arg(u8) - constant id
  Constant,
  /// Load a constant. Arg(u16) - constant id
  ConstantLong,
  /// Load a number. Arg(f64) - the number
  Number,
  /// Load a true
  True,
  /// Load a false
  False,
  /// Load a null
  Null,
  /// Import a value from the context. Arg(u8, u8) - symbol positions for module and item
  Import,

  // Numeric Operations
  /// Takes the top 2 elements of the stack and adds them together
  Add,
  /// Takes the top 2 elements of the stack and subtracts them together
  Subtract,
  /// Takes the top 2 elements of the stack and multiplies them together
  Multiply,
  /// Takes the top 2 elements of the stack and divides them
  Divide,
  /// Takes the top 2 elements of the stack and gets the remainder of them
  Remainder,
  /// Takes the top element and negates it
  Negate,

  // String Operations
  /// Takes the top 2 elements of the stack and concatenates them
  AddString,
  /// Takes the top element and converts it to a string
  ToString,

  // Equalities
  /// Takes the top element and gives true if it is falsy, else false
  Not,
  /// Takes the top 2 elements of the stack and compares them
  Equals,
  /// Takes the top 2 elements of the stack and compares them
  NotEquals,
  /// Takes the top 2 elements of the stack and compares them
  Greater,
  /// Takes the top 2 elements of the stack and compares them
  GreaterEqual,
  /// Takes the top 2 elements of the stack and compares them
  Less,
  /// Takes the top 2 elements of the stack and compares them
  LessEqual,

  // Variables
  /// Defines a global variable. Takes the top item of the stack. Arg(u8) - symbol position
  DefineGlobal,
  /// Gets a global variable. Arg(u8) - symbol position
  GetGlobal,
  /// Gets a local variable on the stack. Arg(u8) - stack offset
  GetLocal,

  // Functions
  /// Calls a function. Top item of stack is the argument, and the second is the function
  Call,
  /// Returns the top item of the stack as the result from the current function
  Return,

  // Closures
  /// Allocates the top value on the stack.
  Allocate,
  /// Creates a closure. Arg(u8) - number of upvalues
  Closure,
  /// Get an upvalue from the frame. Arg(u8) - upvalue id
  GetUpvalue,
  /// Gets the value of an allocated value on the stack. Arg(u8) - stack offset
  GetAllocatedValue,
  /// Gets the pointer to an allocated value from an upvalue. Arg(u8) - upvalue id
  GetAllocatedPointer,

  // VM Operations
  /// Removes the top element of the stack
  Pop,
  /// Removes elements below the top item of the stack. Arg(u8) - how many to remove
  PopBelow,
  /// Duplicate the top element of the stack
  Peek,
  /// Jump forwards. Arg(u16) - how far to jump forwards
  Jump,
  /// Jump forwards if top element of stack is falsy. Arg(u16) - how far to jump forwards
  JumpIfFalse,
  /// End execution
  Halt,
}
impl OpCode {
  /// How many bytes does this opcode and all its arguments take up?
  #[inline]
  #[must_use]
  pub fn length(self) -> usize {
    match self {
      OpCode::Number => 9,
      OpCode::ConstantLong | OpCode::Jump | OpCode::JumpIfFalse | OpCode::Import => 3,
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
  String(SmartString),
  Function(Chunk),
}
impl From<&str> for ConstantValue {
  fn from(value: &str) -> Self {
    Self::String(value.into())
  }
}
impl From<SmartString> for ConstantValue {
  fn from(value: SmartString) -> Self {
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
impl Eq for ConstantValue {}
impl hash::Hash for ConstantValue {
  fn hash<H: hash::Hasher>(&self, state: &mut H) {
    mem::discriminant(self).hash(state);
    match self {
      Self::String(value) => value.hash(state),
      Self::Function(func) => func.as_ptr().hash(state),
    }
  }
}
impl fmt::Debug for ConstantValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::String(value) => write!(f, "'{value:?}'"),
      Self::Function(func) => f.write_str(&func.display()),
    }
  }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct DebugInfoBuilder {
  spans: Vec<(Span, u16)>,
  last: Span,
  repeated: u16,
}
impl DebugInfoBuilder {
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

  fn finalize(mut self) -> DebugInfo {
    if self.repeated > 0 {
      self.spans.push((self.last, self.repeated));
      self.last = Span::default();
      self.repeated = 0;
    }

    DebugInfo { spans: self.spans }
  }
}

#[derive(Clone, Debug, Default, PartialEq, Hash)]
struct DebugInfo {
  spans: Vec<(Span, u16)>,
}
impl DebugInfo {
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
    while position < self.code.len() {
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
        OpCode::Import => {
          let module_name = self.get_symbol(self.get_value(position + 1).into());
          let item_name = self.get_symbol(self.get_value(position + 2).into());

          write!(f, "Import {module_name}::{item_name}")
        }
        OpCode::DefineGlobal => {
          let name_location = self.get_value(position + 1);
          let name = self.get_symbol(name_location.into());
          write!(f, "DefineGlobal '{name}' ({name_location})")
        }
        OpCode::GetGlobal => {
          let name_location = self.get_value(position + 1);
          let name = self.get_symbol(name_location.into());
          write!(f, "GetGlobal '{name}' ({name_location})")
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
