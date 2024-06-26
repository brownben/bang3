use super::{
  bytecode::{Chunk, ConstantValue, OpCode},
  value::{Closure, Object, Value},
};
use crate::{
  ast::{GetSpan, Span},
  collections::{HashMap, SmallVec, String},
};
use std::{error, fmt, ptr};

#[derive(Debug)]
struct CallFrame {
  ip: usize,
  offset: usize,
  chunk: *const Chunk,
  upvalues: SmallVec<[Value; 4]>,
}

/// A virtual machine to execute compiled bytecode
#[derive(Debug)]
pub struct VM {
  stack: Vec<Value>,
  globals: HashMap<String, Value>,
  frames: Vec<CallFrame>,
}
impl VM {
  /// Create a new VM
  #[must_use]
  pub fn new() -> Self {
    Self {
      stack: Vec::with_capacity(512),
      globals: HashMap::default(),
      frames: Vec::with_capacity(16),
    }
  }

  /// Get the value of a global variable
  #[must_use]
  pub fn get_global(&self, name: &str) -> Option<&Value> {
    self.globals.get(name)
  }
  /// Set the value of a global variable
  pub fn set_global(&mut self, name: impl Into<String>, value: impl Into<Value>) {
    self.globals.insert(name.into(), value.into());
  }

  #[inline]
  fn pop(&mut self) -> Value {
    // SAFETY: Assume bytecode is valid, so stack is not empty
    debug_assert!(!self.stack.is_empty());

    unsafe { self.stack.pop().unwrap_unchecked() }
  }
  #[inline]
  fn peek(&self) -> &Value {
    // SAFETY: Assume bytecode is valid, so stack is not empty
    debug_assert!(!self.stack.is_empty());

    unsafe { self.stack.last().unwrap_unchecked() }
  }
  #[inline]
  fn push(&mut self, value: Value) {
    self.stack.push(value);
  }
  #[inline]
  fn get_stack_value(&self, position: usize) -> Value {
    // SAFETY: Assume bytecode is valid, so position exists
    debug_assert!(position < self.stack.len());

    unsafe { self.stack.get_unchecked(position) }.clone()
  }

  #[inline]
  fn peek_frame(&self) -> &CallFrame {
    // SAFETY: Assume bytecode is valid, so frames is not empty
    debug_assert!(!self.frames.is_empty());

    unsafe { self.frames.last().unwrap_unchecked() }
  }
  #[inline]
  fn push_frame(
    &mut self,
    ip: usize,
    offset: usize,
    chunk: *const Chunk,
    upvalues: SmallVec<[Value; 4]>,
  ) {
    self.frames.push(CallFrame {
      ip,
      offset,
      chunk,
      upvalues,
    });
  }
  #[inline]
  fn pop_frame(&mut self) -> CallFrame {
    // SAFETY: Assume bytecode is valid, so frames is not empty
    debug_assert!(!self.frames.is_empty());

    unsafe { self.frames.pop().unwrap_unchecked() }
  }

  /// Run a chunk of bytecode
  ///
  /// # Errors
  /// Returns an error if a runtime error is encountered in the bytecode being executed
  #[allow(clippy::too_many_lines)]
  pub fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut ip = 0;
    let mut offset = 0;
    let mut chunk = chunk;

    let error = loop {
      let instruction = chunk.get(ip);

      match instruction {
        // Constants
        OpCode::Constant => {
          let constant_position = chunk.get_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          self.push(match constant {
            ConstantValue::String(string) => Value::from(ptr::from_ref(string)),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          });
        }
        OpCode::ConstantLong => {
          let constant_position = chunk.get_long_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          self.push(match constant {
            ConstantValue::String(string) => Value::from(ptr::from_ref(string)),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          });
        }
        OpCode::Number => self.push(chunk.get_number(ip + 1).into()),
        OpCode::True => self.push(Value::TRUE),
        OpCode::False => self.push(Value::FALSE),
        OpCode::Null => self.push(Value::NULL),

        // Numeric Operations
        OpCode::Add => numeric_operation!((self, chunk), +),
        OpCode::Subtract => numeric_operation!((self, chunk), -),
        OpCode::Multiply => numeric_operation!((self, chunk), *),
        OpCode::Divide => numeric_operation!((self, chunk), /),
        OpCode::Remainder => numeric_operation!((self, chunk), %),
        OpCode::AddString => {
          let right = self.pop();
          let left = self.pop();

          if left.is_string() && right.is_string() {
            let mut new = left.as_string().clone();
            new.push_str(right.as_string());
            self.push(new.into());
          } else {
            break Some(ErrorKind::TypeErrorBinary {
              expected: "two strings",
              got: format!("a {} and a {}", left.get_type(), right.get_type()),
            });
          }
        }

        // Unary Operations
        OpCode::Negate => {
          let value = self.pop();
          if value.is_number() {
            self.push(Value::from(-value.as_number()));
          } else {
            break Some(ErrorKind::TypeError {
              expected: "number",
              got: value.get_type(),
            });
          }
        }
        OpCode::Not => {
          let value = self.pop();
          self.push(value.is_falsy().into());
        }

        // Equalities
        OpCode::Equals => {
          let (right, left) = (self.pop(), self.pop());
          self.push((left == right).into());
        }
        OpCode::NotEquals => {
          let (right, left) = (self.pop(), self.pop());
          self.push((left != right).into());
        }

        // Comparisons
        OpCode::Greater => comparison_operation!((self, chunk), >),
        OpCode::GreaterEqual => comparison_operation!((self, chunk), >=),
        OpCode::Less => comparison_operation!((self, chunk), <),
        OpCode::LessEqual => comparison_operation!((self, chunk), <=),

        // Variables
        OpCode::DefineGlobal => {
          let string_position = chunk.get_value(ip + 1);
          let string = chunk.get_string(string_position.into()).clone();
          let value = self.pop();

          self.globals.insert(string, value);
        }
        OpCode::GetGlobal => {
          let string_position = chunk.get_value(ip + 1);
          let string = chunk.get_string(string_position.into());

          match self.globals.get(string) {
            Some(value) => self.push(value.clone()),
            None => break Some(ErrorKind::UndefinedVariable(string.to_string())),
          }
        }
        OpCode::GetLocal => {
          let slot = chunk.get_value(ip + 1);
          let value = self.get_stack_value(offset + usize::from(slot));
          self.push(value);
        }

        // Functions
        OpCode::Call => {
          let callee = self.get_stack_value(self.stack.len() - 2);

          if callee.is_function() {
            self.push_frame(ip, offset, chunk, SmallVec::new());

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*callee.as_function().get_pointer() };
          } else if callee.is_closure() {
            let closure = callee.as_closure();
            self.push_frame(ip, offset, chunk, closure.upvalues.clone());

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*closure.function().get_pointer() };
          } else if callee.is_fast_native_function() {
            let function = callee.as_fast_native_function();
            let argument = self.pop();

            let result = (function.func)(argument);
            self.push(result);

            ip += 1;
          } else {
            break Some(ErrorKind::NotCallable(callee.get_type()));
          }

          continue; // skip the ip increment, as we're jumping to a new chunk
        }
        OpCode::Return => {
          let result = self.pop();
          self.stack.drain(offset - 1..);
          self.push(result);

          let frame = self.pop_frame();
          ip = frame.ip;
          offset = frame.offset;
          // SAFETY: callee stays on stack, so call stack reference will always exist
          chunk = unsafe { &*frame.chunk };
        }

        // Closures
        OpCode::Allocate => {
          let index = chunk.get_value(ip + 1);
          let local = &mut self.stack[offset + usize::from(index)];
          let allocated = local.clone().allocate();
          *local = allocated.clone();
          self.push(allocated);
        }
        OpCode::Closure => {
          let upvalue_count = chunk.get_value(ip + 1);
          let upvalues = self
            .stack
            .drain((self.stack.len() - usize::from(upvalue_count))..)
            .collect();

          let value = self.pop();
          if value.is_function() {
            self.push(Closure::new(ptr::from_ref(value.as_function()), upvalues).into());
          } else {
            break Some(ErrorKind::NonFunctionClosure);
          }
        }
        OpCode::GetUpvalue => {
          let upvalue = chunk.get_value(ip + 1);
          let address = &self.peek_frame().upvalues[usize::from(upvalue)];
          let value = address.as_allocated().borrow().clone();

          self.push(value);
        }
        OpCode::GetAllocatedValue => {
          let slot = chunk.get_value(ip + 1);
          let address = &self.stack[offset + usize::from(slot)];
          let value = address.as_allocated().borrow().clone();

          self.push(value);
        }
        OpCode::GetAllocatedPointer => {
          let index = chunk.get_value(ip + 1);
          let allocated = self.peek_frame().upvalues[usize::from(index)].clone();

          self.push(allocated);
        }

        // VM Operations
        OpCode::Pop => {
          self.pop();
        }
        OpCode::PopBelow => {
          let count = chunk.get_value(ip + 1);
          let value = self.pop();
          self.stack.truncate(self.stack.len() - usize::from(count));
          self.push(value);
        }
        OpCode::Peek => {
          let value = self.peek().clone();
          self.push(value);
        }
        OpCode::Jump => {
          let jump = chunk.get_long_value(ip + 1);
          ip += usize::from(jump);
        }
        OpCode::JumpIfFalse => {
          let jump = chunk.get_long_value(ip + 1);
          if self.peek().is_falsy() {
            ip += usize::from(jump);
          }
        }
        OpCode::Halt => break None,
      };

      ip += instruction.length();
    };

    // move constants from the chunk to the heap
    for value in self.globals.values_mut() {
      if value.is_constant_string() {
        *value = Value::from(Object::from(value.as_string().clone()));
      } else if value.is_constant_function() {
        *value = Value::from(Object::from(value.as_function().clone()));
      }
    }

    if let Some(error) = error {
      Err(RuntimeError {
        kind: error,
        span: chunk.get_span(ip),
      })
    } else {
      Ok(())
    }
  }
}
impl Default for VM {
  fn default() -> Self {
    Self::new()
  }
}

macro numeric_operation(($vm:expr, $chunk:expr), $operator:tt) {{
  let right = $vm.pop();
  let left = $vm.pop();

  if left.is_number() && right.is_number() {
    $vm.push(Value::from(left.as_number() $operator right.as_number()));
  } else if right.is_number() {
    break Some(ErrorKind::TypeError { expected: "number", got: left.get_type() });
  } else {
    break Some(ErrorKind::TypeError { expected: "number", got: right.get_type() });
  }
}}

macro comparison_operation(($vm:expr, $chunk:expr), $operator:tt) {{
  let right = $vm.pop();
  let left = $vm.pop();

  if left.is_number() && right.is_number() {
    $vm.push(Value::from(left.as_number() $operator right.as_number()));
  } else if left.is_string() && right.is_string() {
    $vm.push(Value::from(left.as_string() $operator right.as_string()));
  } else {
    break Some(ErrorKind::TypeErrorBinary {
      expected: "two numbers or two strings",
      got: format!("a {} and a {}", left.get_type(), right.get_type()),
    });
  }
}}

/// An error whilst executing bytecode
#[derive(Debug, Clone)]
pub struct RuntimeError {
  kind: ErrorKind,
  span: Span,
}
impl RuntimeError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    self.kind.title()
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> std::string::String {
    self.kind.message()
  }
}
impl GetSpan for RuntimeError {
  fn span(&self) -> Span {
    self.span
  }
}
impl fmt::Display for RuntimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for RuntimeError {}

#[derive(Debug, Clone)]
enum ErrorKind {
  TypeError {
    expected: &'static str,
    got: &'static str,
  },
  TypeErrorBinary {
    expected: &'static str,
    got: std::string::String,
  },
  UndefinedVariable(std::string::String),
  NotCallable(&'static str),
  NonFunctionClosure,
}
impl ErrorKind {
  #[must_use]
  fn title(&self) -> &'static str {
    match self {
      Self::TypeError { .. } | Self::TypeErrorBinary { .. } => "Type Error",
      Self::UndefinedVariable(_) => "Undefined Variable",
      Self::NotCallable(_) => "Not Callable",
      Self::NonFunctionClosure => "Non-Function Closure",
    }
  }

  #[must_use]
  fn message(&self) -> std::string::String {
    match self {
      Self::TypeError { expected, got } => format!("expected `{expected}`, got `{got}`"),
      Self::TypeErrorBinary { expected, got } => {
        format!("expected `{expected}`, got `{got}`")
      }
      Self::UndefinedVariable(variable_name) => {
        format!("variable `{variable_name}` is not defined")
      }
      Self::NotCallable(type_) => {
        format!("`{type_}` is not callable, only functions are callable")
      }
      Self::NonFunctionClosure => "can only close over functions".into(),
    }
  }
}
