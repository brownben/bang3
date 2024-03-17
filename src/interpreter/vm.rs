use super::{
  bytecode::{Chunk, OpCode},
  value::{Closure, ClosureStatus, Object, Value},
};
use crate::collections::{HashMap, SmallVec};
use std::{error, fmt};

#[derive(Clone, Debug)]
struct CallFrame {
  ip: usize,
  offset: usize,
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
  pub fn get_global(&self, name: &str) -> Option<Value> {
    self.globals.get(name).cloned()
  }

  #[inline]
  fn pop(&mut self) -> Value {
    // SAFETY: Assume bytecode is valid, so stack is not empty
    unsafe { self.stack.pop().unwrap_unchecked() }
  }
  #[inline]
  fn peek(&self) -> &Value {
    unsafe { self.stack.last().unwrap_unchecked() }
  }
  #[inline]
  fn push(&mut self, value: Value) {
    self.stack.push(value);
  }
  #[inline]
  fn get_stack_value(&self, position: usize) -> Value {
    // SAFETY: Assume bytecode is valid, so position exists
    unsafe { self.stack.get_unchecked(position) }.clone()
  }

  #[inline]
  fn peek_frame(&self) -> &CallFrame {
    // SAFETY: Assume bytecode is valid, so frames is not empty
    unsafe { self.frames.last().unwrap_unchecked() }
  }
  #[inline]
  fn push_frame(&mut self, ip: usize, offset: usize, upvalues: SmallVec<[Value; 4]>) {
    self.frames.push(CallFrame {
      ip,
      offset,
      upvalues,
    });
  }

  /// Run a chunk of bytecode
  ///
  /// # Errors
  /// Returns an error if a runtime error is encountered in the bytecode being executed
  #[allow(clippy::too_many_lines)]
  pub fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut ip = 0;
    let mut offset = 0;

    loop {
      let instruction = chunk.get(ip);

      match instruction {
        // Constants
        OpCode::Constant => {
          let constant_position = chunk.get_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());
          self.push(constant);
        }
        OpCode::ConstantLong => {
          let constant_position = chunk.get_long_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());
          self.push(constant);
        }
        OpCode::True => self.push(Value::TRUE),
        OpCode::False => self.push(Value::FALSE),

        // Numeric Operations
        OpCode::Add => numeric_operation!((self, chunk), +),
        OpCode::Subtract => numeric_operation!((self, chunk), -),
        OpCode::Multiply => numeric_operation!((self, chunk), *),
        OpCode::Divide => numeric_operation!((self, chunk), /),
        OpCode::Remainder => numeric_operation!((self, chunk), %),
        OpCode::AddString => {
          let right = self.pop();
          let left = self.pop();

          if (left.is_object() && right.is_object())
            && let Object::String(left) = left.as_object()
            && let Object::String(right) = right.as_object()
          {
            let mut new = left.clone();
            new.push_str(right);
            self.push(new.into());
          } else {
            break Err(RuntimeError::TypeErrorBinary {
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
            break Err(RuntimeError::TypeError {
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
            None => break Err(RuntimeError::UndefinedVariable(string.into())),
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

          if !callee.is_object() {
            break Err(RuntimeError::NotCallable(callee.get_type()));
          }

          match callee.as_object() {
            Object::Function(func) => {
              self.push_frame(ip, offset, SmallVec::new());
              ip = func.start - 1;
              offset = self.stack.len() - 1;
            }
            Object::NativeFunction(func) => {
              let result = func(&self.pop());
              self.push(result);
            }
            Object::Closure(closure) => {
              self.push_frame(ip, offset, closure.upvalues.clone());
              ip = closure.func.start - 1;
              offset = self.stack.len() - 1;
            }
            Object::String(_) => {
              break Err(RuntimeError::NotCallable(callee.get_type()));
            }
          }
        }
        OpCode::Return => {
          let result = self.pop();
          self.stack.drain(offset - 1..);
          self.push(result);

          let frame = self.peek_frame();
          ip = frame.ip;
          offset = frame.offset;
        }

        // Closures
        OpCode::Closure => {
          let value = self.pop();

          if value.is_object()
            && let Object::Function(func) = value.as_object()
          {
            let upvalues = func
              .upvalues
              .iter()
              .map(|(index, closed)| match closed {
                ClosureStatus::Open => {
                  let local = &mut self.stack[offset + usize::from(*index)];
                  let allocated = local.clone().allocate();
                  *local = allocated.clone();
                  allocated
                }
                ClosureStatus::Closed => self.stack[offset + usize::from(*index)].clone(),
                ClosureStatus::Upvalue => self.peek_frame().upvalues[usize::from(*index)].clone(),
              })
              .collect();

            self.push(Closure::new(func.clone(), upvalues).into());
          } else {
            break Err(RuntimeError::NonFunctionClosure);
          }
        }
        OpCode::GetUpvalue => {
          let upvalue = chunk.get_value(ip + 1);
          let address = self.peek_frame().upvalues[usize::from(upvalue)].as_allocated();

          self.push(address.borrow().clone());
        }
        OpCode::GetAllocated => {
          let slot = chunk.get_value(ip + 1);
          let address = self.stack[offset + usize::from(slot)].as_allocated();

          self.push(address.borrow().clone());
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
        OpCode::Halt => break Ok(()),
      };

      ip += instruction.length();
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
    break Err(RuntimeError::TypeError { expected: "number", got: left.get_type() });
  } else {
    break Err(RuntimeError::TypeError { expected: "number", got: right.get_type() });
  }
}}

macro comparison_operation(($vm:expr, $chunk:expr), $operator:tt) {{
  let right = $vm.pop();
  let left = $vm.pop();

  if left.is_number() && right.is_number() {
    $vm.push(Value::from(left.as_number() $operator right.as_number()));
  } else if (left.is_object() && right.is_object())
    && let Object::String(left) = left.as_object()
    && let Object::String(right) = right.as_object()
  {
    $vm.push(Value::from(left $operator right));
  } else {
    break Err(RuntimeError::TypeErrorBinary {
      expected: "two numbers or two strings",
      got: format!("a {} and a {}", left.get_type(), right.get_type()),
    });
  }
}}

/// An error whilst executing bytecode
#[derive(Debug, Clone)]
pub enum RuntimeError {
  /// The type of a value doesn't match the type needed for the operation
  TypeError {
    /// the type expected to have been received
    expected: &'static str,
    /// the type that was actually received
    got: &'static str,
  },
  /// The type of a value doesn't match the type needed for the operation
  TypeErrorBinary {
    /// the type expected to have been received
    expected: &'static str,
    /// the type that was actually received
    got: String,
  },
  /// A variable was tried to be accessed, but it was not defined
  UndefinedVariable(String),
  /// Tried to call a value which is not a function
  NotCallable(&'static str),
  /// Tried to close over a non-function value
  NonFunctionClosure,
}
impl RuntimeError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::TypeError { .. } | Self::TypeErrorBinary { .. } => "Type Error",
      Self::UndefinedVariable(_) => "Undefined Variable",
      Self::NotCallable(_) => "Not Callable",
      Self::NonFunctionClosure => "Non-Function Closure",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    match self {
      RuntimeError::TypeError { expected, got } => {
        format!("expected `{expected}`, got `{got}`")
      }
      RuntimeError::TypeErrorBinary { expected, got } => {
        format!("expected `{expected}`, got `{got}`")
      }
      RuntimeError::UndefinedVariable(variable_name) => {
        format!("variable `{variable_name}` is not defined")
      }
      RuntimeError::NotCallable(type_) => {
        format!("`{type_}` is not callable, only functions are callable")
      }
      RuntimeError::NonFunctionClosure => "can only close over functions".into(),
    }
  }
}
impl fmt::Display for RuntimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for RuntimeError {}
