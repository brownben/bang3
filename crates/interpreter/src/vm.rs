use super::{
  bytecode::{Chunk, ConstantValue, OpCode},
  context::{Context, ImportResult},
  object::{BangString, Closure, NativeFunction},
  value::{Type, Value},
};
use bang_gc::{GcList, Heap, HeapSize};
use bang_parser::{GetSpan, LineIndex, Span};

use rustc_hash::FxHashMap as HashMap;
use smartstring::alias::String as SmartString;
use std::{error, fmt, ptr};

#[derive(Debug)]
struct CallFrame {
  ip: usize,
  offset: usize,
  chunk: *const Chunk,
  upvalues: Option<GcList<Value>>,
}

/// A virtual machine to execute compiled bytecode
#[derive(Debug)]
pub struct VM<'context> {
  stack: Vec<Value>,
  globals: HashMap<SmartString, Value>,
  frames: Vec<CallFrame>,

  pub(crate) heap: Heap,
  gc_threshold: usize,

  context: &'context dyn Context,
}
impl<'context> VM<'context> {
  /// Create a new VM
  ///
  /// # Errors
  /// Returns an error if the heap could not be initialized
  pub fn new(heap_size: HeapSize, context: &'context dyn Context) -> Result<Self, RuntimeError> {
    let Some(heap) = Heap::new(heap_size) else {
      return Err(RuntimeError {
        kind: ErrorKind::OutOfMemory,
        traceback: Vec::new(),
      });
    };

    let mut vm = Self {
      stack: Vec::with_capacity(512),
      heap,
      globals: HashMap::default(),
      frames: Vec::with_capacity(16),
      gc_threshold: 6,
      context,
    };

    for function in vm.context.global_functions() {
      let name = function.name;
      let function = vm.heap.allocate(function);
      vm.define_global(name, Value::from_object(function, Type::NativeFunction));
    }

    Ok(vm)
  }

  /// Get the value of a global variable
  #[must_use]
  pub fn get_global(&self, name: &str) -> Option<Value> {
    self.globals.get(name).copied()
  }
  /// Set the value of a global variable
  pub fn define_global(&mut self, name: impl Into<SmartString>, value: impl Into<Value>) {
    self.globals.insert(name.into(), value.into());
  }

  #[inline]
  fn pop(&mut self) -> Value {
    // SAFETY: Assume bytecode is valid, so stack is not empty
    debug_assert!(!self.stack.is_empty());

    unsafe { self.stack.pop().unwrap_unchecked() }
  }
  #[inline]
  fn peek(&self) -> Value {
    // SAFETY: Assume bytecode is valid, so stack is not empty
    debug_assert!(!self.stack.is_empty());

    unsafe { *self.stack.last().unwrap_unchecked() }
  }
  #[inline]
  fn push(&mut self, value: Value) {
    self.stack.push(value);
  }
  #[inline]
  fn get_stack_value(&self, position: usize) -> Value {
    // SAFETY: Assume bytecode is valid, so position exists
    debug_assert!(position < self.stack.len());

    *unsafe { self.stack.get_unchecked(position) }
  }

  #[inline]
  fn push_frame(
    &mut self,
    ip: usize,
    offset: usize,
    chunk: *const Chunk,
    upvalues: Option<GcList<Value>>,
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

  #[inline]
  fn get_last_frame_upvalues(&self) -> GcList<Value> {
    // SAFETY: Assume bytecode is valid, so frames is not empty
    debug_assert!(!self.frames.is_empty());

    unsafe {
      self
        .frames
        .last()
        .unwrap_unchecked()
        .upvalues
        .unwrap_unchecked()
    }
  }

  #[inline]
  fn should_garbage_collect(&mut self) -> bool {
    #[cfg(feature = "gc-stress-test")]
    {
      // Collect garbage after every instruction
      // Means any problems in the marking will be caught faster
      true
    }

    #[cfg(not(feature = "gc-stress-test"))]
    {
      self.heap.full_page_count() > self.gc_threshold
    }
  }
  /// Run the garbage collector
  pub fn garbage_collect(&mut self) {
    fn mark_value(heap: &Heap, value: Value) {
      if value.is_object() {
        heap.mark(value.as_object::<u8>());

        if value.is_object_type(Type::Allocated) {
          let allocated_value = heap[value.as_object::<Value>()];
          mark_value(heap, allocated_value);
        }

        if value.is_object_type(Type::Closure) {
          let closure = &heap[value.as_object::<Closure>()];
          heap.mark(*closure.upvalues);
          for upvalue in heap.get_list_buffer(closure.upvalues) {
            mark_value(heap, *upvalue);
          }
        }
      }
    }

    self.heap.start_gc();

    for value in &self.stack {
      mark_value(&self.heap, *value);
    }
    for value in self.globals.values() {
      mark_value(&self.heap, *value);
    }
    for frame in &self.frames {
      if let Some(upvalues) = frame.upvalues {
        self.heap.mark(*upvalues);

        for upvalue in self.heap.get_list_buffer(upvalues) {
          mark_value(&self.heap, *upvalue);
        }
      }
    }

    self.heap.finish_gc();

    self.gc_threshold = (self.heap.full_page_count() * 2).max(2);
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

        // Imports
        OpCode::Import => {
          let module = chunk.get_symbol(chunk.get_value(ip + 1).into()).as_str();
          let item = chunk.get_symbol(chunk.get_value(ip + 2).into()).as_str();

          match self.context.import_value(&mut self.heap, module, item) {
            ImportResult::Value(value) => self.push(value),
            ImportResult::ModuleNotFound => {
              break Some(ErrorKind::ModuleNotFound {
                module: module.to_owned(),
              });
            }
            ImportResult::ItemNotFound => {
              break Some(ErrorKind::ItemNotFound {
                module: module.to_owned(),
                item: item.to_owned(),
              });
            }
          }
        }

        // Numeric Operations
        OpCode::Add => numeric_operation!((self, chunk), +),
        OpCode::Subtract => numeric_operation!((self, chunk), -),
        OpCode::Multiply => numeric_operation!((self, chunk), *),
        OpCode::Divide => numeric_operation!((self, chunk), /),
        OpCode::Remainder => numeric_operation!((self, chunk), %),

        // String Operations
        OpCode::AddString => {
          let right = self.pop();
          let left = self.pop();

          if left.is_string() && right.is_string() {
            let (left, right) = {
              let left = left.as_string(&self.heap);
              let right = right.as_string(&self.heap);

              ((left.as_ptr(), left.len()), (right.as_ptr(), right.len()))
            };

            let new_string = BangString::concatenate(&mut self.heap, left, right);
            self.push(new_string);
          } else {
            break Some(ErrorKind::TypeErrorBinary {
              expected: "two strings",
              got: format!("a {} and a {}", left.get_type(), right.get_type()),
            });
          }
        }
        OpCode::ToString => {
          let value = self.pop();

          let string = if value.is_string() {
            value
          } else {
            let string = value.display(&self.heap);
            allocate_string(&mut self.heap, &string)
          };
          self.push(string);
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
          self.push(value.is_falsy(&self.heap).into());
        }

        // Equalities
        OpCode::Equals => {
          let (right, left) = (self.pop(), self.pop());
          self.push(left.equals(right, &self.heap).into());
        }
        OpCode::NotEquals => {
          let (right, left) = (self.pop(), self.pop());
          self.push((!left.equals(right, &self.heap)).into());
        }

        // Comparisons
        OpCode::Greater => comparison_operation!((self, chunk), >),
        OpCode::GreaterEqual => comparison_operation!((self, chunk), >=),
        OpCode::Less => comparison_operation!((self, chunk), <),
        OpCode::LessEqual => comparison_operation!((self, chunk), <=),

        // Variables
        OpCode::DefineGlobal => {
          let name = chunk.get_symbol(chunk.get_value(ip + 1).into()).clone();
          let value = self.pop();

          self.globals.insert(name, value);
        }
        OpCode::GetGlobal => {
          let name = chunk.get_symbol(chunk.get_value(ip + 1).into());

          match self.globals.get(name) {
            Some(value) => self.push(*value),
            None => break Some(ErrorKind::UndefinedVariable(name.to_string())),
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
            self.push_frame(ip, offset, chunk, None);

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*callee.as_function(&self.heap).as_ptr() };
          } else if callee.is_object_type(Type::Closure) {
            let upvalues = self.heap[callee.as_object::<Closure>()].upvalues;
            self.push_frame(ip, offset, chunk, Some(upvalues));

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*self.heap[callee.as_object::<Closure>()].function().as_ptr() };
          } else if callee.is_object_type(Type::NativeFunction) {
            let argument = self.pop();
            let function = &self.heap[callee.as_object::<NativeFunction>()];

            let result = (function.func)(self, argument);
            self.pop();
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
          let allocated = Value::from_object(self.heap.allocate(*local), Type::Allocated);
          *local = allocated;
          self.push(allocated);
        }
        OpCode::Closure => {
          let upvalue_count = chunk.get_value(ip + 1);
          let upvalues = self
            .stack
            .drain((self.stack.len() - usize::from(upvalue_count))..);
          let upvalue_list = self.heap.allocate_list(upvalues, upvalue_count.into());

          let value = self.pop(); // assume is function, as bytecode is valid
          let closure = self
            .heap
            .allocate(Closure::new(value.as_function(&self.heap), upvalue_list));
          self.push(Value::from_object(closure, Type::Closure));
        }
        OpCode::GetUpvalue => {
          let upvalue = chunk.get_value(ip + 1);
          let address: Value =
            self.heap.get_list_buffer(self.get_last_frame_upvalues())[usize::from(upvalue)];
          let value = self.heap[address.as_object::<Value>()];

          self.push(value);
        }
        OpCode::GetAllocatedValue => {
          let slot = chunk.get_value(ip + 1);
          let address = self.stack[offset + usize::from(slot)];
          let value = self.heap[address.as_object::<Value>()];

          self.push(value);
        }
        OpCode::GetAllocatedPointer => {
          let index = chunk.get_value(ip + 1);
          let allocated: Value =
            self.heap.get_list_buffer(self.get_last_frame_upvalues())[usize::from(index)];

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
          let value = self.peek();
          self.push(value);
        }
        OpCode::Jump => {
          let jump = chunk.get_long_value(ip + 1);
          ip += usize::from(jump);
        }
        OpCode::JumpIfFalse => {
          let jump = chunk.get_long_value(ip + 1);
          if self.peek().is_falsy(&self.heap) {
            ip += usize::from(jump);
          }
        }
        OpCode::Halt => break None,
      };

      ip += instruction.length();

      if self.should_garbage_collect() {
        self.garbage_collect();
      }
    };

    // move constants from the chunk to the heap
    for value in self.globals.values_mut() {
      if value.is_constant_string() {
        *value = allocate_string(&mut self.heap, value.as_constant_string());
      }
      // TODO: store chunks on the heap, to ensure if they are stored they can be accessed later
    }

    if let Some(error) = error {
      let traceback = std::iter::once(StackTraceLocation::from_chunk_ip(chunk, ip, offset))
        .chain(self.frames.iter().rev().map(StackTraceLocation::from_frame))
        .collect();

      Err(RuntimeError {
        kind: error,
        traceback,
      })
    } else {
      Ok(())
    }
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
    $vm.push(Value::from(left.as_string(&$vm.heap) $operator right.as_string(&$vm.heap)));
  } else {
    break Some(ErrorKind::TypeErrorBinary {
      expected: "two numbers or two strings",
      got: format!("a {} and a {}", left.get_type(), right.get_type()),
    });
  }
}}

/// Allocates a string on the heap and returns a `Value` pointing to it.
pub(crate) fn allocate_string(heap: &mut Heap, value: &str) -> Value {
  let string = heap.allocate_list(value.bytes(), value.len());
  Value::from_object(*string, Type::String)
}

/// An error whilst executing bytecode
#[derive(Debug, Clone)]
pub struct RuntimeError {
  kind: ErrorKind,
  traceback: Vec<StackTraceLocation>,
}
impl RuntimeError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    self.kind.title()
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> String {
    self.kind.message()
  }

  /// The traceback of the error
  #[must_use]
  pub fn traceback(&self, line_index: &LineIndex) -> Option<String> {
    use std::fmt::Write;

    if self.traceback.is_empty() {
      return None;
    }

    let mut string = String::new();

    for location in &self.traceback {
      let line = line_index.get_line(location.span);
      match &location.kind {
        StackTraceLocationKind::Root => {
          writeln!(&mut string, "at line {line}").unwrap();
        }
        StackTraceLocationKind::Function(name) if name.is_empty() => {
          writeln!(string, "in anonymous function at line {line}").unwrap();
        }
        StackTraceLocationKind::Function(name) => {
          writeln!(string, "in function '{name}' at line {line}").unwrap();
        }
      };
    }

    Some(string)
  }
}
impl GetSpan for RuntimeError {
  fn span(&self) -> Span {
    self
      .traceback
      .first()
      .map(GetSpan::span)
      .unwrap_or_default()
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
    got: String,
  },
  UndefinedVariable(String),
  NotCallable(&'static str),
  OutOfMemory,
  ModuleNotFound {
    module: String,
  },
  ItemNotFound {
    module: String,
    item: String,
  },
}
impl ErrorKind {
  #[must_use]
  fn title(&self) -> &'static str {
    match self {
      Self::TypeError { .. } | Self::TypeErrorBinary { .. } => "Type Error",
      Self::UndefinedVariable(_) => "Undefined Variable",
      Self::NotCallable(_) => "Not Callable",
      Self::OutOfMemory => "Out of Memory",
      Self::ModuleNotFound { .. } => "Module Not Found",
      Self::ItemNotFound { .. } => "Item Not Found",
    }
  }

  #[must_use]
  fn message(&self) -> String {
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
      Self::OutOfMemory => "could not initialise enough memory for the heap".into(),
      Self::ModuleNotFound { module } => {
        format!("could not find module `{module}`")
      }
      Self::ItemNotFound { module, item } => {
        format!("could not find `{item}` in `{module}`")
      }
    }
  }
}

#[derive(Clone, Debug)]
struct StackTraceLocation {
  kind: StackTraceLocationKind,
  span: Span,
}
impl StackTraceLocation {
  fn from_chunk_ip(chunk: &Chunk, ip: usize, offset: usize) -> Self {
    let span = chunk.get_span(ip);

    let kind = if offset == 0 {
      StackTraceLocationKind::Root
    } else {
      StackTraceLocationKind::Function(chunk.name.clone())
    };

    StackTraceLocation { kind, span }
  }
  fn from_frame(frame: &CallFrame) -> Self {
    let chunk = unsafe { &*frame.chunk };

    Self::from_chunk_ip(chunk, frame.ip, frame.offset)
  }
}
impl GetSpan for StackTraceLocation {
  fn span(&self) -> Span {
    self.span
  }
}
#[derive(Clone, Debug)]
enum StackTraceLocationKind {
  Function(SmartString),
  Root,
}
