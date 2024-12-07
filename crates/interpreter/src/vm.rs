use super::{
  bytecode::{Chunk, ConstantValue, OpCode},
  object::{self, BangString, Closure, TypeDescriptor},
  stdlib::{Context, ImportResult},
  value::Value,
};
use bang_gc::{GcList, Heap, HeapSize};
use bang_syntax::{LineIndex, Span};

use rustc_hash::FxHashMap as HashMap;
use smartstring::alias::String as SmartString;
use std::{error, fmt, ptr};

#[derive(Debug)]
struct CallFrame {
  ip: usize,
  offset: usize,
  chunk: *const Chunk,
  upvalues: Option<GcList<Value>>,
  native_return: bool,
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
  types: Vec<TypeDescriptor>,
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
      types: object::DEFAULT_TYPE_DESCRIPTORS.to_vec(),
    };

    for function in vm.context.global_functions() {
      let name = function.name;
      let function = vm.heap.allocate(function);
      vm.define_global(
        name,
        Value::from_object(function, object::NATIVE_FUNCTION_TYPE_ID),
      );
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

  /// Checks if two [`Value`]s are equal to each other
  ///
  /// SAFETY: both `Value`s must be from this [`VM`]
  #[must_use]
  pub fn equals(&self, a: Value, b: Value) -> bool {
    if a == b {
      return true;
    }

    if a.is_number() && b.is_number() {
      return (a.as_number() - b.as_number()).abs() < f64::EPSILON;
    }

    if a.is_object() && b.is_object() {
      let type_descriptor = &self.get_type_descriptor(a);
      return (type_descriptor.equals)(self, a, b);
    }

    false
  }

  /// Allocates a new string in the [`VM`] returns a [`Value`] pointing to it
  pub fn allocate_string(&mut self, value: &str) -> Value {
    let string = self.heap.allocate_list(value.bytes(), value.len());
    Value::from_object(*string, object::STRING_TYPE_ID)
  }

  #[inline]
  pub(crate) fn get_type_descriptor(&self, value: Value) -> &TypeDescriptor {
    debug_assert!(value.is_object());
    let type_id = value.object_type();

    debug_assert!(type_id.0 < self.types.len());
    unsafe { self.types.get_unchecked(type_id.0) }
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
      native_return: false,
    });
  }
  #[inline]
  fn push_frame_from_native(&mut self, chunk: *const Chunk, upvalues: Option<GcList<Value>>) {
    self.frames.push(CallFrame {
      ip: 0,
      offset: self.stack.len(),
      chunk,
      upvalues,
      native_return: true,
    });
  }
  #[inline]
  fn push_native_frame(&mut self, upvalues: Option<GcList<Value>>, native_return: bool) {
    self.frames.push(CallFrame {
      ip: 0,
      offset: self.stack.len(),
      chunk: ptr::null(),
      upvalues,
      native_return,
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
    fn mark_value(vm: &VM, value: Value) {
      if value.is_object() {
        vm.heap.mark(value.as_object::<u8>());
        (vm.get_type_descriptor(value).trace)(vm, value.as_object(), mark_value);
      }
    }

    self.heap.start_gc();

    for value in &self.stack {
      mark_value(self, *value);
    }
    for value in self.globals.values() {
      mark_value(self, *value);
    }
    for frame in &self.frames {
      if let Some(upvalues) = frame.upvalues {
        self.heap.mark(*upvalues);

        for upvalue in self.heap.get_list_buffer(upvalues) {
          mark_value(self, *upvalue);
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
  pub fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
    let result = self.main_loop::<true>(chunk);

    if let Err(MainLoopError {
      ip,
      offset,
      chunk,
      kind,
    }) = result
    {
      let mut traceback = Vec::with_capacity(self.stack.len() + 1);
      let chunk = unsafe { &*chunk };

      if offset != 0 {
        traceback.push(StackTraceLocation::from_chunk_ip(chunk, ip, offset));
      }
      traceback.extend(
        (self.frames.iter())
          .rev()
          .map(|frame| StackTraceLocation::from_frame(self, frame)),
      );
      if offset == 0 {
        traceback.push(StackTraceLocation::from_chunk_ip(chunk, ip, offset));
      }

      Err(RuntimeError { kind, traceback })
    } else {
      Ok(())
    }
  }

  /// Call a [`Value`] with the given argument
  ///
  /// Returns an optional value. Which is `None` if the function stops execution instead of
  /// returning a value.
  ///
  /// # Errors
  /// Returns an error if the given value is not callable, or if a runtime error is
  /// encountered during the bytecode execution
  pub(crate) fn call(&mut self, func: Value, argument: Value) -> Result<Option<Value>, ErrorKind> {
    if func.is_constant_function() {
      self.push(func);
      self.push_frame_from_native(func.as_function(&self.heap), None);
      self.push(argument);

      let chunk = unsafe { &*func.as_function(&self.heap).as_ptr() };
      return self.main_loop::<false>(chunk).map_err(|error| error.kind);
    }

    if func.is_object() {
      if func.is_object_type(object::CLOSURE_TYPE_ID) {
        self.push(func);
        let closure = &self.heap[func.as_object::<Closure>()];
        self.push_frame_from_native(closure.function(), Some(closure.upvalues));
        self.push(argument);

        let chunk = unsafe { &*self.heap[func.as_object::<Closure>()].function().as_ptr() };
        return self.main_loop::<false>(chunk).map_err(|error| error.kind);
      }

      if let Some(native_function) = self.get_type_descriptor(func).call {
        self.push(func);
        self.push_native_frame(None, true);
        let result = native_function(self, func.as_object(), argument)?;
        _ = self.pop_frame();
        self.pop();

        return Ok(Some(result));
      }
    }

    Err(ErrorKind::NotCallable {
      type_: func.get_type(self),
    })
  }

  #[allow(clippy::too_many_lines)]
  fn main_loop<const MAIN_SCRIPT: bool>(
    &mut self,
    chunk: &Chunk,
  ) -> Result<Option<Value>, MainLoopError> {
    let mut ip = 0;
    let mut offset = if MAIN_SCRIPT { 0 } else { self.stack.len() - 1 };
    let mut chunk = chunk;

    let error = loop {
      let instruction = chunk.get(ip);

      match instruction {
        // Constants
        OpCode::Constant => {
          let constant_position = chunk.get_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          let value = match constant {
            ConstantValue::String(string) => self.allocate_string(string),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          };
          self.push(value);
        }
        OpCode::ConstantLong => {
          let constant_position = chunk.get_long_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          let value = match constant {
            ConstantValue::String(string) => self.allocate_string(string),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          };
          self.push(value);
        }
        OpCode::Number => self.push(chunk.get_number(ip + 1).into()),
        OpCode::True => self.push(Value::TRUE),
        OpCode::False => self.push(Value::FALSE),
        OpCode::Null => self.push(Value::NULL),

        // Imports
        OpCode::Import => {
          let module = chunk.get_symbol(chunk.get_value(ip + 1).into()).as_str();
          let item = chunk.get_symbol(chunk.get_value(ip + 2).into()).as_str();

          match self.context.import_value(self, module, item) {
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
            let new_string = BangString::concatenate(&mut self.heap, left, right);
            self.push(new_string);
          } else {
            break Some(ErrorKind::TypeErrorBinary {
              expected: "two strings",
              got: format!("a {} and a {}", left.get_type(self), right.get_type(self)),
            });
          }
        }
        OpCode::ToString => {
          let value = self.pop();

          let string = if value.is_string() {
            value
          } else {
            self.allocate_string(&value.display(self))
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
              got: value.get_type(self),
            });
          }
        }
        OpCode::Not => {
          let value = self.pop();
          self.push(value.is_falsy(self).into());
        }

        // Equalities
        OpCode::Equals => {
          let (right, left) = (self.pop(), self.pop());
          self.push(self.equals(left, right).into());
        }
        OpCode::NotEquals => {
          let (right, left) = (self.pop(), self.pop());
          self.push((!self.equals(left, right)).into());
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
            None => {
              break Some(ErrorKind::UndefinedVariable {
                name: name.to_string(),
              });
            }
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

          if callee.is_constant_function() {
            self.push_frame(ip, offset, chunk, None);

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*callee.as_function(&self.heap).as_ptr() };

            continue; // skip the ip increment, as we're jumping to a new chunk
          } else if callee.is_object_type(object::CLOSURE_TYPE_ID) {
            let upvalues = self.heap[callee.as_object::<Closure>()].upvalues;
            self.push_frame(ip, offset, chunk, Some(upvalues));

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*self.heap[callee.as_object::<Closure>()].function().as_ptr() };

            continue; // skip the ip increment, as we're jumping to a new chunk
          } else if callee.is_object()
            && let Some(native_function) = self.get_type_descriptor(callee).call
          {
            let argument = self.pop();
            self.push_native_frame(None, false);

            match native_function(self, callee.as_object(), argument) {
              Ok(value) => {
                _ = self.pop_frame();
                let _callee = self.pop();
                self.push(value);
              }
              Err(err) => break Some(err),
            }
          } else {
            break Some(ErrorKind::NotCallable {
              type_: callee.get_type(self),
            });
          }
        }
        OpCode::Return => {
          let result = self.pop();
          self.stack.drain(offset - 1..);
          let frame = self.pop_frame();

          // If the frame is marked as native, we can return the value directly
          if !MAIN_SCRIPT && frame.native_return {
            return Ok(Some(result));
          }

          self.push(result);

          ip = frame.ip;
          offset = frame.offset;
          // SAFETY: callee stays on stack, so call stack reference will always exist
          chunk = unsafe { &*frame.chunk };
        }

        // Closures
        OpCode::Allocate => {
          let index = chunk.get_value(ip + 1);
          let local = &mut self.stack[offset + usize::from(index)];
          let allocated = Value::from_object(self.heap.allocate(*local), object::ALLOCATED_TYPE_ID);
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
          self.push(Value::from_object(closure, object::CLOSURE_TYPE_ID));
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

        // Lists
        OpCode::List => {
          let length = chunk.get_value(ip + 1);

          let start_of_items = self.stack.len() - usize::from(length);
          let items = self.stack.drain(start_of_items..);

          let list = self.heap.allocate_list(items, length.into());
          self.push(Value::from_object(*list, object::LIST_TYPE_ID));
        }
        OpCode::ListLength => {
          let value = self.peek();

          #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
          let length = if value.is_list() {
            value.as_list(&self.heap).len() as f64
          } else {
            f64::NAN
          };

          self.push(length.into());
        }
        OpCode::ListHeadTail => {
          let list = self.peek();

          if !list.is_list() {
            break Some(ErrorKind::TypeError {
              expected: "list",
              got: list.get_type(self),
            });
          }

          let list_items = list.as_list(&self.heap);
          let head = *list_items.first().unwrap_or(&Value::NULL);
          let tail = (self.heap).allocate(object::ListView::new(self, list, 1, list_items.len()));

          self.push(head);
          self.push(Value::from_object(tail, object::LIST_VIEW_TYPE_ID));
        }

        // Options
        OpCode::OptionIsSome => {
          let is_some = self.peek().is_object_type(object::SOME_TYPE_ID);
          self.push(is_some.into());
        }
        OpCode::OptionIsNone => {
          let is_none = self.peek().is_object_type(object::NONE_TYPE_ID);
          self.push(is_none.into());
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
          if self.peek().is_falsy(self) {
            ip += usize::from(jump);
          }
        }
        OpCode::Halt => break None,
      };

      #[cfg(feature = "debug-stack")]
      self.debug_stack(instruction);

      ip += instruction.length();

      // Only garbage collect if we are in the main script. If we are in a native function
      // callback, we could have references to some values which wouldn't be traced by the GC
      if MAIN_SCRIPT && self.should_garbage_collect() {
        self.garbage_collect();
      }
    };

    if let Some(kind) = error {
      Err(MainLoopError {
        ip,
        offset,
        kind,
        chunk: chunk.as_ptr(),
      })
    } else {
      Ok(None)
    }
  }

  #[cfg(feature = "debug-stack")]
  fn debug_stack(&self, instruction: OpCode) {
    print!("{instruction:?} [");

    for (index, value) in self.stack.iter().enumerate() {
      if index > 0 {
        print!(", ");
      }

      print!("{}", value.debug(self));
    }
    println!("]");
  }
}

macro_rules! numeric_operation {
  (($vm:expr, $chunk:expr), $operator:tt) => {{
    let right = $vm.pop();
    let left = $vm.pop();

    if left.is_number() && right.is_number() {
      $vm.push(Value::from(left.as_number() $operator right.as_number()));
    } else if right.is_number() {
      break Some(ErrorKind::TypeError { expected: "number", got: left.get_type(&$vm) });
    } else {
      break Some(ErrorKind::TypeError { expected: "number", got: right.get_type(&$vm) });
    }
  }}
}
use numeric_operation;

macro_rules! comparison_operation {
  (($vm:expr, $chunk:expr), $operator:tt) => {{
    let right = $vm.pop();
    let left = $vm.pop();

    if left.is_number() && right.is_number() {
      $vm.push(Value::from(left.as_number() $operator right.as_number()));
    } else if left.is_string() && right.is_string() {
      $vm.push(Value::from(left.as_string(&$vm.heap) $operator right.as_string(&$vm.heap)));
    } else {
      break Some(ErrorKind::TypeErrorBinary {
        expected: "two numbers or two strings",
        got: format!("a `{}` and a `{}`", left.get_type(&$vm), right.get_type(&$vm)),
      });
    }
  }}
}
use comparison_operation;

#[derive(Debug)]
struct MainLoopError {
  ip: usize,
  offset: usize,
  chunk: *const Chunk,
  kind: ErrorKind,
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
      if let Some(span) = location.span {
        let line = line_index.line(span);

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
      } else {
        match &location.kind {
          StackTraceLocationKind::Function(name) => {
            writeln!(string, "in native function '{name}'").unwrap();
          }
          StackTraceLocationKind::Root => unreachable!(),
        };
      }
    }

    Some(string)
  }

  /// The location of the error in the source code
  pub fn span(&self) -> Span {
    (self.traceback.iter())
      .find_map(|x| x.span)
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
pub(crate) enum ErrorKind {
  TypeError {
    expected: &'static str,
    got: &'static str,
  },
  TypeErrorBinary {
    expected: &'static str,
    got: String,
  },
  UndefinedVariable {
    name: String,
  },
  NotCallable {
    type_: &'static str,
  },
  OutOfMemory,
  ModuleNotFound {
    module: String,
  },
  ItemNotFound {
    module: String,
    item: String,
  },
  Custom {
    title: &'static str,
    message: String,
  },
}
impl ErrorKind {
  #[must_use]
  fn title(&self) -> &'static str {
    match self {
      Self::TypeError { .. } | Self::TypeErrorBinary { .. } => "Type Error",
      Self::UndefinedVariable { .. } => "Undefined Variable",
      Self::NotCallable { .. } => "Not Callable",
      Self::OutOfMemory => "Out of Memory",
      Self::ModuleNotFound { .. } => "Module Not Found",
      Self::ItemNotFound { .. } => "Item Not Found",
      Self::Custom { title, .. } => title,
    }
  }

  #[must_use]
  fn message(&self) -> String {
    match self {
      Self::TypeError { expected, got } => format!("expected `{expected}`, got `{got}`"),
      Self::TypeErrorBinary { expected, got } => format!("expected {expected}, got {got}"),
      Self::UndefinedVariable { name } => format!("variable `{name}` is not defined"),
      Self::NotCallable { type_ } => {
        format!("`{type_}` is not callable, only functions are callable")
      }
      Self::OutOfMemory => "could not initialise enough memory for the heap".into(),
      Self::ModuleNotFound { module } => format!("could not find module `{module}`"),
      Self::ItemNotFound { module, item } => format!("could not find `{item}` in `{module}`"),
      Self::Custom { message, .. } => message.clone(),
    }
  }
}

#[derive(Clone, Debug)]
struct StackTraceLocation {
  kind: StackTraceLocationKind,
  span: Option<Span>,
}
impl StackTraceLocation {
  fn from_chunk_ip(chunk: &Chunk, ip: usize, offset: usize) -> Self {
    let span = chunk.get_span(ip);

    let kind = if offset == 0 {
      StackTraceLocationKind::Root
    } else {
      StackTraceLocationKind::Function(chunk.name.clone())
    };

    StackTraceLocation {
      kind,
      span: Some(span),
    }
  }
  fn from_frame(vm: &VM, frame: &CallFrame) -> Self {
    if frame.offset == 0 {
      // If the offset is 0, we are not in a native function, so the chunk exists
      let chunk = unsafe { &*frame.chunk };
      let span = Some(chunk.get_span(frame.ip));

      return Self {
        kind: StackTraceLocationKind::Root,
        span,
      };
    }

    let function = vm.stack[frame.offset - 1];
    let span = (!frame.chunk.is_null()).then(|| unsafe { &*frame.chunk }.get_span(frame.ip));

    Self {
      kind: StackTraceLocationKind::Function(get_function_name(vm, function)),
      span,
    }
  }
}
#[derive(Clone, Debug)]
enum StackTraceLocationKind {
  Function(SmartString),
  Root,
}

fn get_function_name(vm: &VM, func: Value) -> SmartString {
  use object::{NativeClosure, NativeClosureTwo, NativeFunction};

  if func.is_constant_function() {
    return func.as_function(&vm.heap).name.clone();
  }

  match func.object_type() {
    object::CLOSURE_TYPE_ID => vm.heap[func.as_object::<Closure>()].function().name.clone(),

    object::NATIVE_FUNCTION_TYPE_ID => vm.heap[func.as_object::<NativeFunction>()].name.into(),
    object::NATIVE_CLOSURE_TYPE_ID => vm.heap[func.as_object::<NativeClosure>()].name.into(),
    object::NATIVE_CLOSURE_TWO_TYPE_ID => vm.heap[func.as_object::<NativeClosureTwo>()].name.into(),

    _ => "".into(),
  }
}
