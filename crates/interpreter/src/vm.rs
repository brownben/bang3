use super::{
  bytecode::{Chunk, ConstantValue, OpCode},
  object::{self, BangString, Closure, TypeDescriptor},
  stdlib::{Context, ImportResult},
  value::{TypeId, Value},
};
use bang_gc::{Gc, GcList, Heap, HeapSize};
use bang_syntax::{LineIndex, Span};

use rustc_hash::FxHashMap as HashMap;
use smartstring::alias::String as SmartString;
use std::{error, fmt, ops, ptr, slice};

#[derive(Debug)]
struct CallFrame {
  ip: usize,
  offset: usize,
  chunk: *const Chunk,
  upvalues: Option<GcList<Value>>,
  native_return: bool,
}
impl Default for CallFrame {
  fn default() -> Self {
    Self {
      ip: 0,
      offset: 0,
      chunk: ptr::null(),
      upvalues: None,
      native_return: false,
    }
  }
}

/// The configuration for the [`VM`]
#[derive(Debug)]
pub struct Config {
  /// The size of the heap to allocate
  pub heap_size: HeapSize,
  /// The size of the primary stack to allocate
  pub stack_size: usize,
  /// The size of the call stack to allocate
  pub call_stack_size: usize,
}
impl Config {
  /// A configuration with the smallest possible heap
  pub const SMALL: Self = Self {
    heap_size: HeapSize::Small,
    stack_size: 512,
    call_stack_size: 64,
  };
}
impl Default for Config {
  fn default() -> Self {
    Self {
      heap_size: HeapSize::default(),
      // sizes are one less than the power of 2, so it fits neatly in a ream
      stack_size: 1023,
      call_stack_size: 63,
    }
  }
}

/// A virtual machine to execute compiled bytecode
#[derive(Debug)]
pub struct VM<'context> {
  stack: Stack<Value>,
  frames: Stack<CallFrame>,
  globals: HashMap<SmartString, Value>,

  /// The heap
  pub heap: Heap,
  gc_threshold: usize,

  context: &'context dyn Context,
  types: &'static [TypeDescriptor],
}
impl<'context> VM<'context> {
  /// Create a new VM
  ///
  /// # Errors
  /// Returns an error if the heap could not be initialized
  pub fn new(config: &Config, context: &'context dyn Context) -> Result<Self, RuntimeError> {
    let Some(mut heap) = Heap::new(config.heap_size) else {
      return Err(RuntimeError {
        kind: ErrorKind::OutOfMemory,
        traceback: Vec::new(),
      });
    };

    let mut vm = Self {
      stack: Stack::new(config.stack_size, &mut heap),
      frames: Stack::new(config.call_stack_size, &mut heap),
      globals: HashMap::default(),
      heap,
      gc_threshold: 6,
      context,
      types: object::DEFAULT_TYPE_DESCRIPTORS,
    };

    for function in vm.context.global_functions() {
      let name = function.name;

      let function = vm.allocate_value(function, object::NATIVE_FUNCTION_TYPE_ID);
      vm.define_global(name, function);
    }
    for (constant_name, constant) in vm.context.global_constants() {
      vm.define_global(constant_name, constant);
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
    if a.is_number() && b.is_number() {
      return (a.as_number() - b.as_number()).abs() < f64::EPSILON;
    }

    if a == b {
      return true;
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
  /// Allocates a value in the [`VM`] returns a [`Value`] pointing to it
  pub fn allocate_value<T>(&mut self, value: T, type_id: TypeId) -> Value {
    let value = self.heap.allocate(value);
    Value::from_object(value, type_id)
  }

  /// Gets the [`TypeDescriptor`] for a [`Value`]
  ///
  /// SAFETY: the [`Value`] must be an object, and be from this [`VM`]
  #[inline]
  #[must_use]
  pub fn get_type_descriptor(&self, value: Value) -> &TypeDescriptor {
    debug_assert!(value.is_object());
    let type_id = value.object_type();

    debug_assert!(type_id.0 < self.types.len());
    unsafe { self.types.get_unchecked(type_id.0) }
  }
  /// Temporarily stash a value to the stack.
  ///
  /// Allows native functions to ensure values are not collected when executing other code.
  ///
  /// # Errors
  /// Returns an error if the stack is full
  pub fn stash_value(&mut self, value: Value) -> Result<StashedValue, ErrorKind> {
    push!(return, self.stack, value);
    Ok(StashedValue)
  }
  /// Temporarily stash an object to the stack.
  ///
  /// Allows native functions to ensure values are not collected when executing other code.
  ///
  /// # Errors
  /// Returns an error if the stack is full
  pub fn stash_object<T>(
    &mut self,
    value: Gc<T>,
    type_id: TypeId,
  ) -> Result<StashedValue, ErrorKind> {
    push!(return, self.stack, Value::from_object(value, type_id));
    Ok(StashedValue)
  }
  /// Gets a stashed value from the stack.
  pub fn pop_stashed_value(&mut self, _value: StashedValue) -> Value {
    self.stack.pop()
  }

  #[inline]
  fn get_last_frame_upvalues(&self) -> GcList<Value> {
    unsafe { self.frames.peek().upvalues.unwrap_unchecked() }
  }

  #[inline]
  fn should_garbage_collect(&self) -> bool {
    self.heap.full_page_count() > self.gc_threshold
  }
  /// Run the garbage collector
  pub fn garbage_collect(&mut self) {
    fn mark_value(vm: &VM, value: Value) {
      if value.is_object() {
        let ptr = value.as_object::<u8>();

        // if it has already been marked as used, we don't need to trace it again
        if vm.heap.is_used(ptr) {
          return;
        }

        vm.heap.mark(ptr);
        (vm.get_type_descriptor(value).trace)(vm, ptr, mark_value);
      }
    }

    self.heap.start_gc();

    self.heap.mark(*self.stack.gc);
    self.heap.mark(*self.frames.gc);

    for value in self.stack.as_ref() {
      mark_value(self, *value);
    }
    for value in self.globals.values() {
      mark_value(self, *value);
    }
    for frame in self.frames.as_ref() {
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
  pub fn call(&mut self, func: Value, argument: Value) -> Result<Option<Value>, ErrorKind> {
    if func.is_constant_function() {
      push!(return, self.stack, func);
      push!(return, self.frames, CallFrame {
        offset: self.stack.len(),
        chunk: func.as_function(&self.heap),
        native_return: true,
        ..CallFrame::default()
      });
      push!(return, self.stack, argument);

      let chunk = unsafe { &*func.as_function(&self.heap).as_ptr() };
      return self.main_loop::<false>(chunk).map_err(|error| error.kind);
    }

    if func.is_object() {
      if func.is_object_type(object::CLOSURE_TYPE_ID) {
        push!(return, self.stack, func);
        let closure = &self.heap[func.as_object::<Closure>()];
        push!(return, self.frames, CallFrame {
          offset: self.stack.len(),
          chunk: closure.function(),
          upvalues: Some(closure.upvalues),
          native_return: true,
          ..CallFrame::default()
        });
        push!(return, self.stack, argument);

        let chunk = unsafe { &*self.heap[func.as_object::<Closure>()].function().as_ptr() };
        return self.main_loop::<false>(chunk).map_err(|error| error.kind);
      }

      if let Some(native_function) = self.get_type_descriptor(func).call {
        push!(return, self.stack, func);
        push!(return, self.stack, argument);
        push!(return, self.frames, CallFrame {
          offset: self.stack.len() - 1,
          upvalues: None,
          native_return: true,
          ..CallFrame::default()
        });
        let result = native_function(self, func.as_object(), argument)?;
        _ = self.frames.pop();
        let (_func, _argument) = (self.stack.pop(), self.stack.pop());

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

    let mut instruction = chunk.get(ip);
    let error = loop {
      match instruction {
        // Constants
        OpCode::Constant => {
          let constant_position = chunk.get_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          let value = match constant {
            ConstantValue::String(string) => self.allocate_string(string),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          };
          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::ConstantLong => {
          let constant_position = chunk.get_long_value(ip + 1);
          let constant = chunk.get_constant(constant_position.into());

          let value = match constant {
            ConstantValue::String(string) => self.allocate_string(string),
            ConstantValue::Function(function) => Value::from(ptr::from_ref(function)),
          };
          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Number => {
          push!(self.stack, chunk.get_number(ip + 1).into());
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::True => {
          push!(self.stack, Value::TRUE);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::False => {
          push!(self.stack, Value::FALSE);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Null => {
          push!(self.stack, Value::NULL);
          next_instruction!(self, instruction, ip, chunk);
        }

        // Imports
        OpCode::Import => {
          let module = chunk.get_symbol(chunk.get_value(ip + 1).into()).as_str();
          let item = chunk.get_symbol(chunk.get_value(ip + 2).into()).as_str();

          match self.context.import_value(module, item) {
            ImportResult::Constant(value) => push!(self.stack, value),
            ImportResult::ConstantString(string) => {
              let value = self.allocate_string(string);
              push!(self.stack, value);
            }
            ImportResult::NativeFunction(native_function) => {
              let value = self.allocate_value(native_function, object::NATIVE_FUNCTION_TYPE_ID);
              push!(self.stack, value);
            }
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
          next_instruction!(self, instruction, ip, chunk);
        }

        // Numeric Operations
        OpCode::Add => numeric_operation!((self,instruction, ip, chunk), +),
        OpCode::Subtract => numeric_operation!((self,instruction, ip, chunk), -),
        OpCode::Multiply => numeric_operation!((self,instruction, ip, chunk), *),
        OpCode::Divide => numeric_operation!((self,instruction, ip, chunk), /),
        OpCode::Remainder => numeric_operation!((self,instruction, ip, chunk), %),

        // String Operations
        OpCode::AddString => {
          let (right, left) = (self.stack.pop(), self.stack.pop());

          if !left.is_string() || !right.is_string() {
            break Some(ErrorKind::TypeErrorBinary {
              expected: "two strings",
              got: format!("a {} and a {}", left.get_type(self), right.get_type(self)),
            });
          }

          let new_string = BangString::concatenate(&mut self.heap, left, right);
          push!(self.stack, new_string);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::ToString => {
          let value = self.stack.pop();

          let string = if value.is_string() {
            value
          } else {
            self.allocate_string(&value.display(self))
          };
          push!(self.stack, string);
          next_instruction!(self, instruction, ip, chunk);
        }

        // Unary Operations
        OpCode::Negate => {
          let value = self.stack.pop();

          if !value.is_number() {
            break Some(ErrorKind::TypeError {
              expected: "number",
              got: value.get_type(self),
            });
          }

          push!(self.stack, Value::from(-value.as_number()));
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Not => {
          let value = self.stack.pop();
          push!(self.stack, value.is_falsy(self).into());
          next_instruction!(self, instruction, ip, chunk);
        }

        // Equalities
        OpCode::Equals => {
          let (right, left) = (self.stack.pop(), self.stack.pop());
          push!(self.stack, self.equals(left, right).into());
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::NotEquals => {
          let (right, left) = (self.stack.pop(), self.stack.pop());
          push!(self.stack, (!self.equals(left, right)).into());
          next_instruction!(self, instruction, ip, chunk);
        }

        // Comparisons
        OpCode::Greater => comparison_operation!((self, instruction, ip, chunk), >),
        OpCode::GreaterEqual => comparison_operation!((self, instruction, ip, chunk), >=),
        OpCode::Less => comparison_operation!((self, instruction, ip, chunk), <),
        OpCode::LessEqual => comparison_operation!((self, instruction, ip, chunk), <=),

        // Variables
        OpCode::DefineGlobal => {
          let name = chunk.get_symbol(chunk.get_value(ip + 1).into()).clone();
          let value = self.stack.pop();

          self.globals.insert(name, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::GetGlobal => {
          let name = chunk.get_symbol(chunk.get_value(ip + 1).into());

          match self.globals.get(name) {
            Some(value) => push!(self.stack, *value),
            None => {
              break Some(ErrorKind::UndefinedVariable {
                name: name.to_string(),
              });
            }
          }
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::GetLocal => {
          let slot = chunk.get_value(ip + 1);
          let value = self.stack[offset + usize::from(slot)];
          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }

        // Functions
        OpCode::Call => {
          let callee = self.stack[self.stack.len() - 2];

          if callee.is_constant_function() {
            push!(self.frames, CallFrame {
              ip,
              offset,
              chunk,
              upvalues: None,
              native_return: false,
            });

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*callee.as_function(&self.heap).as_ptr() };
            instruction = chunk.get(ip); // no ip increment, as we're jumping to a new chunk
          } else if callee.is_object_type(object::CLOSURE_TYPE_ID) {
            let upvalues = self.heap[callee.as_object::<Closure>()].upvalues;
            push!(self.frames, CallFrame {
              ip,
              offset,
              chunk,
              upvalues: Some(upvalues),
              native_return: false,
            });

            ip = 0;
            offset = self.stack.len() - 1;
            chunk = unsafe { &*self.heap[callee.as_object::<Closure>()].function().as_ptr() };
            instruction = chunk.get(ip); // no ip increment, as we're jumping to a new chunk
          } else if callee.is_object()
            && let Some(native_function) = self.get_type_descriptor(callee).call
          {
            let argument = self.stack.peek();
            push!(self.frames, CallFrame {
              offset: self.stack.len() - 1,
              ..CallFrame::default()
            });

            match native_function(self, callee.as_object(), argument) {
              Ok(value) => {
                _ = self.frames.pop();
                let (_argument, _callee) = (self.stack.pop(), self.stack.pop());
                push!(self.stack, value);
                next_instruction!(self, instruction, ip, chunk);
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
          let result = self.stack.pop();
          self.stack.truncate(offset - 1);
          let frame = self.frames.pop();

          // If the frame is marked as native, we can return the value directly
          if !MAIN_SCRIPT && frame.native_return {
            return Ok(Some(result));
          }

          push!(self.stack, result);

          ip = frame.ip;
          offset = frame.offset;
          // SAFETY: callee stays on stack, so call stack reference will always exist
          chunk = unsafe { &*frame.chunk };
          next_instruction!(self, instruction, ip, chunk);
        }

        // Closures
        OpCode::Allocate => {
          let index = chunk.get_value(ip + 1);
          let local = &mut self.stack[offset + usize::from(index)];
          let allocated = Value::from_object(self.heap.allocate(*local), object::ALLOCATED_TYPE_ID);
          *local = allocated;
          push!(self.stack, allocated);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Closure => {
          let upvalue_count = chunk.get_value(ip + 1);
          let upvalues = (self.stack).drain(self.stack.len() - usize::from(upvalue_count));
          let upvalue_list = (self.heap).allocate_list(upvalues, upvalue_count.into());

          let function = self.stack.pop(); // assume is function, as bytecode is valid
          let closure = Closure::new(function.as_function(&self.heap), upvalue_list);

          let value = self.allocate_value(closure, object::CLOSURE_TYPE_ID);
          self.stack.push(value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::GetUpvalue => {
          let upvalue = chunk.get_value(ip + 1);
          let address: Value =
            self.heap.get_list_buffer(self.get_last_frame_upvalues())[usize::from(upvalue)];
          let value = self.heap[address.as_object::<Value>()];

          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::GetAllocatedValue => {
          let slot = chunk.get_value(ip + 1);
          let address = self.stack[offset + usize::from(slot)];
          let value = self.heap[address.as_object::<Value>()];

          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::GetAllocatedPointer => {
          let index = chunk.get_value(ip + 1);
          let allocated: Value =
            self.heap.get_list_buffer(self.get_last_frame_upvalues())[usize::from(index)];

          push!(self.stack, allocated);
          next_instruction!(self, instruction, ip, chunk);
        }

        // Lists
        OpCode::List => {
          let length = chunk.get_value(ip + 1);

          let start_of_items = self.stack.len() - usize::from(length);
          let items = self.stack.drain(start_of_items);

          let list = self.heap.allocate_list(items, length.into());
          (self.stack).push(Value::from_object(*list, object::LIST_TYPE_ID));
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::ListLength => {
          let value = self.stack.peek();

          #[allow(clippy::cast_precision_loss, reason = "value < 2^52")]
          let length = if value.is_list() {
            value.as_list(&self.heap).len() as f64
          } else {
            // we use NaN rather than an error here so that when used in a match
            // expression on a potentially hetrogeneous variables it doesn't error early
            //
            // there is no way for a user to use this opcode directly
            f64::NAN
          };

          push!(self.stack, length.into());
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::ListHeadTail => {
          let list = self.stack.pop();

          if !list.is_list() {
            break Some(ErrorKind::TypeError {
              expected: "list",
              got: list.get_type(self),
            });
          }

          let list_items = list.as_list(&self.heap);
          let head = *list_items.first().unwrap_or(&Value::NULL);
          let tail = (self.heap).allocate(object::ListView::new(self, list, 1, list_items.len()));

          push!(self.stack, head);
          (self.stack).push(Value::from_object(tail, object::LIST_VIEW_TYPE_ID));
          next_instruction!(self, instruction, ip, chunk);
        }

        // Options
        OpCode::OptionIsSome => {
          let is_some = self.stack.peek().is_object_type(object::SOME_TYPE_ID);
          push!(self.stack, is_some.into());
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::OptionIsNone => {
          let is_none = self.stack.peek().is_object_type(object::NONE_TYPE_ID);
          push!(self.stack, is_none.into());
          next_instruction!(self, instruction, ip, chunk);
        }

        // VM Operations
        OpCode::Pop => {
          self.stack.pop();
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::PopBelow => {
          let count = chunk.get_value(ip + 1);
          let value = self.stack.pop();
          self.stack.truncate(self.stack.len() - usize::from(count));
          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Peek => {
          let value = self.stack.peek();
          push!(self.stack, value);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Jump => {
          let jump = chunk.get_long_value(ip + 1);
          ip += usize::from(jump);
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::JumpIfFalse => {
          let jump = chunk.get_long_value(ip + 1);
          if self.stack.peek().is_falsy(self) {
            ip += usize::from(jump);
          }
          next_instruction!(self, instruction, ip, chunk);
        }
        OpCode::Halt => break None,
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

macro_rules! next_instruction {
  ($vm:expr, $instruction:expr, $ip:expr, $chunk:expr) => {{
    #[cfg(feature = "gc-stress-test")]
    $vm.garbage_collect();

    if $vm.should_garbage_collect() {
      $vm.garbage_collect();
    }

    #[cfg(feature = "debug-stack")]
    self.debug_stack(instruction);

    $ip += $instruction.length();
    $instruction = $chunk.get($ip);
  }};
}
use next_instruction;

macro_rules! numeric_operation {
  (($vm:expr, $instruction:expr, $ip:expr, $chunk:expr), $operator:tt) => {{
    let (right, left) = ($vm.stack.pop(), $vm.stack.pop());

    if left.is_number() && right.is_number() {
      $vm.stack.push(Value::from(left.as_number() $operator right.as_number()));
    } else if right.is_number() {
      break Some(ErrorKind::TypeError { expected: "number", got: left.get_type(&$vm) });
    } else {
      break Some(ErrorKind::TypeError { expected: "number", got: right.get_type(&$vm) });
    }

    next_instruction!($vm, $instruction, $ip, $chunk);
  }}
}
use numeric_operation;

macro_rules! comparison_operation {
  (($vm:expr, $instruction:expr, $ip:expr, $chunk:expr), $operator:tt) => {{
    let (right, left) = ($vm.stack.pop(), $vm.stack.pop());

    if left.is_number() && right.is_number() {
      $vm.stack.push(Value::from(left.as_number() $operator right.as_number()));
    } else if left.is_string() && right.is_string() {
      $vm.stack.push(Value::from(left.as_string(&$vm.heap) $operator right.as_string(&$vm.heap)));
    } else {
      break Some(ErrorKind::TypeErrorBinary {
        expected: "two numbers or two strings",
        got: format!("a `{}` and a `{}`", left.get_type(&$vm), right.get_type(&$vm)),
      });
    }

    next_instruction!($vm, $instruction, $ip, $chunk);
  }}
}
use comparison_operation;

pub struct StashedValue;

#[derive(Debug)]
struct Stack<T> {
  gc: GcList<T>,
  pointer: *mut T,
  len: usize,
  capacity: usize,
}
impl<T> Stack<T> {
  fn new(size: usize, heap: &mut Heap) -> Self {
    let gc = heap.allocate_list_header::<T>(size);
    let pointer = heap.get_list_buffer_ptr(gc).cast_mut();
    let capacity = gc.capacity(heap);

    Self {
      gc,
      pointer,
      len: 0,
      capacity,
    }
  }

  #[inline]
  #[must_use]
  fn is_empty(&self) -> bool {
    self.len == 0
  }

  #[inline]
  #[must_use]
  fn len(&self) -> usize {
    self.len
  }

  #[inline]
  fn push(&mut self, item: T) {
    debug_assert!(self.len < self.capacity);

    unsafe { *self.pointer.add(self.len) = item };
    self.len += 1;
  }

  #[inline]
  fn pop(&mut self) -> T {
    debug_assert!(!self.is_empty());

    self.len -= 1;
    unsafe { ptr::read(self.pointer.add(self.len)) }
  }
  #[inline]
  fn peek(&self) -> T {
    debug_assert!(!self.is_empty());

    unsafe { ptr::read(self.pointer.add(self.len - 1)) }
  }

  fn truncate(&mut self, new_size: usize) {
    debug_assert!(new_size <= self.capacity);

    self.len = new_size;
  }
}
impl<T: Copy> Stack<T> {
  fn drain(&mut self, new_size: usize) -> impl Iterator<Item = T> {
    debug_assert!(new_size <= self.capacity);

    let remaining_len = self.len - new_size;
    let s = unsafe { ptr::slice_from_raw_parts_mut(self.pointer.add(new_size), remaining_len) };
    self.len = new_size;
    unsafe { &*s }.iter().copied()
  }
}
impl<T> ops::Index<usize> for Stack<T> {
  type Output = T;

  fn index(&self, index: usize) -> &Self::Output {
    debug_assert!(index <= self.len);

    unsafe { &*self.pointer.add(index) }
  }
}
impl<T> ops::IndexMut<usize> for Stack<T> {
  fn index_mut(&mut self, index: usize) -> &mut Self::Output {
    debug_assert!(index <= self.len);

    unsafe { &mut *self.pointer.add(index) }
  }
}
impl<T> ops::Deref for Stack<T> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    unsafe { slice::from_raw_parts_mut(self.pointer, self.len()) }
  }
}

macro_rules! push {
  (return, $stack:expr, $value:expr) => {{
    if $stack.len() == $stack.capacity {
      return Err(ErrorKind::StackOverflow);
    }

    $stack.push($value);
  }};

  ($stack:expr, $value:expr) => {{
    if $stack.len() == $stack.capacity {
      break Some(ErrorKind::StackOverflow);
    }

    $stack.push($value);
  }};
}
use push;

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

    fn write_traceback_item(
      string: &mut String,
      line_index: &LineIndex,
      location: &StackTraceLocation,
    ) {
      if let Some(span) = location.span {
        let line = line_index.line(span);

        match &location.kind {
          StackTraceLocationKind::Root => {
            writeln!(string, "at line {line}").unwrap();
          }
          StackTraceLocationKind::Function(name) if name.is_empty() => {
            writeln!(string, "in anonymous function at line {line}").unwrap();
          }
          StackTraceLocationKind::Function(name) => {
            writeln!(string, "in function '{name}' at line {line}").unwrap();
          }
        }
      } else {
        match &location.kind {
          StackTraceLocationKind::Function(name) => {
            writeln!(string, "in native function '{name}'").unwrap();
          }
          StackTraceLocationKind::Root => unreachable!(),
        }
      }
    }

    if self.traceback.is_empty() {
      return None;
    }

    let mut string = String::new();

    if self.traceback.len() > 16 {
      for location in &self.traceback[..10] {
        write_traceback_item(&mut string, line_index, location);
      }
      writeln!(string, "... {} frames hidden", self.traceback.len() - 16).unwrap();
      for location in &self.traceback[self.traceback.len() - 6..] {
        write_traceback_item(&mut string, line_index, location);
      }
    } else {
      for location in &self.traceback {
        write_traceback_item(&mut string, line_index, location);
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

/// The kind of an error which occured in the runtime
#[derive(Debug, Clone)]
pub enum ErrorKind {
  /// A type error
  TypeError {
    /// The expected type
    expected: &'static str,
    /// The type recieved
    got: &'static str,
  },
  /// A type error
  TypeErrorBinary {
    /// The expected type
    expected: &'static str,
    /// The type recieved
    got: String,
  },
  /// Accessing a variable which does not exist
  UndefinedVariable {
    /// the name of the variable
    name: String,
  },
  /// The value is not callable
  NotCallable {
    /// the type of the value
    type_: &'static str,
  },
  /// Enough memory could not be allocated for the heap
  OutOfMemory,
  /// Too many items in the stack
  StackOverflow,
  /// A module couldn't be found
  ModuleNotFound {
    /// the name of the module
    module: String,
  },
  /// An item could not be found with a specific name in a module
  ItemNotFound {
    /// the name of the module
    module: String,
    /// the name of the item which could not be found
    item: String,
  },
  /// A different kind of error, specified by the creator
  Custom {
    /// the title of the error
    title: &'static str,
    /// the message of the error
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
      Self::StackOverflow => "Stack Overflow",
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
      Self::StackOverflow => "the stack has overflowed. consider increasing the stack size".into(),
      Self::ModuleNotFound { module } => format!("could not find module `{module}`"),
      Self::ItemNotFound { module, item } => format!("could not find `{item}` in `{module}`"),
      Self::Custom { message, .. } => message.clone(),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    Self {
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
impl Default for StackTraceLocation {
  fn default() -> Self {
    Self {
      kind: StackTraceLocationKind::Root,
      span: None,
    }
  }
}
#[derive(Clone, Debug, PartialEq, Eq)]
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
