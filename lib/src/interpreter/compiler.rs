use super::bytecode::{Chunk, ConstantValue, OpCode};
use crate::{
  allocator::{Allocator, Vec},
  ast::{expression::*, statement::*, GetSpan, Span},
  collections::{SmallVec, String},
  parser::AST,
};
use std::{error, fmt, mem};

pub struct Compiler<'s, 'a> {
  allocator: &'a Allocator,

  chunk: Chunk,
  chunk_stack: Vec<'a, Chunk>,

  scope_depth: u8,
  locals: Vec<'a, Vec<'a, Local<'s>>>,
  closures: Vec<'a, SmallVec<[(u8, VariableStatus); 8]>>,
}
impl<'s, 'a> Compiler<'s, 'a> {
  pub fn new(allocator: &'a Allocator) -> Self {
    let mut locals = Vec::new_in(allocator);
    locals.push(Vec::new_in(allocator));

    Self {
      allocator,

      chunk: Chunk::new("main".into()),
      chunk_stack: Vec::new_in(allocator),

      scope_depth: 0,
      locals,
      closures: Vec::new_in(allocator),
    }
  }
  pub fn compile(&mut self, ast: &AST<'s, '_>) -> Result<(), CompileError> {
    ast.compile(self)
  }
  pub fn finish(mut self) -> Chunk {
    self.chunk.add_opcode(OpCode::Halt, Span::default());
    self.chunk.finalize();
    self.chunk
  }

  fn new_chunk(&mut self, name: String) {
    let chunk = mem::replace(&mut self.chunk, Chunk::new(name));
    self.chunk_stack.push(chunk);

    self.begin_scope();
    self.locals.push(Vec::new_in(self.allocator));
    self.closures.push(SmallVec::new());
  }
  fn finish_chunk(&mut self, span: Span) -> Result<Chunk, CompileError> {
    self.end_scope(span)?;
    self.locals.pop();

    let mut chunk = mem::replace(&mut self.chunk, self.chunk_stack.pop().unwrap());
    chunk.finalize();
    Ok(chunk)
  }

  fn add_constant(&mut self, value: ConstantValue, span: Span) -> Result<(), CompileError> {
    let constant_position = self.chunk.add_constant(value);

    if let Ok(constant_position) = u8::try_from(constant_position) {
      self.chunk.add_opcode(OpCode::Constant, span);
      self.chunk.add_value(constant_position, span);
    } else if let Ok(constant_position) = u16::try_from(constant_position) {
      self.chunk.add_opcode(OpCode::ConstantLong, span);
      self.chunk.add_long_value(constant_position, span);
    } else {
      Err(CompileError::TooManyConstants)?;
    }

    Ok(())
  }
  fn add_constant_string(&mut self, string: &str, span: Span) -> Result<(), CompileError> {
    let string_position = self.chunk.add_string(string);

    if let Ok(string_position) = u8::try_from(string_position) {
      self.chunk.add_value(string_position, span);
    } else {
      Err(CompileError::TooManyStrings)?;
    }

    Ok(())
  }

  fn add_jump(&mut self, instruction: OpCode, span: Span) -> usize {
    self.chunk.add_opcode(instruction, span);
    self.chunk.add_long_value(u16::MAX, span);
    self.chunk.len() - 2
  }
  fn patch_jump(&mut self, offset: usize) -> Result<(), CompileError> {
    let jump = self.chunk.len() - offset - 2;

    if let Ok(jump) = u16::try_from(jump) {
      self.chunk.replace_long_value(offset, jump);
      Ok(())
    } else {
      Err(CompileError::TooBigJump)
    }
  }

  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }
  fn end_scope(&mut self, span: Span) -> Result<(), CompileError> {
    let mut count: usize = 0;

    while let Some(last) = self.function_locals().last()
      && last.depth == self.scope_depth
    {
      self.function_locals().pop();
      count += 1;
    }

    if count > 0 {
      self.chunk.add_opcode(OpCode::PopBelow, span);
      if let Ok(count) = u8::try_from(count) {
        self.chunk.add_value(count, span);
      } else {
        Err(CompileError::TooManyLocalVariables)?;
      }
    }
    self.scope_depth -= 1;

    Ok(())
  }
  fn function_locals(&mut self) -> &mut Vec<'a, Local<'s>> {
    self.locals.last_mut().unwrap()
  }
  fn define_variable(&mut self, name: &'s str, span: Span) -> Result<(), CompileError> {
    let depth = self.scope_depth;

    if depth > 0 {
      self.function_locals().push(Local {
        name,
        depth,
        status: VariableStatus::Open,
      });
    } else {
      self.chunk.add_opcode(OpCode::DefineGlobal, span);
      self.add_constant_string(name, span)?;
    }

    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Local<'s> {
  name: &'s str,
  depth: u8,
  status: VariableStatus,
}
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum VariableStatus {
  /// The local variable is a normal variable on the stack
  #[default]
  Open,
  /// The variable has been allocated to the heap, but exists on the stack
  Closed,
  /// The variable is in an outer function, and must be accessed through the call stack
  Upvalue,
}

trait Compile<'s> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError>;
}

impl<'s> Compile<'s> for AST<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    for statement in &self.statements {
      statement.compile(compiler)?;

      match statement {
        Statement::Expression(_) => compiler.chunk.add_opcode(OpCode::Pop, statement.span()),
        Statement::Comment(_) | Statement::Let(_) => {}
      }
    }

    Ok(())
  }
}

impl<'s> Compile<'s> for Expression<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    match self {
      Expression::Binary(binary) => binary.compile(compiler),
      Expression::Block(block) => block.compile(compiler),
      Expression::Call(call) => call.compile(compiler),
      Expression::Comment(comment) => comment.compile(compiler),
      Expression::Function(function) => function.compile(compiler),
      Expression::Group(group) => group.compile(compiler),
      Expression::If(if_) => if_.compile(compiler),
      Expression::Literal(literal) => literal.compile(compiler),
      Expression::Match(match_) => match_.compile(compiler),
      Expression::Unary(unary) => unary.compile(compiler),
      Expression::Variable(variable) => variable.compile(compiler),
      Expression::Invalid => Err(CompileError::InvalidAST),
    }
  }
}
impl<'s> Compile<'s> for Binary<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    match self.operator {
      BinaryOperator::And => {
        self.left.compile(compiler)?;
        let jump = compiler.add_jump(OpCode::JumpIfFalse, self.span);
        compiler.chunk.add_opcode(OpCode::Pop, self.span);
        self.right.compile(compiler)?;
        compiler.patch_jump(jump)?;

        return Ok(());
      }
      BinaryOperator::Or => {
        self.left.compile(compiler)?;
        let else_jump = compiler.add_jump(OpCode::JumpIfFalse, self.span);
        let end_jump = compiler.add_jump(OpCode::Jump, self.span);

        compiler.patch_jump(else_jump)?;
        compiler.chunk.add_opcode(OpCode::Pop, self.span);

        self.right.compile(compiler)?;
        compiler.patch_jump(end_jump)?;

        return Ok(());
      }
      BinaryOperator::Pipeline => {
        self.right.compile(compiler)?;
        self.left.compile(compiler)?;
        compiler.chunk.add_opcode(OpCode::Call, self.span);

        return Ok(());
      }
      _ => {}
    }

    self.left.compile(compiler)?;
    self.right.compile(compiler)?;

    compiler.chunk.add_opcode(
      match self.operator {
        BinaryOperator::Add => OpCode::Add,
        BinaryOperator::Subtract => OpCode::Subtract,
        BinaryOperator::Multiply => OpCode::Multiply,
        BinaryOperator::Divide => OpCode::Divide,
        BinaryOperator::Remainder => OpCode::Remainder,
        BinaryOperator::AddString => OpCode::AddString,
        BinaryOperator::NotEqual => OpCode::NotEquals,
        BinaryOperator::Equal => OpCode::Equals,
        BinaryOperator::Greater => OpCode::Greater,
        BinaryOperator::GreaterEqual => OpCode::GreaterEqual,
        BinaryOperator::Less => OpCode::Less,
        BinaryOperator::LessEqual => OpCode::LessEqual,
        BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Pipeline => {
          unreachable!("handled above")
        }
      },
      self.span,
    );

    Ok(())
  }
}
impl<'s> Compile<'s> for Block<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    compiler.begin_scope();

    let (last, rest) = self.statements.split_last().unwrap();
    for statement in rest {
      statement.compile(compiler)?;

      match statement {
        Statement::Expression(_) => compiler.chunk.add_opcode(OpCode::Pop, self.span),
        Statement::Comment(_) | Statement::Let(_) => {}
      }
    }
    match last {
      Statement::Expression(_) => last.compile(compiler)?,
      stmt => Err(CompileError::BlockMustEndWithExpression(stmt.span()))?,
    }

    compiler.end_scope(self.span)
  }
}
impl<'s> Compile<'s> for Call<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;
    if let Some(argument) = &self.argument {
      argument.compile(compiler)?;
    } else {
      compiler.chunk.add_opcode(OpCode::Null, self.span);
    }
    compiler.chunk.add_opcode(OpCode::Call, self.span);
    Ok(())
  }
}
impl<'s> Compile<'s> for Comment<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.expression.compile(compiler)
  }
}
impl<'s> Compile<'s> for Function<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    let name = self.name.unwrap_or("").into();
    compiler.new_chunk(name);

    compiler.define_variable(self.parameter.name, self.span)?;
    self.body.compile(compiler)?;
    compiler.chunk.add_opcode(OpCode::Return, self.span);

    let function_chunk = compiler.finish_chunk(self.span)?;
    compiler.add_constant(function_chunk.into(), self.span)?;

    let upvalues = compiler.closures.pop().unwrap();
    if !upvalues.is_empty() {
      for (upvalue_index, status) in &upvalues {
        compiler.chunk.add_opcode(
          match status {
            VariableStatus::Open => OpCode::Allocate,
            VariableStatus::Closed => OpCode::GetLocal,
            VariableStatus::Upvalue => OpCode::GetAllocatedPointer,
          },
          self.span,
        );
        compiler.chunk.add_value(*upvalue_index, self.span);
      }

      compiler.chunk.add_opcode(OpCode::Closure, self.span);
      if let Ok(value) = u8::try_from(upvalues.len()) {
        compiler.chunk.add_value(value, self.span);
      } else {
        return Err(CompileError::TooManyClosures);
      }
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Group<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.expression.compile(compiler)
  }
}
impl<'s> Compile<'s> for If<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.condition.compile(compiler)?;
    let jump_to_else = compiler.add_jump(OpCode::JumpIfFalse, self.span);

    compiler.chunk.add_opcode(OpCode::Pop, self.span);
    self.then.compile(compiler)?;
    let jump_to_end = compiler.add_jump(OpCode::Jump, self.span);

    compiler.patch_jump(jump_to_else)?;
    compiler.chunk.add_opcode(OpCode::Pop, self.span);
    self.otherwise.compile(compiler)?;

    compiler.patch_jump(jump_to_end)?;

    Ok(())
  }
}
impl<'s> Compile<'s> for Literal<'s> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    match self.kind {
      LiteralKind::Boolean(true) => compiler.chunk.add_opcode(OpCode::True, self.span),
      LiteralKind::Boolean(false) => compiler.chunk.add_opcode(OpCode::False, self.span),
      LiteralKind::Number { value, .. } => compiler.chunk.add_number(value, self.span),
      LiteralKind::String(value) => compiler.add_constant(value.into(), self.span)?,
    };

    Ok(())
  }
}
impl<'s> Compile<'s> for Match<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    let is_exhaustive = self
      .cases
      .iter()
      .any(|case| matches!(case.pattern, Pattern::Identifier(_)));

    self.value.compile(compiler)?;

    let mut case_end_jumps = Vec::new_in(compiler.allocator);
    for case in &self.cases {
      match &case.pattern {
        Pattern::Identifier(variable) => {
          compiler.begin_scope();
          compiler.define_variable(variable.name, variable.span)?;
          case.expression.compile(compiler)?;
          compiler.end_scope(case.span)?; // clears up the value being matched upon
          break; // optimisation: nothing can match after this
        }
        Pattern::Literal(literal) => {
          // Check if the value matches the literal
          compiler.chunk.add_opcode(OpCode::Peek, self.span);
          literal.compile(compiler)?;
          compiler.chunk.add_opcode(OpCode::Equals, self.span);

          // If it does, do the expression
          let non_match_jump = compiler.add_jump(OpCode::JumpIfFalse, self.span);
          compiler.chunk.add_opcode(OpCode::Pop, self.span);
          case.expression.compile(compiler)?;
          case_end_jumps.push(compiler.add_jump(OpCode::Jump, self.span));

          // Otherwise skip to the next case
          compiler.patch_jump(non_match_jump)?;
          compiler.chunk.add_opcode(OpCode::Pop, self.span);
        }
        Pattern::Range(range) => {
          // check that the value is within the range
          match (&range.start, &range.end) {
            (None, Some(pattern)) => {
              compiler.chunk.add_opcode(OpCode::Peek, self.span);
              pattern.get_range_literal().compile(compiler)?;
              compiler.chunk.add_opcode(OpCode::LessEqual, self.span);
            }
            (Some(pattern), None) => {
              compiler.chunk.add_opcode(OpCode::Peek, self.span);
              pattern.get_range_literal().compile(compiler)?;
              compiler.chunk.add_opcode(OpCode::GreaterEqual, self.span);
            }
            (Some(greater_than), Some(less_than)) => {
              compiler.chunk.add_opcode(OpCode::Peek, self.span);
              greater_than.get_range_literal().compile(compiler)?;
              compiler.chunk.add_opcode(OpCode::GreaterEqual, self.span);

              let jump = compiler.add_jump(OpCode::JumpIfFalse, self.span);
              compiler.chunk.add_opcode(OpCode::Pop, self.span);

              compiler.chunk.add_opcode(OpCode::Peek, self.span);
              less_than.get_range_literal().compile(compiler)?;
              compiler.chunk.add_opcode(OpCode::LessEqual, self.span);

              compiler.patch_jump(jump)?;
            }
            (None, None) => unreachable!("range has bound, checked in parser"),
          }

          // If it does, do the expression
          let non_match_jump = compiler.add_jump(OpCode::JumpIfFalse, self.span);
          compiler.chunk.add_opcode(OpCode::Pop, self.span);
          case.expression.compile(compiler)?;
          case_end_jumps.push(compiler.add_jump(OpCode::Jump, self.span));

          // Otherwise skip to the next case
          compiler.patch_jump(non_match_jump)?;
          compiler.chunk.add_opcode(OpCode::Pop, self.span);
        }
      }
    }

    if !is_exhaustive {
      compiler.chunk.add_opcode(OpCode::Pop, self.span);
      compiler.chunk.add_opcode(OpCode::Null, self.span);
    }

    for jump in case_end_jumps {
      compiler.patch_jump(jump)?;
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Unary<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;

    compiler.chunk.add_opcode(
      match self.operator {
        UnaryOperator::Not => OpCode::Not,
        UnaryOperator::Minus => OpCode::Negate,
      },
      self.span,
    );

    Ok(())
  }
}
impl<'s> Compile<'s> for Variable<'s> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    // See if the variable is local to the current function
    if let Some(local_position) = compiler
      .function_locals()
      .iter()
      .rposition(|local| local.name == self.name)
    {
      match compiler.function_locals()[local_position].status {
        VariableStatus::Open => compiler.chunk.add_opcode(OpCode::GetLocal, self.span),
        VariableStatus::Closed => compiler
          .chunk
          .add_opcode(OpCode::GetAllocatedValue, self.span),
        VariableStatus::Upvalue => unreachable!("we are only checking the current scope"),
      }
      if let Ok(local_position) = u8::try_from(local_position) {
        compiler.chunk.add_value(local_position, self.span);
      } else {
        Err(CompileError::TooManyLocalVariables)?;
      }

      return Ok(());
    }

    // Check for it being a closure of an outer function
    if let Some((scope_index, local_index)) = compiler
      .locals
      .iter()
      .enumerate()
      .rev()
      .skip(1)
      .find_map(|(scope_index, locals)| {
        locals
          .iter()
          .rposition(|local| local.name == self.name)
          .map(|local_index| (scope_index, local_index))
      })
    {
      let local_status = mem::replace(
        &mut compiler.locals[scope_index][local_index].status,
        VariableStatus::Closed,
      );

      let (mut index, mut status) = (local_index, local_status);
      for closures in compiler.closures.iter_mut().skip(scope_index) {
        index = closures
          .iter()
          .rposition(|(i, _)| usize::from(*i) == index)
          .unwrap_or_else(|| {
            closures.push((u8::try_from(index).unwrap_or(0), status));
            closures.len() - 1
          });
        status = VariableStatus::Upvalue;
      }

      compiler.chunk.add_opcode(OpCode::GetUpvalue, self.span);
      if let Ok(index) = u8::try_from(index) {
        compiler.chunk.add_value(index, self.span);
      } else {
        Err(CompileError::TooManyClosures)?;
      }

      return Ok(());
    }

    // Otherwise, it must be a global variable
    compiler.chunk.add_opcode(OpCode::GetGlobal, self.span);
    compiler.add_constant_string(self.name, self.span)?;

    Ok(())
  }
}

impl<'s> Compile<'s> for Statement<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    match self {
      Statement::Comment(_) => Ok(()),
      Statement::Expression(expression) => expression.compile(compiler),
      Statement::Let(let_) => let_.compile(compiler),
    }
  }
}
impl<'s> Compile<'s> for Let<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s, '_>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;
    compiler.define_variable(self.identifier.name, self.span)
  }
}

/// An error from compiling an AST into bytecode
#[derive(Debug, Clone, Copy)]
pub enum CompileError {
  /// Too many constants
  TooManyConstants,
  /// Too many strings
  TooManyStrings,
  /// Too big jump
  TooBigJump,
  /// Too many closures
  TooManyClosures,
  /// Too many local variables
  TooManyLocalVariables,
  /// Block must end with expression
  BlockMustEndWithExpression(Span),
  /// AST to compile has an error in it
  InvalidAST,
}
impl CompileError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "Too Many Constants",
      Self::TooManyStrings => "Too Many Strings",
      Self::TooBigJump => "Too Big Jump",
      Self::TooManyClosures => "Too Many Closures",
      Self::TooManyLocalVariables => "Too Many Local Variables",
      Self::BlockMustEndWithExpression(_) => "Block Must End With Expression",
      Self::InvalidAST => "Invalid AST",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "the maximum no. of constants has been reached (65536)",
      Self::TooManyStrings => "the maximum no. of strings has been reached (256)",
      Self::TooBigJump => "the maximum jump size has been reached (65536)",
      Self::TooManyClosures => "the maximum no. of closures has been reached (256)",
      Self::TooManyLocalVariables => "more than 256 local variables have been defined",
      Self::BlockMustEndWithExpression(_) => {
        "a blocks must return a value, so must end with an expression"
      }
      Self::InvalidAST => "the AST contains an error, see errors from parser",
    }
  }
}
impl GetSpan for CompileError {
  fn span(&self) -> Span {
    match self {
      Self::BlockMustEndWithExpression(span) => *span,
      _ => Span::default(),
    }
  }
}
impl fmt::Display for CompileError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for CompileError {}
