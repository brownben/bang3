use super::bytecode::{Chunk, ChunkBuilder, ConstantValue, OpCode};
use bang_syntax::{
  AST, Span,
  ast::{expression::*, statement::*},
};
use std::{error, fmt, mem};

pub struct Compiler<'s> {
  chunk: ChunkBuilder<'s>,
  chunk_stack: Vec<ChunkBuilder<'s>>,

  scope_depth: u8,
  locals: Vec<Vec<Local<'s>>>,
  closures: Vec<Vec<(u8, VariableStatus)>>,
}
impl<'s> Compiler<'s> {
  fn new() -> Self {
    let mut locals = Vec::with_capacity(8);
    locals.push(Vec::new());

    Self {
      chunk: ChunkBuilder::new("main"),
      chunk_stack: Vec::with_capacity(2),

      scope_depth: 0,
      locals,
      closures: Vec::with_capacity(8),
    }
  }
  fn finish(mut self) -> Chunk {
    self.chunk.add_opcode(OpCode::Halt, Span::default());
    self.chunk.finalize()
  }

  pub fn compile(ast: &AST) -> Result<Chunk, CompileError> {
    let mut compiler = Compiler::new();

    for statement in &ast.root_statements {
      statement.compile(&mut compiler, ast)?;

      match statement {
        Statement::Expression(_) => compiler.chunk.add_opcode(OpCode::Pop, statement.span(ast)),
        Statement::Comment(_) | Statement::Import(_) | Statement::Let(_) | Statement::Return(_) => {
        }
      }
    }

    Ok(compiler.finish())
  }
  pub fn compile_expression(expression: &Expression, ast: &AST) -> Result<Chunk, CompileError> {
    let mut compiler = Compiler::new();
    expression.compile(&mut compiler, ast)?;
    compiler
      .chunk
      .add_opcode(OpCode::Return, expression.span(ast));
    Ok(compiler.finish())
  }

  fn new_chunk(&mut self, name: &'s str) {
    let chunk = mem::replace(&mut self.chunk, ChunkBuilder::new(name));
    self.chunk_stack.push(chunk);

    self.begin_scope();
    self.locals.push(Vec::new());
    self.closures.push(Vec::new());
  }
  fn finish_chunk(&mut self, span: Span) -> Result<Chunk, CompileError> {
    self.end_scope(span)?;
    self.locals.pop();

    let chunk = mem::replace(&mut self.chunk, self.chunk_stack.pop().unwrap());
    Ok(chunk.finalize())
  }

  fn add_constant(
    &mut self,
    value: impl Into<ConstantValue>,
    span: Span,
  ) -> Result<(), CompileError> {
    let constant_position = self.chunk.add_constant(value);

    if let Ok(constant_position) = u8::try_from(constant_position) {
      self.chunk.add_opcode(OpCode::Constant, span);
      self.chunk.add_value(constant_position, span);
    } else if let Ok(constant_position) = u16::try_from(constant_position) {
      self.chunk.add_opcode(OpCode::ConstantLong, span);
      self.chunk.add_long_value(constant_position, span);
    } else {
      return Err(CompileError::TooManyConstants);
    }

    Ok(())
  }
  fn add_symbol(&mut self, string: &'s str, span: Span) -> Result<(), CompileError> {
    let string_position = self.chunk.add_symbol(string);

    if let Ok(string_position) = u8::try_from(string_position) {
      self.chunk.add_value(string_position, span);
    } else {
      return Err(CompileError::TooManySymbols);
    }

    Ok(())
  }

  fn add_jump(&mut self, instruction: OpCode, span: Span) -> Jump {
    self.chunk.add_opcode(instruction, span);
    self.chunk.add_long_value(u16::MAX, span);
    Jump(self.chunk.len() - 2)
  }
  fn patch_jump(&mut self, offset: Jump) -> Result<(), CompileError> {
    let jump = self.chunk.len() - offset.0 - 2;

    if let Ok(jump) = u16::try_from(jump) {
      self.chunk.replace_long_value(offset.0, jump);
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
        return Err(CompileError::TooManyLocalVariables);
      }
    }
    self.scope_depth -= 1;

    Ok(())
  }
  fn function_locals(&mut self) -> &mut Vec<Local<'s>> {
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
      self.add_symbol(name, span)?;
    }

    Ok(())
  }
}

/// The location of a jump which has not yet been patched
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Jump(usize);

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
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError>;
}

impl<'s> Compile<'s> for Expression {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    match self {
      Self::Binary(binary) => binary.compile(compiler, ast),
      Self::Block(block) => block.compile(compiler, ast),
      Self::Call(call) => call.compile(compiler, ast),
      Self::Comment(comment) => comment.compile(compiler, ast),
      Self::FormatString(format_string) => format_string.compile(compiler, ast),
      Self::Function(function) => function.compile(compiler, ast),
      Self::Group(group) => group.compile(compiler, ast),
      Self::If(if_) => if_.compile(compiler, ast),
      Self::List(list) => list.compile(compiler, ast),
      Self::Literal(literal) => literal.compile(compiler, ast),
      Self::Match(match_) => match_.compile(compiler, ast),
      Self::ModuleAccess(module_access) => module_access.compile(compiler, ast),
      Self::Unary(unary) => unary.compile(compiler, ast),
      Self::Variable(variable) => variable.compile(compiler, ast),
      Self::Invalid(_) => Err(CompileError::InvalidAST),
    }
  }
}
impl<'s> Compile<'s> for Binary {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    match self.operator(ast) {
      BinaryOperator::And => {
        self.left(ast).compile(compiler, ast)?;
        let jump = compiler.add_jump(OpCode::JumpIfFalse, self.span(ast));
        compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));
        self.right(ast).compile(compiler, ast)?;
        compiler.patch_jump(jump)?;

        return Ok(());
      }
      BinaryOperator::Or => {
        self.left(ast).compile(compiler, ast)?;
        let else_jump = compiler.add_jump(OpCode::JumpIfFalse, self.span(ast));
        let end_jump = compiler.add_jump(OpCode::Jump, self.span(ast));

        compiler.patch_jump(else_jump)?;
        compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));

        self.right(ast).compile(compiler, ast)?;
        compiler.patch_jump(end_jump)?;

        return Ok(());
      }
      BinaryOperator::Pipeline => {
        self.right(ast).compile(compiler, ast)?;
        self.left(ast).compile(compiler, ast)?;
        compiler.chunk.add_opcode(OpCode::Call, self.span(ast));

        return Ok(());
      }
      _ => {}
    }

    self.left(ast).compile(compiler, ast)?;
    self.right(ast).compile(compiler, ast)?;

    compiler.chunk.add_opcode(
      match self.operator(ast) {
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
        BinaryOperator::InvalidSingleEqual => return Err(CompileError::InvalidAST),
      },
      self.span(ast),
    );

    Ok(())
  }
}
impl<'s> Compile<'s> for Block {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    compiler.begin_scope();

    for (id, statement) in self.statements(ast).enumerate() {
      statement.compile(compiler, ast)?;

      if let Statement::Expression(_) = statement
        && id != self.len() - 1
      {
        compiler.chunk.add_opcode(OpCode::Pop, statement.span(ast));
      }
    }

    compiler.end_scope(self.span(ast))
  }
}
impl<'s> Compile<'s> for Call {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.callee(ast).compile(compiler, ast)?;
    if let Some(argument) = self.argument(ast) {
      argument.compile(compiler, ast)?;
    } else {
      compiler.chunk.add_opcode(OpCode::Null, self.span(ast));
    }
    (compiler.chunk).add_opcode(OpCode::Call, self.argument_span(ast));
    Ok(())
  }
}
impl<'s> Compile<'s> for Comment {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.expression(ast).compile(compiler, ast)
  }
}
impl<'s> Compile<'s> for FormatString {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    let mut strings = self.strings_with_spans(ast);

    let (first_string, first_string_span) = strings.next().unwrap();
    compiler.add_constant(first_string, first_string_span)?;

    for (expr, (string, string_span)) in self.expressions(ast).zip(strings) {
      expr.compile(compiler, ast)?;
      compiler.chunk.add_opcode(OpCode::ToString, expr.span(ast));
      compiler.chunk.add_opcode(OpCode::AddString, expr.span(ast));

      compiler.add_constant(string, string_span)?;
      compiler.chunk.add_opcode(OpCode::AddString, string_span);
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Function {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    let name = self.name(ast).unwrap_or("");
    compiler.new_chunk(name);

    compiler.define_variable(self.parameter.name(ast), self.span(ast))?;
    self.body(ast).compile(compiler, ast)?;
    compiler.chunk.add_opcode(OpCode::Return, self.span(ast));

    let function_chunk = compiler.finish_chunk(self.span(ast))?;
    compiler.add_constant(function_chunk, self.span(ast))?;

    let upvalues = compiler.closures.pop().unwrap();
    if !upvalues.is_empty() {
      for (upvalue_index, status) in &upvalues {
        compiler.chunk.add_opcode(
          match status {
            VariableStatus::Open => OpCode::Allocate,
            VariableStatus::Closed => OpCode::GetLocal,
            VariableStatus::Upvalue => OpCode::GetAllocatedPointer,
          },
          self.span(ast),
        );
        compiler.chunk.add_value(*upvalue_index, self.span(ast));
      }

      compiler.chunk.add_opcode(OpCode::Closure, self.span(ast));
      if let Ok(value) = u8::try_from(upvalues.len()) {
        compiler.chunk.add_value(value, self.span(ast));
      } else {
        return Err(CompileError::TooManyClosures);
      }
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Group {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.expression(ast).compile(compiler, ast)
  }
}
impl<'s> Compile<'s> for If {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.condition(ast).compile(compiler, ast)?;
    let jump_to_else = compiler.add_jump(OpCode::JumpIfFalse, self.span(ast));

    compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));
    self.then(ast).compile(compiler, ast)?;
    let jump_to_end = compiler.add_jump(OpCode::Jump, self.span(ast));

    compiler.patch_jump(jump_to_else)?;
    compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));
    if let Some(otherwise) = &self.otherwise(ast) {
      otherwise.compile(compiler, ast)?;
    } else {
      compiler.chunk.add_opcode(OpCode::Null, self.span(ast));
    }

    compiler.patch_jump(jump_to_end)?;

    Ok(())
  }
}
impl<'s> Compile<'s> for List {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    for item in self.items(ast) {
      item.compile(compiler, ast)?;
    }

    compiler.chunk.add_opcode(OpCode::List, self.span(ast));
    if let Ok(length) = u8::try_from(self.length()) {
      compiler.chunk.add_value(length, self.span(ast));
    } else {
      return Err(CompileError::TooManyListItems);
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Literal {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    match self.value(ast) {
      LiteralValue::Boolean(true) => compiler.chunk.add_opcode(OpCode::True, self.span(ast)),
      LiteralValue::Boolean(false) => compiler.chunk.add_opcode(OpCode::False, self.span(ast)),
      LiteralValue::Number(value) => compiler.chunk.add_number(value, self.span(ast)),
      LiteralValue::String(value) => compiler.add_constant(value, self.span(ast))?,
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Match {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    // Calculate the value being matched upon, and store it as a temporary variable
    compiler.begin_scope();
    self.value(ast).compile(compiler, ast)?;
    compiler.define_variable("$MATCH_VALUE$", self.span(ast))?;

    let mut case_end_jumps = Vec::new();
    let mut match_end_jumps = Vec::new();
    for case in self.arms() {
      if let Pattern::Identifier(variable) = &case.pattern
        && case.guard(ast).is_none()
      {
        compiler.chunk.add_opcode(OpCode::Peek, self.span(ast));
        compiler.begin_scope();
        compiler.define_variable(variable.name(ast), variable.span(ast))?;
        case.expression(ast).compile(compiler, ast)?;
        compiler.end_scope(case.span(ast))?;
        break; // optimisation: nothing can match after this
      }

      compiler.begin_scope(); // for any varibles bound in the pattern

      // Compile the pattern and guard if it exists
      // If it doesn't match either, jump to the end of the case
      (case.pattern).compile_pattern(compiler, ast, &mut case_end_jumps)?;
      if let Some(guard) = &case.guard(ast) {
        guard.compile(compiler, ast)?;
        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, self.span(ast)));
        compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));
      }

      // Compile the arm, then remove bound variables, and jump to the end of the match
      case.expression(ast).compile(compiler, ast)?;
      compiler.end_scope(case.span(ast))?;
      match_end_jumps.push(compiler.add_jump(OpCode::Jump, self.span(ast)));

      // Clean-up after any failed patterns
      if case.guard(ast).is_some() {
        let guard_jump = case_end_jumps.pop().unwrap(); // as there is a guard, there must be a jump
        compiler.patch_jump(guard_jump)?;
        // there may be variables defined by the pattern which need to be cleared up
        let bound_vars = case.pattern.bound_vars_count(ast);
        (0..bound_vars).for_each(|_| compiler.chunk.add_opcode(OpCode::Pop, self.span(ast)));
      }
      for jump in case_end_jumps.drain(..) {
        compiler.patch_jump(jump)?;
      }
      compiler.chunk.add_opcode(OpCode::Pop, self.span(ast)); // remove the failed condition value
    }

    // If an arm isn't guaranteed to match, make sure that a value is always returned
    let is_exhaustive = (self.arms())
      .any(|case| case.guard(ast).is_none() && matches!(case.pattern, Pattern::Identifier(_)));
    if !is_exhaustive {
      compiler.chunk.add_opcode(OpCode::Null, self.span(ast));
    }

    // Link the ends of all the cases to the end of the match
    for jump in match_end_jumps {
      compiler.patch_jump(jump)?;
    }

    // Remove the value being matched upon
    compiler.end_scope(self.span(ast))?;

    Ok(())
  }
}
impl<'s> Compile<'s> for ModuleAccess {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    compiler.chunk.add_opcode(OpCode::Import, self.span(ast));
    compiler.add_symbol(self.module(ast), self.span(ast))?;
    compiler.add_symbol(self.item(ast), self.span(ast))
  }
}
impl<'s> Compile<'s> for Unary {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.expression(ast).compile(compiler, ast)?;

    compiler.chunk.add_opcode(
      match self.operator(ast) {
        UnaryOperator::Not => OpCode::Not,
        UnaryOperator::Minus => OpCode::Negate,
      },
      self.span(ast),
    );

    Ok(())
  }
}
impl<'s> Compile<'s> for Variable {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    let span = self.span(ast);

    // See if the variable is local to the current function
    if let Some(local_position) = compiler
      .function_locals()
      .iter()
      .rposition(|local| local.name == self.name(ast))
    {
      match compiler.function_locals()[local_position].status {
        VariableStatus::Open => compiler.chunk.add_opcode(OpCode::GetLocal, span),
        VariableStatus::Closed => compiler.chunk.add_opcode(OpCode::GetAllocatedValue, span),
        VariableStatus::Upvalue => unreachable!("we are only checking the current scope"),
      }
      if let Ok(local_position) = u8::try_from(local_position) {
        compiler.chunk.add_value(local_position, span);
      } else {
        return Err(CompileError::TooManyLocalVariables);
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
          .rposition(|local| local.name == self.name(ast))
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

      compiler.chunk.add_opcode(OpCode::GetUpvalue, span);
      if let Ok(index) = u8::try_from(index) {
        compiler.chunk.add_value(index, span);
      } else {
        return Err(CompileError::TooManyClosures);
      }

      return Ok(());
    }

    // if the variable name matches the current chunk (and the chunk is not the root chunk)
    // then it is a recursive lookup. if the function is named it is a variable, and there is
    // no way to for another value to shadow it before the body of the function.
    if compiler.chunk.name == self.name(ast) && !compiler.chunk_stack.is_empty() {
      compiler.chunk.add_opcode(OpCode::Recursive, span);
      return Ok(());
    }

    // Otherwise, it must be a global variable
    compiler.chunk.add_opcode(OpCode::GetGlobal, span);
    compiler.add_symbol(self.name(ast), span)?;

    Ok(())
  }
}

impl<'s> Compile<'s> for Statement {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    match self {
      Self::Comment(_) => Ok(()),
      Self::Expression(expression) => expression.expression(ast).compile(compiler, ast),
      Self::Import(import) => import.compile(compiler, ast),
      Self::Let(let_) => let_.compile(compiler, ast),
      Self::Return(return_) => return_.compile(compiler, ast),
    }
  }
}
impl<'s> Compile<'s> for Import {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    for item in self.items(ast) {
      compiler.chunk.add_opcode(OpCode::Import, item.span);
      compiler.add_symbol(self.module(ast), item.span)?;
      compiler.add_symbol(item.name, item.span)?;

      if let Some(alias) = &item.alias {
        compiler.define_variable(alias, item.span)?;
      } else {
        compiler.define_variable(item.name, item.span)?;
      }
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Let {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.value(ast).compile(compiler, ast)?;
    compiler.define_variable(self.identifier(ast), self.span(ast))
  }
}
impl<'s> Compile<'s> for Return {
  fn compile(&self, compiler: &mut Compiler<'s>, ast: &'s AST) -> Result<(), CompileError> {
    self.expression(ast).compile(compiler, ast)?;
    compiler.chunk.add_opcode(OpCode::Return, self.span(ast));
    Ok(())
  }
}

trait CompilePattern<'s> {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError>;

  /// How many variables will be bound if this pattern is fully matched?
  fn bound_vars_count(&self, ast: &AST) -> usize;
}

impl<'s> CompilePattern<'s> for Pattern {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    match self {
      Self::Identifier(variable) => variable.compile_pattern(compiler, ast, case_end_jumps),
      Self::Literal(literal) => literal.compile_pattern(compiler, ast, case_end_jumps),
      Self::Range(range) => range.compile_pattern(compiler, ast, case_end_jumps),
      Self::List(list) => list.compile_pattern(compiler, ast, case_end_jumps),
      Self::Option(option) => option.compile_pattern(compiler, ast, case_end_jumps),
      Self::Invalid => Err(CompileError::InvalidAST),
    }
  }

  fn bound_vars_count(&self, ast: &AST) -> usize {
    match self {
      Self::Identifier(variable) => variable.bound_vars_count(ast),
      Self::Literal(literal) => literal.bound_vars_count(ast),
      Self::Range(range) => range.bound_vars_count(ast),
      Self::List(list) => list.bound_vars_count(ast),
      Self::Option(option) => option.bound_vars_count(ast),
      Self::Invalid => unreachable!(),
    }
  }
}
impl<'s> CompilePattern<'s> for Variable {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    _: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    compiler.chunk.add_opcode(OpCode::Peek, self.span(ast));
    compiler.define_variable(self.name(ast), self.span(ast))?;

    Ok(())
  }

  fn bound_vars_count(&self, _: &AST) -> usize {
    1
  }
}
impl<'s> CompilePattern<'s> for Literal {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    compiler.chunk.add_opcode(OpCode::Peek, self.span(ast));
    self.compile(compiler, ast)?;
    compiler.chunk.add_opcode(OpCode::Equals, self.span(ast));

    case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, self.span(ast)));
    compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));

    Ok(())
  }

  fn bound_vars_count(&self, _: &AST) -> usize {
    0
  }
}
impl<'s> CompilePattern<'s> for PatternList {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    let span = self.span(ast);
    #[expect(clippy::cast_precision_loss)]
    let variable_count = self.variables(ast).len() as f64;

    match (variable_count, self.rest(ast)) {
      // Empty List
      (0.0, None) => {
        compiler.chunk.add_opcode(OpCode::ListLength, span);
        compiler.chunk.add_opcode(OpCode::Not, span);

        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
        compiler.chunk.add_opcode(OpCode::Pop, span);
      }
      // No variables, then a rest
      (0.0, Some(rest)) => {
        compiler.chunk.add_opcode(OpCode::ListLength, span);
        compiler.chunk.add_number(0.0, span);
        compiler.chunk.add_opcode(OpCode::GreaterEqual, span);

        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
        compiler.chunk.add_opcode(OpCode::Pop, span);

        (compiler.chunk).add_opcode(OpCode::Peek, span);
        compiler.define_variable(rest.name(ast), span)?;
      }
      // Some number of variables, and then the rest
      (variable_count, Some(rest)) => {
        compiler.chunk.add_opcode(OpCode::ListLength, span);
        compiler.chunk.add_number(variable_count, span);
        compiler.chunk.add_opcode(OpCode::GreaterEqual, span);

        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
        compiler.chunk.add_opcode(OpCode::Pop, span);

        compiler.chunk.add_opcode(OpCode::Peek, span);
        for variable in self.variables(ast) {
          (compiler.chunk).add_opcode(OpCode::ListHeadTail, variable.span(ast));
          compiler.define_variable(variable.name(ast), variable.span(ast))?;
        }
        compiler.define_variable(rest.name(ast), rest.span(ast))?;
      }
      // An exact number of variables
      (variable_count, None) => {
        compiler.chunk.add_opcode(OpCode::ListLength, span);
        compiler.chunk.add_number(variable_count, span);
        compiler.chunk.add_opcode(OpCode::Equals, span);

        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
        compiler.chunk.add_opcode(OpCode::Pop, span);

        compiler.chunk.add_opcode(OpCode::Peek, span);
        for variable in self.variables(ast) {
          (compiler.chunk).add_opcode(OpCode::ListHeadTail, variable.span(ast));
          compiler.define_variable(variable.name(ast), variable.span(ast))?;
        }
        compiler.chunk.add_opcode(OpCode::Pop, span);
      }
    }

    Ok(())
  }

  fn bound_vars_count(&self, ast: &AST) -> usize {
    let variables = self.variables(ast).len();
    let rest = usize::from(self.rest(ast).is_some());

    variables + rest
  }
}
impl<'s> CompilePattern<'s> for PatternRange {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    let span = self.span(ast);

    match (&self.start, &self.end) {
      (Some(pattern), None) => {
        compiler.chunk.add_opcode(OpCode::Peek, span);
        pattern.compile(compiler, ast)?;
        compiler.chunk.add_opcode(OpCode::GreaterEqual, span);
      }
      (None, Some(pattern)) => {
        compiler.chunk.add_opcode(OpCode::Peek, span);
        pattern.compile(compiler, ast)?;
        compiler.chunk.add_opcode(OpCode::LessEqual, span);
      }
      (Some(greater_than), Some(less_than)) => {
        compiler.chunk.add_opcode(OpCode::Peek, span);
        greater_than.compile(compiler, ast)?;
        compiler.chunk.add_opcode(OpCode::GreaterEqual, span);

        case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
        compiler.chunk.add_opcode(OpCode::Pop, span);

        compiler.chunk.add_opcode(OpCode::Peek, span);
        less_than.compile(compiler, ast)?;
        compiler.chunk.add_opcode(OpCode::LessEqual, span);
      }
      (None, None) => return Err(CompileError::InvalidAST),
    }

    case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, span));
    compiler.chunk.add_opcode(OpCode::Pop, span);

    Ok(())
  }

  fn bound_vars_count(&self, _: &AST) -> usize {
    0
  }
}
impl<'s> CompilePattern<'s> for PatternOption {
  fn compile_pattern(
    &self,
    compiler: &mut Compiler<'s>,
    ast: &'s AST,
    case_end_jumps: &mut Vec<Jump>,
  ) -> Result<(), CompileError> {
    if self.kind.is_some() {
      (compiler.chunk).add_opcode(OpCode::OptionIsSome, self.span(ast));
      case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, self.span(ast)));
      compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));

      (compiler.chunk).add_opcode(OpCode::GetAllocatedValue, self.span(ast));
      if let Ok(match_value_location) = u8::try_from(compiler.function_locals().len() - 1) {
        (compiler.chunk).add_value(match_value_location, self.span(ast));
      } else {
        return Err(CompileError::TooManyLocalVariables);
      }
      let variable = self.variable().ok_or(CompileError::InvalidAST)?;
      compiler.define_variable(variable.name(ast), self.span(ast))?;
    }

    if self.kind.is_none() {
      (compiler.chunk).add_opcode(OpCode::OptionIsNone, self.span(ast));
      case_end_jumps.push(compiler.add_jump(OpCode::JumpIfFalse, self.span(ast)));
      compiler.chunk.add_opcode(OpCode::Pop, self.span(ast));
    }

    Ok(())
  }

  fn bound_vars_count(&self, _: &AST) -> usize {
    match self.kind {
      Some(()) => 1,
      None => 0,
    }
  }
}
/// An error from compiling an AST into bytecode
#[derive(Debug, Clone, Copy)]
pub enum CompileError {
  /// Too many constants
  TooManyConstants,
  /// Too many symbols
  TooManySymbols,
  /// Too big jump
  TooBigJump,
  /// Too many closures
  TooManyClosures,
  /// Too many local variables
  TooManyLocalVariables,
  /// Too many items in a list
  TooManyListItems,
  /// AST to compile has an error in it
  InvalidAST,
}
impl CompileError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "Too Many Constants",
      Self::TooManySymbols => "Too Many Symbols",
      Self::TooBigJump => "Too Big Jump",
      Self::TooManyClosures => "Too Many Closures",
      Self::TooManyLocalVariables => "Too Many Local Variables",
      Self::TooManyListItems => "Too Many List Items",
      Self::InvalidAST => "Invalid AST",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "the maximum no. of constants has been reached (65536)",
      Self::TooManySymbols => "the maximum no. of symbols has been reached (256)",
      Self::TooBigJump => "the maximum jump size has been reached (65536)",
      Self::TooManyClosures => "the maximum no. of closures has been reached (256)",
      Self::TooManyLocalVariables => "more than 256 local variables have been defined",
      Self::TooManyListItems => "the maximum no. of items in a list has been reached (256)",
      Self::InvalidAST => "the AST contains an error, see errors from parser",
    }
  }
}
impl fmt::Display for CompileError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}
impl error::Error for CompileError {}
