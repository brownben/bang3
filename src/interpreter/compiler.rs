use super::{
  bytecode::{Chunk, OpCode},
  value::{self, Value},
};
use crate::ast::{expression::*, statement::*, GetSpan, Span, AST};
use std::{error, fmt};

struct Local<'s> {
  name: &'s str,
  depth: u8,
}

pub struct Compiler<'s> {
  chunk: Chunk,

  scope_depth: u8,
  locals: Vec<Local<'s>>,
}
impl<'s> Compiler<'s> {
  pub fn new() -> Self {
    Self {
      chunk: Chunk::new(),

      scope_depth: 0,
      locals: Vec::with_capacity(16),
    }
  }
  pub fn compile(&mut self, ast: &AST<'s, '_>) -> Result<(), CompileError> {
    ast.compile(self)
  }
  pub fn finish(mut self) -> Chunk {
    self.add_opcode(OpCode::Halt);
    self.chunk
  }

  fn add_opcode(&mut self, opcode: OpCode) {
    self.chunk.add_opcode(opcode);
  }
  fn add_value(&mut self, value: u8) {
    self.chunk.add_value(value);
  }
  fn add_long_value(&mut self, value: u16) {
    self.chunk.add_long_value(value);
  }

  fn add_constant(&mut self, value: Value) -> Result<(), CompileError> {
    let constant_position = self.chunk.add_constant(value);

    if let Ok(constant_position) = u8::try_from(constant_position) {
      self.add_opcode(OpCode::Constant);
      self.add_value(constant_position);
    } else if let Ok(constant_position) = u16::try_from(constant_position) {
      self.add_opcode(OpCode::ConstantLong);
      self.add_long_value(constant_position);
    } else {
      Err(CompileError::TooManyConstants)?;
    }

    Ok(())
  }
  fn add_constant_string(&mut self, string: &str) -> Result<(), CompileError> {
    let string_position = self.chunk.add_string(string);

    if let Ok(string_position) = u8::try_from(string_position) {
      self.add_value(string_position);
    } else {
      Err(CompileError::TooManyStrings)?;
    }

    Ok(())
  }

  fn add_jump(&mut self, instruction: OpCode) -> usize {
    self.add_opcode(instruction);
    self.add_long_value(u16::MAX);
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
  fn end_scope(&mut self) {
    let mut count: usize = 0;

    while let Some(last) = self.locals.last()
      && last.depth == self.scope_depth
    {
      self.locals.pop();
      count += 1;
    }

    self.add_opcode(OpCode::PopBelow);
    self.add_value(count as u8);
    self.scope_depth -= 1;
  }
  fn define_variable(&mut self, name: &'s str) -> Result<(), CompileError> {
    if self.scope_depth > 0 {
      self.locals.push(Local {
        name,
        depth: self.scope_depth,
      });
    } else {
      self.add_opcode(OpCode::DefineGlobal);
      self.add_constant_string(name)?;
    }

    Ok(())
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
  /// Block must end with expression
  BlockMustEndWithExpression(Span),
}
impl CompileError {
  /// The title of the error message
  #[must_use]
  pub fn title(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "Too Many Constants",
      Self::TooManyStrings => "Too Many Strings",
      Self::TooBigJump => "Too Big Jump",
      Self::BlockMustEndWithExpression(_) => "Block Must End With Expression",
    }
  }

  /// The body of the error message describing what has gone wrong
  #[must_use]
  pub fn message(&self) -> &'static str {
    match self {
      Self::TooManyConstants => "the maximum number of constants has been reached (65536)",
      Self::TooManyStrings => "the maximum number of strings has been reached (256)",
      Self::TooBigJump => "the maximum jump size has been reached (65536)",
      Self::BlockMustEndWithExpression(_) => {
        "a blocks must return with a value, and so must end with an expression"
      }
    }
  }
}
impl GetSpan for CompileError {
  fn span(&self) -> Span {
    match self {
      Self::BlockMustEndWithExpression(span) => span.clone(),
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

trait Compile<'s> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError>;
}

impl<'s> Compile<'s> for AST<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    for statement in &self.statements {
      statement.compile(compiler)?;

      match statement {
        Statement::Expression(_) => compiler.add_opcode(OpCode::Pop),
        Statement::Comment(_) | Statement::Let(_) => {}
      }
    }

    Ok(())
  }
}

impl<'s> Compile<'s> for Expression<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    match self {
      Expression::Binary(binary) => binary.compile(compiler),
      Expression::Block(block) => block.compile(compiler),
      Expression::Call(call) => call.compile(compiler),
      Expression::Comment(comment) => comment.compile(compiler),
      Expression::Function(function) => function.compile(compiler),
      Expression::Group(group) => group.compile(compiler),
      Expression::If(if_) => if_.compile(compiler),
      Expression::Literal(literal) => literal.compile(compiler),
      Expression::Match(_) => todo!(),
      Expression::Unary(unary) => unary.compile(compiler),
      Expression::Variable(variable) => variable.compile(compiler),
    }
  }
}
impl<'s> Compile<'s> for Binary<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    match self.operator {
      BinaryOperator::And => {
        self.left.compile(compiler)?;
        let jump = compiler.add_jump(OpCode::JumpIfFalse);
        compiler.add_opcode(OpCode::Pop);
        self.right.compile(compiler)?;
        compiler.patch_jump(jump)?;

        return Ok(());
      }
      BinaryOperator::Or => {
        self.left.compile(compiler)?;
        let else_jump = compiler.add_jump(OpCode::JumpIfFalse);
        let end_jump = compiler.add_jump(OpCode::Jump);

        compiler.patch_jump(else_jump)?;
        compiler.add_opcode(OpCode::Pop);

        self.right.compile(compiler)?;
        compiler.patch_jump(end_jump)?;

        return Ok(());
      }
      BinaryOperator::Pipeline => {
        self.right.compile(compiler)?;
        self.left.compile(compiler)?;
        compiler.add_opcode(OpCode::Call);

        return Ok(());
      }
      _ => {}
    }

    self.left.compile(compiler)?;
    self.right.compile(compiler)?;

    compiler.add_opcode(match self.operator {
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
      _ => unreachable!(),
    });

    Ok(())
  }
}
impl<'s> Compile<'s> for Block<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    compiler.begin_scope();

    let (last, rest) = self.statements.split_last().unwrap();
    for statement in rest {
      statement.compile(compiler)?;

      match statement {
        Statement::Expression(_) => compiler.add_opcode(OpCode::Pop),
        Statement::Comment(_) | Statement::Let(_) => {}
      }
    }
    match last {
      Statement::Expression(_) => last.compile(compiler)?,
      stmt => Err(CompileError::BlockMustEndWithExpression(stmt.span()))?,
    }

    compiler.end_scope();

    Ok(())
  }
}
impl<'s> Compile<'s> for Call<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;
    self.argument.compile(compiler)?;
    compiler.add_opcode(OpCode::Call);
    Ok(())
  }
}
impl<'s> Compile<'s> for Comment<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.expression.compile(compiler)
  }
}
impl<'s> Compile<'s> for Function<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    let constant_position = compiler.add_jump(OpCode::ConstantLong);
    let jump_position = compiler.add_jump(OpCode::Jump);

    let start = compiler.chunk.len();
    compiler.begin_scope();
    compiler.define_variable(self.parameter)?;
    self.body.compile(compiler)?;
    compiler.add_opcode(OpCode::Return);
    compiler.end_scope();

    compiler.patch_jump(jump_position)?;

    let name = self.name.unwrap_or("").into();
    let function_value = value::Function { name, start }.into();
    let constant_value = compiler.chunk.add_constant(function_value);

    if let Ok(constant_value) = u16::try_from(constant_value) {
      compiler
        .chunk
        .replace_long_value(constant_position, constant_value);
    } else {
      return Err(CompileError::TooManyConstants);
    }

    Ok(())
  }
}
impl<'s> Compile<'s> for Group<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.expression.compile(compiler)
  }
}
impl<'s> Compile<'s> for If<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.condition.compile(compiler)?;
    let jump_to_else = compiler.add_jump(OpCode::JumpIfFalse);

    compiler.add_opcode(OpCode::Pop);
    self.then.compile(compiler)?;
    let jump_to_end = compiler.add_jump(OpCode::Jump);

    compiler.patch_jump(jump_to_else)?;
    compiler.add_opcode(OpCode::Pop);
    self.otherwise.compile(compiler)?;

    compiler.patch_jump(jump_to_end)?;

    Ok(())
  }
}
impl<'s> Compile<'s> for Literal<'s> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    match self.kind {
      LiteralKind::Boolean(true) => compiler.add_opcode(OpCode::True),
      LiteralKind::Boolean(false) => compiler.add_opcode(OpCode::False),
      LiteralKind::Number { value, .. } => compiler.add_constant(Value::from(value))?,
      LiteralKind::String(value) => compiler.add_constant(Value::from(value))?,
    };

    Ok(())
  }
}
impl<'s> Compile<'s> for Unary<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;

    compiler.add_opcode(match self.operator {
      UnaryOperator::Not => OpCode::Not,
      UnaryOperator::Minus => OpCode::Negate,
    });

    Ok(())
  }
}
impl<'s> Compile<'s> for Variable<'s> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    if let Some(local_position) = compiler
      .locals
      .iter()
      .position(|local| local.name == self.name)
    {
      let local_position = local_position as u8;
      compiler.add_opcode(OpCode::GetLocal);
      compiler.add_value(local_position);
    } else {
      compiler.add_opcode(OpCode::GetGlobal);
      compiler.add_constant_string(self.name)?;
    }

    Ok(())
  }
}

impl<'s> Compile<'s> for Statement<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    match self {
      Statement::Comment(_) => Ok(()),
      Statement::Expression(expression) => expression.compile(compiler),
      Statement::Let(let_) => let_.compile(compiler),
    }
  }
}
impl<'s> Compile<'s> for Let<'s, '_> {
  fn compile(&self, compiler: &mut Compiler<'s>) -> Result<(), CompileError> {
    self.expression.compile(compiler)?;
    compiler.define_variable(self.identifier)
  }
}
