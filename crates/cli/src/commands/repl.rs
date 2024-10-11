use super::{compile, parse, CommandStatus};
use crate::diagnostics::{CodeFrame, Message};

use bang_interpreter::{Chunk, ChunkBuilder, HeapSize, OpCode, VM};
use bang_parser::{ast, Allocator, GetSpan, Span};

use anstream::{eprintln, println};
use owo_colors::OwoColorize;

pub fn repl() -> Result<CommandStatus, ()> {
  println!("{}", crate::coloured_header());
  println!("{}", "exit using ctrl+d, or ctrl+c".dimmed());

  let mut vm = match VM::new(HeapSize::Standard) {
    Ok(vm) => vm,
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      return Err(());
    }
  };
  vm.define_builtin_functions();

  let mut rl = rustyline::DefaultEditor::new().unwrap();
  while let Ok(line) = rl.readline(">> ") {
    rl.add_history_entry(line.as_str()).unwrap();
    _ = run_repl_entry(&mut vm, &line);
  }

  Ok(CommandStatus::Success)
}

fn run_repl_entry(vm: &mut VM, line: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let ast = parse("REPL", line, &allocator)?;

  // If it is an expression, print the result, else just compile it.
  let chunk = if let Some(ast::Statement::Expression(expression)) = &ast.statements.first() {
    print_expression_result_function(line, expression)?
  } else {
    compile("REPL", line, &ast)?
  };

  if let Err(error) = vm.run(&chunk) {
    eprintln!("{}", Message::from(&error));
  }

  Ok(())
}

/// Constructs bytecode which prints the result of the expression.
///
/// Overview:
/// - Get the `print` function from the global scope.
/// - Call the expression function, to get the result (With null as argument).
/// - Call the `print` function. (With the result as the argument).
fn print_expression_result_function(
  source: &str,
  expression: &ast::Expression,
) -> Result<Chunk, ()> {
  let expression_function = compile_expression(source, expression)?;

  let mut function = ChunkBuilder::new("REPL");

  function.add_opcode(OpCode::GetGlobal, Span::default());
  let print_name_id = function.add_global_name("print");
  function.add_value(print_name_id.try_into().unwrap(), Span::default());

  function.add_opcode(OpCode::Constant, Span::default());
  let expression_function_id = function.add_constant(expression_function);
  function.add_value(expression_function_id.try_into().unwrap(), Span::default());

  function.add_opcode(OpCode::Null, Span::default());
  function.add_opcode(OpCode::Call, Span::default());
  function.add_opcode(OpCode::Call, Span::default());
  function.add_opcode(OpCode::Halt, Span::default());

  Ok(function.finalize())
}

fn compile_expression(
  source: &str,
  expression: &ast::Expression,
) -> Result<bang_interpreter::Chunk, ()> {
  match bang_interpreter::compile_expression(expression) {
    Ok(chunk) => Ok(chunk),
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      if error.span() != Span::default() {
        eprintln!("{}", CodeFrame::new("REPL", source, error.span()));
      }
      Err(())
    }
  }
}
