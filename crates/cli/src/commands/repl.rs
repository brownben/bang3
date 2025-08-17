use super::{CommandStatus, compile, parse};
use crate::diagnostics::{Message, highlight_source};

use bang_interpreter::{Chunk, ChunkBuilder, OpCode, VM};
use bang_stdlib::StandardContext;
use bang_syntax::{AST, Span, Token, TokenKind, ast, tokenise};

use anstream::{eprintln, println};
use owo_colors::OwoColorize;
use rustyline::highlight::{CmdKind, Highlighter};
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use std::borrow::Cow;

#[derive(rustyline::Helper, rustyline::Completer, rustyline::Hinter)]
struct BangRustyLine;

impl Highlighter for BangRustyLine {
  fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
    &'s self,
    prompt: &'p str,
    _default: bool,
  ) -> Cow<'b, str> {
    prompt.yellow().to_string().into()
  }

  fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
    let mut highlighted_source = String::with_capacity(line.len());
    highlight_source(&mut highlighted_source, line).unwrap();
    highlighted_source.into()
  }

  fn highlight_char(&self, _line: &str, _pos: usize, _update_kind: CmdKind) -> bool {
    true
  }
}

// Check the input for multiline entry
// Assumes is multiline if the brackets are not balanced, or if it ends with an unterminated string
impl Validator for BangRustyLine {
  fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
    let input = ctx.input();
    let tokens = tokenise(input).collect::<Vec<_>>();

    if !brackets_approx_balanced(&tokens) {
      return Ok(ValidationResult::Incomplete);
    }

    if ends_with_unterminated_string(&tokens) {
      return Ok(ValidationResult::Incomplete);
    }

    Ok(ValidationResult::Valid(None))
  }
}

pub fn repl() -> Result<CommandStatus, ()> {
  println!("{}", crate::coloured_header());
  println!("{}", "exit using ctrl+d, or ctrl+c".dimmed());

  let mut vm = match VM::new(&bang_interpreter::Config::default(), &StandardContext) {
    Ok(vm) => vm,
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      return Err(());
    }
  };

  let mut rl = rustyline::Editor::new().unwrap();
  rl.set_helper(Some(BangRustyLine));

  while let Ok(line) = rl.readline(">> ") {
    rl.add_history_entry(line.as_str()).unwrap();

    // if `exit` or `quit` are not defined as variables, but used -> exit the repl
    if (line.trim() == "exit" || line.trim() == "quit") && vm.get_global(line.trim()).is_none() {
      return Ok(CommandStatus::Success);
    }

    _ = run_repl_entry(&mut vm, line);
  }

  Ok(CommandStatus::Success)
}

fn run_repl_entry(vm: &mut VM, line: String) -> Result<(), ()> {
  let ast = parse("REPL", line)?;

  // If it is an expression, print the result, else just compile it.
  let chunk = if let Some(ast::Statement::Expression(expression)) = &ast.root_statements.first() {
    print_expression_result_function(expression.expression(&ast), &ast)?
  } else {
    compile(&ast)?
  };

  if let Err(error) = vm.run(&chunk) {
    eprint!("{}", Message::from(&error));
  }

  Ok(())
}

/// Constructs bytecode which prints the result of the expression.
///
/// Overview:
/// - Get the `print` function from the global scope.
/// - Call the expression function, to get the result (With null as argument).
/// - Call the `print` function. (With the result as the argument).
fn print_expression_result_function(expression: &ast::Expression, ast: &AST) -> Result<Chunk, ()> {
  let expression_function = match bang_interpreter::compile_expression(expression, ast) {
    Ok(chunk) => chunk,
    Err(error) => {
      eprint!("{}", Message::from(&error));
      return Err(());
    }
  };

  let mut function = ChunkBuilder::new("REPL");

  function.add_opcode(OpCode::GetGlobal, Span::default());
  let print_name_id = function.add_symbol("print");
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

/// Are all the brackets in the source balanced?
///
/// This is not a perfect check that the source brackets are balanced,
/// but it makes sure that there are the same number of opening and closing brackets.
fn brackets_approx_balanced(tokens: &[Token]) -> bool {
  let mut bracket_count = 0;

  for token in tokens {
    match token.kind {
      TokenKind::LeftCurly | TokenKind::LeftParen | TokenKind::FormatStringStart => {
        bracket_count += 1;
      }
      TokenKind::RightCurly | TokenKind::RightParen | TokenKind::FormatStringEnd => {
        bracket_count -= 1;
      }
      _ => {}
    }
  }

  bracket_count < 1
}

/// Does the source end with a string that is not terminated?
fn ends_with_unterminated_string(tokens: &[Token]) -> bool {
  let last_token_kind = (tokens.iter().rev())
    .find(|token| token.kind != TokenKind::EndOfLine)
    .map_or(TokenKind::Unknown, |token| token.kind);

  matches!(
    last_token_kind,
    TokenKind::UnterminatedString | TokenKind::FormatStringUnterminated
  )
}
