use super::diagnostics::{CodeFrame, Message, Severity};
use super::FormatOptions;

use bang_formatter::FormatterConfig;
use bang_interpreter::{HeapSize, VM};
use bang_lsp::LanguageServer;
use bang_parser::{tokenise, Allocator, GetSpan, LineIndex, AST};

use anstream::{eprintln, println};
use std::fs;

pub enum CommandStatus {
  /// Command was successful, no errors or warnings occurred
  Success,
  /// Command was successful, but warnings occurred
  Failure,
}

fn read_file(filename: &str) -> Result<String, ()> {
  match fs::read_to_string(filename) {
    Ok(file) if file.is_empty() => {
      eprintln!("{}", Message::warning(format!("Empty file `{filename}`")));
      Err(())
    }
    Ok(file) if file.bytes().len() > u32::MAX as usize => {
      eprintln!("{}", Message::error("File too large - max size 4GB".into()));
      Err(())
    }
    Ok(file) => Ok(file),
    Err(_) => {
      eprintln!("{}", Message::error(format!("File not found `{filename}`")));
      Err(())
    }
  }
}

fn parse<'s, 'a>(
  filename: &str,
  source: &'s str,
  allocator: &'a Allocator,
) -> Result<AST<'s, 'a>, ()> {
  let ast = bang_parser::parse(source, allocator);

  if ast.is_valid() {
    Ok(ast)
  } else {
    for error in ast.errors {
      eprintln!("{}", Message::from(&error));
      eprintln!("{}", CodeFrame::new(filename, source, error.span()));
    }
    Err(())
  }
}

fn compile(ast: &AST) -> Result<bang_interpreter::Chunk, ()> {
  match bang_interpreter::compile(ast) {
    Ok(chunk) => Ok(chunk),
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      Err(())
    }
  }
}

pub fn run(filename: &str) -> Result<CommandStatus, ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let chunk = compile(&ast)?;

  let mut vm = match VM::new(HeapSize::Standard) {
    Ok(vm) => vm,
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      return Err(());
    }
  };

  vm.define_builtin_functions();

  if let Err(error) = vm.run(&chunk) {
    eprintln!("{}", Message::from(&error));
    eprintln!("{}", CodeFrame::new(filename, &source, error.span()));

    let line_index = LineIndex::from_source(&source);
    if let Some(traceback) = error.traceback(&line_index) {
      eprintln!("\n{traceback}");
    }

    return Err(());
  }

  Ok(CommandStatus::Success)
}

pub fn format(options: &FormatOptions) -> Result<CommandStatus, ()> {
  let config = FormatterConfig {
    print_width: options.config_print_width,
    single_quotes: options.config_single_quote,
    indentation: options.config_indent_size.into(),
    ..FormatterConfig::default()
  };

  let allocator = Allocator::new();
  let source = read_file(&options.file)?;
  let ast = parse(&options.file, &source, &allocator)?;
  let formatted_source = bang_formatter::format(&ast, config);

  if options.dryrun {
    println!("{formatted_source}");
    return Ok(CommandStatus::Success);
  }

  if options.check && formatted_source != source {
    eprintln!(
      "{}",
      Message {
        title: "File is not formatted".into(),
        body: format!("`{}` is not formatted", options.file),
        severity: Severity::Error,
      }
    );
    return Ok(CommandStatus::Failure);
  }

  if formatted_source != source && fs::write(&options.file, formatted_source).is_err() {
    eprintln!("{}", Message::error("Problem writing to file".into()));
    return Err(());
  };

  Ok(CommandStatus::Success)
}

pub fn lint(filename: &str) -> Result<CommandStatus, ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let diagnostics = bang_linter::lint(&ast);

  for diagnostic in &diagnostics {
    println!("{}", Message::from(diagnostic));
    println!("{}", CodeFrame::new(filename, &source, diagnostic.span()));
  }

  if diagnostics.is_empty() {
    Ok(CommandStatus::Success)
  } else {
    Ok(CommandStatus::Failure)
  }
}

pub fn typecheck(filename: &str) -> Result<CommandStatus, ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let errors = bang_typechecker::typecheck(&ast);

  for error in &errors {
    println!("{}", Message::from(error));
    println!("{}", CodeFrame::new(filename, &source, error.span()));
  }

  if errors.is_empty() {
    Ok(CommandStatus::Success)
  } else {
    Ok(CommandStatus::Failure)
  }
}

pub fn print_tokens(filename: &str) -> Result<CommandStatus, ()> {
  let source = read_file(filename)?;

  println!("    ╭─[Tokens: {filename}]");
  for token in tokenise(&source) {
    print!("{:>3} │ {}", token.start, token.kind);
    if !token.kind.has_fixed_length() {
      print!(" (length: {})", token.length);
    }
    println!();
  }
  println!("────╯");

  Ok(CommandStatus::Success)
}

pub fn print_ast(filename: &str) -> Result<CommandStatus, ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;

  println!("╭─[Abstract Syntax Tree: {filename}]");
  print!("{ast}");
  println!("╯");

  Ok(CommandStatus::Success)
}

pub fn print_chunk(filename: &str) -> Result<CommandStatus, ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let chunk = compile(&ast)?;

  print!("{chunk}");

  Ok(CommandStatus::Success)
}

pub fn language_server() -> CommandStatus {
  LanguageServer::new().run();

  CommandStatus::Success
}

mod repl;
pub use repl::repl;
