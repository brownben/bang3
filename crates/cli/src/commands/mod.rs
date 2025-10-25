use super::FormatOptions;
use super::diagnostics::{CodeFrame, Message, Severity};

use bang_formatter::FormatterConfig;
use bang_interpreter::VM;
use bang_lsp::LanguageServer;
use bang_stdlib::StandardContext;
use bang_syntax::{AST, tokenise};

use anstream::{eprintln, println};
use std::fs;

pub enum CommandStatus {
  /// Command was successful, no errors or warnings occurred
  Success,
  /// Command was successful, but warnings occurred
  Failure,
}

fn read_file(filename: &str) -> Result<String, ()> {
  if filename == "-" {
    return read_stdin();
  }

  match fs::read_to_string(filename) {
    Ok(file) if file.is_empty() => {
      eprintln!("{}", Message::warning(format!("Empty file `{filename}`")));
      Err(())
    }
    Ok(file) if file.len() > u32::MAX as usize => {
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

fn read_stdin() -> Result<String, ()> {
  use std::io::{self, Read};

  let mut buffer = Vec::new();
  let mut stdin = io::stdin().lock();

  match stdin.read_to_end(&mut buffer) {
    Ok(_) if buffer.len() > usize::try_from(u32::MAX).unwrap() => {
      eprintln!("{}", Message::error("File too large - max size 4GB".into()));
      Err(())
    }
    Ok(_) => Ok(unsafe { String::from_utf8_unchecked(buffer) }),
    Err(_) => {
      eprintln!("{}", Message::error("Problem Reading from STDIN".into()));
      Err(())
    }
  }
}

fn parse(filename: &str, source: String) -> Result<AST, ()> {
  let ast = bang_syntax::parse(source);

  if ast.is_valid() {
    Ok(ast)
  } else {
    for error in ast.errors {
      eprintln!("{}", Message::from(&error));
      eprintln!("{}", CodeFrame::new(filename, &ast.source, error.span()));
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
  let source = read_file(filename)?;
  let ast = parse(filename, source)?;
  let chunk = compile(&ast)?;

  let mut vm = match VM::new(&bang_interpreter::Config::default(), &StandardContext) {
    Ok(vm) => vm,
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      return Err(());
    }
  };

  if let Err(error) = vm.run(&chunk) {
    eprintln!("{}", Message::from(&error));
    eprintln!("{}", CodeFrame::new(filename, &ast.source, error.span()));

    let line_index = ast.line_index();
    if let Some(traceback) = error.traceback(line_index) {
      eprint!("\n{traceback}");
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
    sort_imports: options.config_sort_imports,
    ..FormatterConfig::default()
  };

  let source = read_file(&options.file)?;
  let ast = parse(&options.file, source)?;
  let formatted_source = bang_formatter::format(&ast, config);

  if options.dryrun {
    println!("{formatted_source}");
    return Ok(CommandStatus::Success);
  }

  if options.check && formatted_source != ast.source {
    eprintln!("{}", Message {
      title: "File is not formatted".into(),
      body: format!("`{}` is not formatted", options.file),
      hint: None,
      severity: Severity::Error,
    });
    return Ok(CommandStatus::Failure);
  }

  if options.file == "-" {
    println!("{formatted_source}");
    return Ok(CommandStatus::Success);
  }

  if formatted_source != ast.source && fs::write(&options.file, formatted_source).is_err() {
    eprintln!("{}", Message::error("Problem writing to file".into()));
    return Err(());
  }

  Ok(CommandStatus::Success)
}

pub fn lint(filename: &str) -> Result<CommandStatus, ()> {
  let source = read_file(filename)?;
  let ast = parse(filename, source)?;
  let diagnostics = bang_linter::lint(&ast);

  for diagnostic in &diagnostics {
    println!("{}", Message::from(diagnostic));
    println!(
      "{}",
      CodeFrame::new(filename, &ast.source, diagnostic.span())
    );
  }

  if diagnostics.is_empty() {
    Ok(CommandStatus::Success)
  } else {
    Ok(CommandStatus::Failure)
  }
}

pub fn typecheck(filename: &str) -> Result<CommandStatus, ()> {
  let source = read_file(filename)?;
  let ast = parse(filename, source)?;
  let typechecker = bang_typechecker::TypeChecker::check(&ast);
  let errors = typechecker.problems();

  for error in errors {
    println!("{}", Message::from(error));
    println!("{}", CodeFrame::new(filename, &ast.source, error.span()));
  }

  if errors.is_empty() {
    Ok(CommandStatus::Success)
  } else {
    Ok(CommandStatus::Failure)
  }
}

pub fn print_tokens(filename: &str) -> Result<CommandStatus, ()> {
  let source = read_file(filename)?;
  let filename = if filename == "-" { "STDIN" } else { filename };

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
  let source = read_file(filename)?;
  let ast = bang_syntax::parse(source);

  if !ast.is_valid() {
    for error in &ast.errors {
      eprintln!("{}", Message::from(error));
      eprintln!("{}", CodeFrame::new(filename, &ast.source, error.span()));
    }
    println!();
  }

  let filename = if filename == "-" { "STDIN" } else { filename };
  println!("╭─[Abstract Syntax Tree: {filename}]");
  print!("{ast}");
  println!("╯");

  Ok(CommandStatus::Success)
}

pub fn print_chunk(filename: &str) -> Result<CommandStatus, ()> {
  let source = read_file(filename)?;
  let ast = parse(filename, source)?;
  let chunk = compile(&ast)?;

  display_chunk(&chunk);

  Ok(CommandStatus::Success)
}

fn display_chunk(chunk: &bang_interpreter::Chunk) {
  println!("{chunk}");

  for sub_chunk in chunk.chunks() {
    display_chunk(sub_chunk);
  }
}

pub fn language_server() -> CommandStatus {
  LanguageServer::new().run();

  CommandStatus::Success
}

mod repl;
pub use repl::repl;
