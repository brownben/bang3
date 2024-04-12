use super::diagnostics::{CodeFrame, Message};
use super::FormatOptions;
use anstream::{eprintln, println};
use bang::{Allocator, GetSpan, Span, AST};
use std::fs;

fn read_file(filename: &str) -> Result<String, ()> {
  match fs::read_to_string(filename) {
    Ok(file) if !file.is_empty() => Ok(file),
    Ok(file) => {
      if file.is_empty() {
        eprintln!("{}", Message::error("File is empty"));
        Err(())
      } else if file.bytes().len() > u32::MAX as usize {
        eprintln!("{}", Message::error("File too large - max size 4GB"));
        Err(())
      } else {
        Ok(file)
      }
    }
    Err(_) => {
      eprintln!("{}", Message::error("File not found"));
      Err(())
    }
  }
}

fn parse<'s, 'a>(
  filename: &str,
  source: &'s str,
  allocator: &'a Allocator,
) -> Result<AST<'s, 'a>, ()> {
  let ast = bang::parse(source, allocator);

  if ast.errors.is_empty() {
    Ok(ast)
  } else {
    for error in ast.errors {
      eprintln!("{}", Message::from(&error));
      eprintln!("{}", CodeFrame::new(filename, source, error.span()));
    }
    Err(())
  }
}

fn compile(
  filename: &str,
  source: &str,
  ast: &AST,
  allocator: &Allocator,
) -> Result<bang::Chunk, ()> {
  match bang::compile(ast, allocator) {
    Ok(chunk) => Ok(chunk),
    Err(error) => {
      eprintln!("{}", Message::from(&error));
      if error.span() != Span::default() {
        eprintln!("{}", CodeFrame::new(filename, source, error.span()));
      }
      Err(())
    }
  }
}

pub fn run(filename: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let chunk = compile(filename, &source, &ast, &allocator)?;

  let mut vm = bang::VM::new();
  if let Err(error) = vm.run(&chunk) {
    eprintln!("{}", Message::from(&error));
    eprintln!("{}", CodeFrame::new(filename, &source, error.span()));
    return Err(());
  }

  Ok(())
}

pub fn format(options: &FormatOptions) -> Result<(), ()> {
  let config = bang::FormatterConfig {
    print_width: options.config_print_width,
    single_quotes: options.config_single_quote,
    indentation: options.config_indent_size,
  };

  let allocator = Allocator::new();
  let source = read_file(&options.file)?;
  let ast = parse(&options.file, &source, &allocator)?;
  let formatted_source = bang::format(&ast, config);

  if options.dryrun {
    println!("{formatted_source}");
    return Ok(());
  }

  if options.check && formatted_source != source {
    eprintln!("{}", Message::error("File is not formatted"));
    Err(())?;
  }

  if formatted_source != source && fs::write(&options.file, formatted_source).is_err() {
    eprintln!("{}", Message::error("Problem writing to file"));
    Err(())?;
  };

  Ok(())
}

pub fn lint(filename: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let diagnostics = bang::lint(&ast);

  for diagnostic in &diagnostics {
    println!("{}", Message::from(diagnostic));
    println!("{}", CodeFrame::new(filename, &source, diagnostic.span));
  }

  Ok(())
}

pub fn typecheck(filename: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let errors = bang::typecheck(&ast);

  for error in &errors {
    println!("{}", Message::from(error));
    println!("{}", CodeFrame::new(filename, &source, error.span()));
  }

  Ok(())
}

pub fn print_ast(filename: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;

  print!("{ast}");

  Ok(())
}

pub fn print_chunk(filename: &str) -> Result<(), ()> {
  let allocator = Allocator::new();
  let source = read_file(filename)?;
  let ast = parse(filename, &source, &allocator)?;
  let chunk = compile(filename, &source, &ast, &allocator)?;

  print!("{chunk}");

  Ok(())
}
