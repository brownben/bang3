//! # Bang - My Language
// CLI to access the language, and associated tools like the linter and formatter.

use clap::{Args, Parser, Subcommand};
use std::process;

#[derive(Parser)]
#[clap(name = "bang", version)]
enum App {
  /// Formats source files
  #[clap(alias = "fmt")]
  Format(FormatOptions),

  /// Checks for lint warnings
  Lint {
    /// The file to lint
    file: String,
  },

  /// Prints debugging information
  Print {
    #[command(subcommand)]
    command: PrintCommand,
  },
}

#[derive(Args)]
struct FormatOptions {
  /// The file to format
  file: String,
  /// Preview the results of the formatting
  #[clap(long)]
  dryrun: bool,
  /// Check the file is formatted
  #[clap(long)]
  check: bool,

  /// Use single quotes. Defaults to true
  #[clap(long, default_value_t = true)]
  config_single_quote: bool,
  /// Maximum line width
  #[clap(long, default_value_t = 80)]
  config_print_width: u16,
  /// Indentation size (spaces) to use. If 0 uses tabs
  #[clap(long, default_value_t = 2)]
  config_indent_size: u8,
}

#[derive(Subcommand)]
enum PrintCommand {
  /// Displays the Abstract Syntax Tree
  Ast {
    /// The file to print
    file: String,
  },
}

fn main() -> process::ExitCode {
  let args = App::parse();

  let result = match args {
    App::Format(options) => commands::format(&options),
    App::Lint { file } => commands::lint(&file),
    App::Print { command } => match command {
      PrintCommand::Ast { file } => commands::print_ast(&file),
    },
  };

  match result {
    Ok(()) => process::ExitCode::SUCCESS,
    Err(()) => process::ExitCode::FAILURE,
  }
}

mod commands {
  use super::helpers::{parse, read_file, CodeFrame, Message};
  use super::FormatOptions;
  use anstream::{eprintln, println};
  use std::fs;

  pub fn format(options: &FormatOptions) -> Result<(), ()> {
    let config = bang::FormatterConfig {
      print_width: options.config_print_width,
      single_quotes: options.config_single_quote,
      indentation: options.config_indent_size,
    };

    let allocator = bang::Allocator::new();
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
    let allocator = bang::Allocator::new();
    let source = read_file(filename)?;
    let ast = parse(filename, &source, &allocator)?;
    let diagnostics = bang::lint(&ast);

    for diagnostic in &diagnostics {
      println!("{}", Message::from(diagnostic));
      println!("{}", CodeFrame::new(filename, &source, diagnostic.span));
    }

    Ok(())
  }

  pub fn print_ast(filename: &str) -> Result<(), ()> {
    let allocator = bang::Allocator::new();
    let source = read_file(filename)?;
    let ast = parse(filename, &source, &allocator)?;

    print!("{ast}");

    Ok(())
  }
}

mod helpers {
  use anstream::eprintln;
  use bang::{
    ast::{GetSpan, LineIndex, Span},
    Allocator, LintDiagnostic, ParseError, AST,
  };
  use owo_colors::OwoColorize;
  use std::{fmt, fs};

  #[derive(Debug)]
  enum Severity {
    Error,
    Warning,
  }
  pub struct Message {
    title: String,
    body: String,
    severity: Severity,
  }
  impl Message {
    pub fn error(message: &str) -> Self {
      Self {
        title: message.to_owned(),
        body: String::new(),
        severity: Severity::Error,
      }
    }
  }
  impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self.severity {
        Severity::Error => write!(f, "{}", "✕ Error".bold().red()),
        Severity::Warning => write!(f, "{}", "⚠ Warning".bold().yellow()),
      }?;
      writeln!(f, "{} {}", ":".bold(), &self.title.bold())?;

      if !self.body.is_empty() {
        writeln!(f, "{}", &self.body)?;
      }

      Ok(())
    }
  }
  impl From<&ParseError> for Message {
    fn from(error: &ParseError) -> Self {
      Self {
        title: error.title(),
        body: error.message(),
        severity: Severity::Error,
      }
    }
  }
  impl From<&LintDiagnostic> for Message {
    fn from(error: &LintDiagnostic) -> Self {
      Self {
        title: error.title.to_owned(),
        body: error.message.to_owned(),
        severity: Severity::Warning,
      }
    }
  }

  pub struct CodeFrame<'a> {
    title: &'a str,
    source: &'a str,
    span: Span,

    lines: LineIndex,
  }
  impl<'a> CodeFrame<'a> {
    pub fn new(title: &'a str, source: &'a str, span: Span) -> Self {
      Self {
        title,
        source,
        span,
        lines: LineIndex::from_source(source),
      }
    }
  }
  impl fmt::Display for CodeFrame<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let start_line = self.lines.get_line(self.span).max(1);
      let end_line = self.lines.get_final_line(self.span);

      writeln!(f, "    ╭─[{}]", self.title)?;

      for line in start_line..=end_line {
        let line_text = self.lines.line_span(line).source_text(self.source);
        write!(f, "{line:>3} │ {line_text}")?;
        if !line_text.ends_with('\n') {
          writeln!(f)?;
        }
      }

      write!(f, "────╯")
    }
  }

  pub fn read_file(filename: &str) -> Result<String, ()> {
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

  pub fn parse<'s, 'a>(
    filename: &str,
    source: &'s str,
    allocator: &'a Allocator,
  ) -> Result<AST<'s, 'a>, ()> {
    match bang::parse(source, allocator) {
      Ok(ast) => Ok(ast),
      Err(error) => {
        eprintln!("{}", Message::from(&error));
        eprintln!("{}", CodeFrame::new(filename, source, error.span()));
        Err(())
      }
    }
  }
}
