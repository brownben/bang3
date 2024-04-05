//! # Bang - My Language
// CLI to access the language, and associated tools like the linter and formatter.

use clap::{Args, Parser, Subcommand};
use std::process;

mod commands;
mod diagnostics;

#[derive(Parser)]
#[clap(name = "bang", version)]
enum App {
  /// Runs a file
  Run {
    /// The file to run
    file: String,
  },

  /// Formats source files
  #[clap(alias = "fmt")]
  Format(FormatOptions),

  /// Checks for lint warnings
  Lint {
    /// The file to lint
    file: String,
  },

  /// Checks the file for type errors
  Typecheck {
    /// The file to typecheck
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
  /// Displays the bytecode
  Bytecode {
    /// The file to print
    file: String,
  },
}

fn main() -> process::ExitCode {
  let args = App::parse();

  let result = match args {
    App::Run { file } => commands::run(&file),
    App::Format(options) => commands::format(&options),
    App::Lint { file } => commands::lint(&file),
    App::Typecheck { file } => commands::typecheck(&file),
    App::Print { command } => match command {
      PrintCommand::Ast { file } => commands::print_ast(&file),
      PrintCommand::Bytecode { file } => commands::print_chunk(&file),
    },
  };

  match result {
    Ok(()) => process::ExitCode::SUCCESS,
    Err(()) => process::ExitCode::FAILURE,
  }
}
