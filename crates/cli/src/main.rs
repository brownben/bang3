//! # Bang - My Language
//! My attempt at creating my own language.
//! A strongly typed, functional, bytecode interpreter written in Rust.
//!
//! Complete with linter, formatter, typechecker, and language server.

mod commands;
mod diagnostics;

use clap::{Args, Parser, Subcommand};
use commands::CommandStatus;
use std::process;

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

  /// Run the language server
  Lsp,
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
  config_indent_size: u16,
}

#[derive(Subcommand)]
enum PrintCommand {
  /// Displays the tokens in the file
  Tokens {
    /// The file to print
    file: String,
  },
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
      PrintCommand::Tokens { file } => commands::print_tokens(&file),
      PrintCommand::Ast { file } => commands::print_ast(&file),
      PrintCommand::Bytecode { file } => commands::print_chunk(&file),
    },
    App::Lsp => Ok(commands::language_server()),
  };

  match result {
    Ok(CommandStatus::Success) => process::ExitCode::from(0),
    Ok(CommandStatus::Failure) => process::ExitCode::from(1),
    Err(()) => process::ExitCode::from(2),
  }
}
