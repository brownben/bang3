//! # Formatter
//!
//! A tool to consistently format source code.
//!
//! Works in stages:
//! - Takes the parsed AST and converts it into an intermediate representation ([IR])
//! - Then calculates how much of the intermediate representation it can fit in printWidth
//! - Converts to a second IR which can be directly printed
//!
//! Based upon the algorithm described by Philip Wadler in [`A prettier printer`](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

#![feature(let_chains)]
#![feature(decl_macro)]
#![deny(unsafe_code)]

mod ast;
pub mod config;
mod formatter;

#[cfg(test)]
mod test;

/// Opinionated formatting of an AST into a string.
/// Tries to respect the print width given in the config.
///
/// # Examples
/// ```
/// use bang_formatter::{format, FormatterConfig};
/// use bang_parser::{parse, Allocator};
/// let allocator = Allocator::new();
/// let source = "5 + 3";
/// let ast = parse(source, &allocator);
/// let config = FormatterConfig::default();
/// let formatted = format(&ast, config);
/// ```
#[must_use]
pub fn format(ast: &bang_parser::AST, config: FormatterConfig) -> String {
  let allocator = bang_parser::Allocator::new();
  let formatter = formatter::Formatter::new(config, &allocator);
  formatter.print(ast)
}
pub use config::Config as FormatterConfig;
