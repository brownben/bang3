//! # Formatter
//!
//! A tool to consistently format source code.
//!
//! Works in stages:
//! - Takes the parsed AST and converts it into an intermediate representation `formatter::IR`
//! - Then calculates how much of the intermediate representation it can fit in printWidth
//! - Converts to a second IR which can be directly printed
//!
//! Based upon the algorithm described by Philip Wadler in [`A prettier printer`](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

#![deny(unsafe_code)]

mod ast;
pub mod config;
mod formatter;

/// Opinionated formatting of an AST into a string.
/// Tries to respect the print width given in the config.
///
/// # Examples
/// ```
/// use bang_formatter::{FormatterConfig, format};
/// use bang_syntax::parse;
///
/// let ast = parse("5 + 3".to_owned());
///
/// let config = FormatterConfig::default();
/// let formatted = format(&ast, config);
///
/// assert_eq!(formatted.trim(), "5 + 3");
/// ```
#[must_use]
pub fn format(ast: &bang_syntax::AST, config: FormatterConfig) -> String {
  let allocator = bumpalo::Bump::new();
  formatter::Formatter::format(ast, config, &allocator)
}

#[doc(inline)]
pub use config::Config as FormatterConfig;
