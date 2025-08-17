//! # Generate stdlib docs
//!
//! Generates the standard library documentation, from the documentation for each item of the library.
#![allow(clippy::print_stdout)]

use bang_stdlib::{MODULE_NAMES, MODULES};

use std::fmt::Write;
use std::{env, fs, io, process};

const OUTPUT_FILE: &str = "docs/stdlib_docs.md";

const HEADER: &str =
  "# Bang Standard Library\n\nDocumentation for the Bang Language Standard Library.\n\n";

fn main() -> io::Result<()> {
  let mut file = String::new();

  file.push_str(HEADER);

  file.push_str("**Modules:**\n");
  for module in MODULE_NAMES {
    writeln!(&mut file, "- [{module}](#{module})").unwrap();
  }
  file.push_str("\n\n");

  for module in MODULES {
    write!(&mut file, "## {}\n\n", module.name()).unwrap();

    for item in module.items() {
      let docs = module.docs(item).unwrap();
      let types = module.type_of(item).unwrap();

      let docs = docs
        .lines()
        .filter(|line| line.trim() != "## Example")
        .map(|line| if line.is_empty() { "" } else { &line[1..] })
        .fold(String::new(), |a, b| a + b + "\n");

      write!(&mut file, "### {item} *{types}*\n\n{docs}\n").unwrap();
    }
  }

  // if `--check` is passed as an argument, ensure the existing docs match
  let args: Vec<String> = env::args().collect();
  if let Some(arg) = args.get(1)
    && arg == "--check"
  {
    let existing = fs::read_to_string(OUTPUT_FILE)?;

    if file != existing {
      println!("The standard library documentation is not up-to-date");
      process::exit(1);
    }

    println!("The standard library documentation is up-to-date");
    return Ok(());
  }

  fs::write(OUTPUT_FILE, file)
}
