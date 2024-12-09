//! # Generate stdlib docs
//!
//! Generates the standard library documentation, from the documentation for each item of the library.
#![allow(clippy::print_stdout)]

use bang_interpreter::stdlib::{ITER_ITEMS, LIST_ITEMS, MATHS_ITEMS, OPTION_ITEMS, STRING_ITEMS};
use bang_interpreter::stdlib::{iter_docs, list_docs, maths_docs, option_docs, string_docs};
use bang_interpreter::stdlib::{iter_types, list_types, maths_types, option_types, string_types};

use std::fmt::Write;
use std::{env, fs, io, process};

type GetDocs = fn(&str) -> Option<&'static str>;
type GetType = fn(&str) -> Option<&'static str>;

const MODULES: [(&str, &[&str], GetDocs, GetType); 5] = [
  ("string", &STRING_ITEMS, string_docs, string_types),
  ("maths", &MATHS_ITEMS, maths_docs, maths_types),
  ("list", &LIST_ITEMS, list_docs, list_types),
  ("option", &OPTION_ITEMS, option_docs, option_types),
  ("iter", &ITER_ITEMS, iter_docs, iter_types),
];

const OUTPUT_FILE: &str = "docs/stdlib_docs.md";

const HEADER: &str =
  "# Bang Standard Library\n\nDocumentation for the Bang Language Standard Library.\n\n";

fn main() -> io::Result<()> {
  let mut file = String::new();

  file.push_str(HEADER);

  file.push_str("**Modules:**\n");
  for (module, _, _, _) in MODULES {
    writeln!(&mut file, "- [{module}](#{module})").unwrap();
  }
  file.push_str("\n\n");

  for (module, items, docs, types) in MODULES {
    write!(&mut file, "## {module}\n\n").unwrap();

    for item in items {
      let docs = docs(item).unwrap();
      let types = types(item).unwrap();

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
  if let Some(arg) = args.get(1) {
    if arg == "--check" {
      let existing = fs::read_to_string(OUTPUT_FILE)?;

      if file != existing {
        println!("The standard library documentation is not up-to-date");
        process::exit(1);
      }

      println!("The standard library documentation is up-to-date");
      return Ok(());
    }
  }

  fs::write(OUTPUT_FILE, file)
}
