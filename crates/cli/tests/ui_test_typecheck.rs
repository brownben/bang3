//! # UI Tests for Typecheck Errors
#![cfg(not(miri))]

use assert_cmd::Command;
use indoc::indoc;

fn run_typecheck(file: &str) -> String {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["typecheck", "-"])
    .write_stdin(file)
    .output()
    .unwrap();

  assert!(!output.status.success());
  String::from_utf8(output.stdout).unwrap()
}

#[test]
fn fibonacci_example() {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["typecheck", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stdout.is_empty());
  assert!(output.stderr.is_empty());
}

#[test]
fn unknown_variable_with_suggestion() {
  let source = indoc! {"
    let hello = 5
    print(hello)

    hallo
  "};
  let output = run_typecheck(source);

  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Undefined Variable
      no variable defined with the name `hallo`
      hint: a variable with a similar name exists, did you mean `hello`?

          ╭─[STDIN:4]
        4 │ hallo
      ────╯
    "}
  );
}

#[test]
fn unknown_variable_without_suggestion() {
  let source = indoc! {"
    let hello = 5
    print(hello)

    wiggle
  "};
  let output = run_typecheck(source);

  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Undefined Variable
      no variable defined with the name `wiggle`

          ╭─[STDIN:4]
        4 │ wiggle
      ────╯
    "}
  );
}

#[test]
fn unknown_module_with_suggestion() {
  let source = indoc! {"
    from math import { sin }
  "};
  let output = run_typecheck(source);

  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Module Not Found
      could not find module `math`
      hint: a module with a similar name exists, did you mean `maths`?

          ╭─[STDIN:1]
        1 │ from math import { sin }
      ────╯
    "}
  );
}

#[test]
fn unknown_module_without_suggestion() {
  let source = indoc! {"
    from wiggle import { sin }
  "};
  let output = run_typecheck(source);

  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Module Not Found
      could not find module `wiggle`

          ╭─[STDIN:1]
        1 │ from wiggle import { sin }
      ────╯
    "}
  );
}
