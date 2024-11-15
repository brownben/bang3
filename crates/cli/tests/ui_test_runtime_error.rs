//! # UI Tests for Runtime Errors
#![cfg(not(miri))]

use assert_cmd::Command;
use indoc::indoc;

fn run(file: &str) -> String {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["run", "-"])
    .write_stdin(file)
    .output()
    .unwrap();

  assert!(!output.status.success());
  String::from_utf8(output.stderr).unwrap()
}

#[test]
fn fibonacci_example() {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["run", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stderr.is_empty());

  let stdout = String::from_utf8(output.stdout).unwrap();
  assert_eq!(stdout, "75025\n75025\n");
}

#[test]
fn undefined_variable() {
  let output = run("a");

  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Undefined Variable
      variable `a` is not defined

          ╭─[STDIN:1]
        1 │ a
      ────╯

      at line 1

    "}
  );
}

#[test]
fn expected_number() {
  let output = run("5 + 'hello'");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected `number`, got `string`

          ╭─[STDIN:1]
        1 │ 5 + 'hello'
      ────╯

      at line 1

    "}
  );

  let output = run("false + 5");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected `number`, got `boolean`

          ╭─[STDIN:1]
        1 │ false + 5
      ────╯

      at line 1

    "}
  );
}

#[test]
fn expected_ordered() {
  let output = run("5 > 'hello'");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected two numbers or two strings, got a `number` and a `string`

          ╭─[STDIN:1]
        1 │ 5 > 'hello'
      ────╯

      at line 1

    "}
  );

  let output = run("false <= 5");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected two numbers or two strings, got a `boolean` and a `number`

          ╭─[STDIN:1]
        1 │ false <= 5
      ────╯

      at line 1

    "}
  );
}

#[test]
fn import_not_found() {
  let output = run("from unknown import { item }");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Module Not Found
      could not find module `unknown`

          ╭─[STDIN:1]
        1 │ from unknown import { item }
      ────╯

      at line 1

    "}
  );

  let output = run("from maths import { unknown }");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Item Not Found
      could not find `unknown` in `maths`

          ╭─[STDIN:1]
        1 │ from maths import { unknown }
      ────╯

      at line 1

    "}
  );
}

#[test]
fn not_callable() {
  let output = run("5()");
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Not Callable
      `number` is not callable, only functions are callable

          ╭─[STDIN:1]
        1 │ 5()
      ────╯

      at line 1

    "}
  );
}

#[test]
fn traceback() {
  let source = indoc! {"
    let outerFunction = _ => {
      let innerFunction = _ => {
        let problem = _ => 5 * false
        problem()
      }
      innerFunction()
    }
    outerFunction()
  "};
  let output = run(source);
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected `number`, got `boolean`

          ╭─[STDIN:3]
        3 │     let problem = _ => 5 * false
      ────╯

      in function 'problem' at line 3
      in function 'innerFunction' at line 4
      in function 'outerFunction' at line 6
      at line 8

    "}
  );

  let source = indoc! {"
    let outerFunction = _ => {
      let innerFunction = _ => _ => 5 * false
      innerFunction
    }
    outerFunction()()()
  "};
  let output = run(source);
  assert_eq!(
    output,
    indoc! {"
      ✕ Error: Type Error
      expected `number`, got `boolean`

          ╭─[STDIN:2]
        2 │   let innerFunction = _ => _ => 5 * false
      ────╯

      in anonymous function at line 2
      at line 5

    "}
  );
}
