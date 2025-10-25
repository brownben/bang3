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
fn unused_variable() {
  let source = indoc! {"
    let hello = 5
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Unused Variable
    variable `hello` is declared but never used
    hint: if this is intentional prefix with a underscore

        ╭─[STDIN:1]
      1 │ let hello = 5
    ────╯
  "});
}

#[test]
fn unknown_variable_with_suggestion() {
  let source = indoc! {"
    let hello = 5
    print(hello)

    hallo
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Undefined Variable
    no variable defined with the name `hallo`
    hint: a variable with a similar name exists, did you mean `hello`?

        ╭─[STDIN:4]
      4 │ hallo
    ────╯
  "});
}

#[test]
fn unknown_variable_without_suggestion() {
  let source = indoc! {"
    let hello = 5
    print(hello)

    wiggle
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Undefined Variable
    no variable defined with the name `wiggle`

        ╭─[STDIN:4]
      4 │ wiggle
    ────╯
  "});
}

#[test]
fn undefined_variable_possible_imports() {
  let source = indoc! {"
    let _x = sin(3.14)
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Undefined Variable
    no variable defined with the name `sin`
    hint: `sin` can be imported from: maths

        ╭─[STDIN:1]
      1 │ let _x = sin(3.14)
    ────╯
    ✕ Error: Type Not Callable
    expected a function, `unknown` is not callable

        ╭─[STDIN:1]
      1 │ let _x = sin(3.14)
    ────╯
  "});
}

#[test]
fn unknown_module_with_suggestion() {
  let source = indoc! {"
    from math import { sin }
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Module Not Found
    could not find module `math`
    hint: a module with a similar name exists, did you mean `maths`?

        ╭─[STDIN:1]
      1 │ from math import { sin }
    ────╯
  "});
}

#[test]
fn unknown_module_without_suggestion() {
  let source = indoc! {"
    from wiggle import { sin }
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Module Not Found
    could not find module `wiggle`

        ╭─[STDIN:1]
      1 │ from wiggle import { sin }
    ────╯
  "});
}

#[test]
fn unknown_module_item_with_suggestion() {
  let source = indoc! {"
    from maths import { sine }
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Item Not Found
    could not find `sine` in `maths`
    hint: an item with a similar name exists in `maths`, did you mean `sin`?

        ╭─[STDIN:1]
      1 │ from maths import { sine }
    ────╯
  "});
}

#[test]
fn unknown_module_item_without_suggestion() {
  let source = indoc! {"
    from maths import { farty }
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Item Not Found
    could not find `farty` in `maths`

        ╭─[STDIN:1]
      1 │ from maths import { farty }
    ────╯
  "});
}

#[test]
fn unknown_type_annotation_with_suggestion() {
  let source = indoc! {"
    let _x: strings = ''
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Unknown Type Annotation
    type annotation is not a valid type
    hint: a type with a similar name exists, did you mean `string`?

        ╭─[STDIN:1]
      1 │ let _x: strings = ''
    ────╯
  "});
}

#[test]
fn unknown_type_annotation_without_suggestion() {
  let source = indoc! {"
    let _x: farty = ''
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Unknown Type Annotation
    type annotation is not a valid type

        ╭─[STDIN:1]
      1 │ let _x: farty = ''
    ────╯
  "});
}

#[test]
fn type_doesnt_accept_parameter() {
  let source = indoc! {"
    let _x: string<string> = ''
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: Unexpected Parameter
    type `string` does not accept a parameter

        ╭─[STDIN:1]
      1 │ let _x: string<string> = ''
    ────╯
  "});
}

#[test]
fn no_return_from_match_guard() {
  let source = indoc! {"
    let a = 5
    _ => match a
      | ..1 -> false
      | _ if { return a > 5 } -> true
      | _ -> false
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ✕ Error: No Return from Match Guard
    early returns from match guards can cause unexpected execution behaviour

        ╭─[STDIN:4]
      4 │   | _ if { return a > 5 } -> true
    ────╯
  "});
}

#[test]
fn module_access_item_already_imported() {
  let source = indoc! {"
    from string import { length }

    let _x = length('hello')
    'hello' >> string::length >> print
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Module Access Item Already Imported
    `string::length` has been imported as `length`

        ╭─[STDIN:4]
      4 │ 'hello' >> string::length >> print
    ────╯
  "});
}

#[test]
fn referential_equality_warning() {
  let source = indoc! {"
    from string import { byteLength }

    byteLength == string::length
  "};
  let output = run_typecheck(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Referential Equality
    functions and iterators only have referential equality defined
    this can give unexpected behaviour

        ╭─[STDIN:3]
      3 │ byteLength == string::length
    ────╯
  "});
}
