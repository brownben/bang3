//! # UI Tests for Parse Errors
#![cfg(not(miri))]

use assert_cmd::Command;
use indoc::indoc;

fn run_stderr(file: &str) -> String {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["run", "-"])
    .write_stdin(file)
    .output()
    .unwrap();

  assert!(!output.status.success());
  assert!(output.stdout.is_empty());
  String::from_utf8(output.stderr).unwrap()
}

#[test]
fn expected() {
  let file = indoc! {"
    (5
    3
    from maths { sin }
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Expected )
    expected ) but got Number

        ╭─[STDIN:2]
      2 │ 3
    ────╯
    ✕ Error: Expected import
    expected import but got {

        ╭─[STDIN:3]
      3 │ from maths { sin }
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn expected_expression() {
  let file = indoc! {"
    ()
    + 5
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Expected Expression
    expected expression but got )

        ╭─[STDIN:1]
      1 │ ()
    ────╯
    ✕ Error: Expected Expression
    expected expression but got +

        ╭─[STDIN:2]
      2 │ + 5
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn expected_pattern() {
  let file = indoc! {"
    match x
      | -> 4
      | , -> 2
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Missing Pattern
    expected pattern to match on

        ╭─[STDIN:2]
      2 │   | -> 4
    ────╯
    ✕ Error: Expected Pattern
    expected pattern but got ,

        ╭─[STDIN:3]
      3 │   | , -> 2
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn expected_pattern_range_end() {
  let file = indoc! {"
    match x
      | .. -> 5
      | ..false -> 6
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Expected End of Pattern Range
    expected end of range pattern but got ->

        ╭─[STDIN:2]
      2 │   | .. -> 5
    ────╯
    ✕ Error: Expected End of Pattern Range
    expected end of range pattern but got false

        ╭─[STDIN:3]
      3 │   | ..false -> 6
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn expected_import_item() {
  let file = indoc! {"
    from maths import { 5 }
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Expected Import Item
    expected import item but got Number

        ╭─[STDIN:1]
      1 │ from maths import { 5 }
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn expected_type() {
  let file = "let a: () = 4";
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Expected Type
    expected type but got )

        ╭─[STDIN:1]
      1 │ let a: () = 4
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn unknown_character() {
  let file = indoc! {"
    $
    2 £ 3
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Unknown Character
    got unknown character

        ╭─[STDIN:1]
      1 │ $
    ────╯
    ✕ Error: Unknown Character
    got unknown character

        ╭─[STDIN:2]
      2 │ 2 £ 3
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn missing_identifier() {
  let file = indoc! {"
    let = 5
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Missing Identifier
    expected identifier for variable name

        ╭─[STDIN:1]
      1 │ let = 5
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn missing_module_name() {
  let file = indoc! {"
    from import { sin }
    from 3 import { sin }
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Missing Module Name in Import
    expected module name for import

        ╭─[STDIN:1]
      1 │ from import { sin }
    ────╯
    ✕ Error: Expected Identifier
    expected Identifier but got Number

        ╭─[STDIN:2]
      2 │ from 3 import { sin }
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn unterminated_string() {
  let file = indoc! {"
    'hello
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Unterminated String
    missing closing quote for string

        ╭─[STDIN:1]
      1 │ 'hello
      2 │
    ────╯
  "};
  assert_eq!(output, expected);

  let file = indoc! {"
    `hello {4}
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Unterminated String
    missing closing quote for string

        ╭─[STDIN:1]
      1 │ `hello {4}
      2 │
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn block_end_with_expression() {
  let file = indoc! {"
    {
      let _ = 0
    }
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Block Must End With Expression
    a block must return a value, so must end with an expression rather than a declaration

        ╭─[STDIN:3]
      3 │ }
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn return_outside_function() {
  let file = indoc! {"
    return 5
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Return Outside of Function
    can only return a value from a function

        ╭─[STDIN:1]
      1 │ return 5
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn no_single_equal_operator() {
  let file = indoc! {"
    x = 5
    4 = 5
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: No Single Equal Operator
    a single equal is not an operator. start line with `let` for variable declaration, or use `==` for equality

        ╭─[STDIN:1]
      1 │ x = 5
    ────╯
    ✕ Error: No Single Equal Operator
    a single equal is not an operator. use `==` for equality

        ╭─[STDIN:2]
      2 │ 4 = 5
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn keyword_as_import_item() {
  let file = "from string import { from }";
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Keyword as Import Item
    import items may be keywords, but they must be renamed with `as` or accessed using module access

        ╭─[STDIN:1]
      1 │ from string import { from }
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn extra_arguments() {
  let file = "func(1, 2, 3)";
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Multiple Function Arguments
    function calls only take a single argument
    hint: each argument should be in their own brackets e.g. `a(1, 2)` should be `a(1)(2)`

        ╭─[STDIN:1]
      1 │ func(1, 2, 3)
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn nested_pattern() {
  let file = indoc! {"
    match x
      | Some(0) -> 0
      | Some('hello') -> 1
      | Some(y) -> y
      | Some(..2) -> 2
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Nested Pattern
    nested patterns are not supported yet

        ╭─[STDIN:2]
      2 │   | Some(0) -> 0
    ────╯
    ✕ Error: Nested Pattern
    nested patterns are not supported yet

        ╭─[STDIN:3]
      3 │   | Some('hello') -> 1
    ────╯
    ✕ Error: Nested Pattern
    nested patterns are not supported yet

        ╭─[STDIN:5]
      5 │   | Some(..2) -> 2
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn extra_dot() {
  let file = indoc! {"
    match x
      | ...'a' -> 0
      | 3...'b' -> 1
      | [..._x] -> 2
  "};
  let output = run_stderr(file);
  let expected = indoc! {"
    ✕ Error: Extra Dot
    Patterns only uses two dots (`..`), not three (`...`)

        ╭─[STDIN:2]
      2 │   | ...'a' -> 0
    ────╯
    ✕ Error: Extra Dot
    Patterns only uses two dots (`..`), not three (`...`)

        ╭─[STDIN:3]
      3 │   | 3...'b' -> 1
    ────╯
    ✕ Error: Extra Dot
    Patterns only uses two dots (`..`), not three (`...`)

        ╭─[STDIN:4]
      4 │   | [..._x] -> 2
    ────╯
  "};
  assert_eq!(output, expected);
}
