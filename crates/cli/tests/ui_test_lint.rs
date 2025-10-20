//! # UI Tests for Lints
#![cfg(not(miri))]

use assert_cmd::Command;
use indoc::indoc;

fn run_lint(file: &str) -> String {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["lint", "-"])
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
    .args(["lint", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stdout.is_empty());
  assert!(output.stderr.is_empty());
}

#[test]
fn constant_conditions() {
  let source = indoc! {"
    if (true) 4 else 5
    match 5
      | 0 -> 4
      | _ -> 5
    let x = 1
    match x
      | _ if true -> 4
      | _ -> 5
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Constant Condition
    the control flow could be removed, as the condition is always the same

        ╭─[STDIN:1]
      1 │ if (true) 4 else 5
    ────╯
    ⚠ Warning: Constant Condition
    the control flow could be removed, as the condition is always the same

        ╭─[STDIN:2]
      2 │ match 5
    ────╯
    ⚠ Warning: Constant Condition
    the control flow could be removed, as the condition is always the same

        ╭─[STDIN:7]
      7 │   | _ if true -> 4
    ────╯
  "});
}

#[test]
fn negative_zero() {
  let source = "-0";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Negative Zero
    negative zero is unnecessary, as 0 == -0

        ╭─[STDIN:1]
      1 │ -0
    ────╯
  "});
}

#[test]
fn self_assignment() {
  let source = indoc! {"
    let a = 5
    let a = a
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Self Assignment
    assigning a variable to itself is unnecessary

        ╭─[STDIN:2]
      2 │ let a = a
    ────╯
  "});
}

#[test]
fn self_comparison() {
  let source = "5 == 5";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Self Comparison
    comparing a value to itself is unnecessary

        ╭─[STDIN:1]
      1 │ 5 == 5
    ────╯
  "});
}

#[test]
fn underscore_variable_use() {
  let source = indoc! {"
    let _ = 5
    _
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Underscore Variable Use
    a `_` prefix indicates the variable/ parameter is unused, but it has been used

        ╭─[STDIN:2]
      2 │ _
    ────╯
  "});
}

#[test]
fn useless_match() {
  let source = indoc! {"
    match x
      | _ -> 5
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Useless Match
    the first case matches everything, therefore the match is useless

        ╭─[STDIN:1]
      1 │ match x
      2 │   | _ -> 5
    ────╯
  "});
}

#[test]
fn yoda_equality() {
  let source = indoc! {"
    let x = 4
    5 == x
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Yoda Equality
    it is clearer to have the variable first then the value to compare to

        ╭─[STDIN:2]
      2 │ 5 == x
    ────╯
  "});
}

#[test]
fn unnecessary_closure() {
  let source = "x => print(x)";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Unnecessary Closure
    the inner function could be used directly, without being wrapped in another function

        ╭─[STDIN:1]
      1 │ x => print(x)
    ────╯
  "});
}

#[test]
fn identical_branches() {
  let source = indoc! {"
    if (a) 4 else 4
    if (a) 4 else {
      // comment
      4
    }
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Identical Branches
    both branches of the if-else are the same, consider removing the if

        ╭─[STDIN:1]
      1 │ if (a) 4 else 4
    ────╯
    ⚠ Warning: Identical Branches
    both branches of the if-else are the same, consider removing the if

        ╭─[STDIN:2]
      2 │ if (a) 4 else {
      3 │   // comment
      4 │   4
      5 │ }
    ────╯
  "});
}

#[test]
fn erasing_operation() {
  let source = indoc! {"
    0 * 4
    0 / 8
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Erasing Operation
    this operation always returns 0

        ╭─[STDIN:1]
      1 │ 0 * 4
    ────╯
    ⚠ Warning: Erasing Operation
    this operation always returns 0

        ╭─[STDIN:2]
      2 │ 0 / 8
    ────╯
  "});
}

#[test]
fn constant_string_in_format_string() {
  let source = "`hello {'john'}`";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Constant String in Format String
    the constant can be combined with the rest of the string

        ╭─[STDIN:1]
      1 │ `hello {'john'}`
    ────╯
  "});
}

#[test]
fn unnecessary_return() {
  let source = "x => { return x }";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Unnecessary Return
    the return statement is unnecessary, as a block will return the last expression

        ╭─[STDIN:1]
      1 │ x => { return x }
    ────╯
  "});
}

#[test]
fn unreachable_code() {
  let source = indoc! {"
    a => {
      return a + 5

      a + 4
    }
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Unreachable Code
    code after a return will never be run

        ╭─[STDIN:4]
      4 │   a + 4
      5 │ }
    ────╯
  "});
}

#[test]
fn double_comparison_chain() {
  let source = indoc! {"
    let a = 0
    let b = 1
    a == b or (a > b)
  "};
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Double Comparison Chain
    the expression can be simplified into a single condition
    `a == b or a > b` can be simplified to `a >= b`

        ╭─[STDIN:3]
      3 │ a == b or (a > b)
    ────╯
  "});
}

#[test]
fn empty_imports() {
  let source = "from maths import {  }";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Empty Imports
    the import statement imports nothing, it can be removed

        ╭─[STDIN:1]
      1 │ from maths import {  }
    ────╯
  "});
}

#[test]
fn loss_of_precision() {
  let source = "let x = {\n  1234567890123456789\n}";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Loss of Precision
    numbers are stored as double-precision floating-point numbers according to the IEEE 754 standard
    when the literal is converted to a number, precision will be lost and the value may not be what was intended

        ╭─[STDIN:2]
      2 │   1234567890123456789
    ────╯
  "});
}

#[test]
fn subtraction_zero_comparison() {
  let source = "a - b < 0";
  let output = run_lint(source);

  assert_eq!(output, indoc! {"
    ⚠ Warning: Subtraction Zero Comparison
    a comparison between two numbers is clearer than a subtraction compared to zero
    e.g. `x - y == 0` can be written as `x == y`

        ╭─[STDIN:1]
      1 │ a - b < 0
    ────╯
  "});
}
