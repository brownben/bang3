//! # UI Tests for Debug Print Commands
#![cfg(not(miri))]

use assert_cmd::Command;
use indoc::indoc;

#[test]
fn tokens_fibonacci_example() {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["print", "tokens", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stderr.is_empty());

  let output = String::from_utf8(output.stdout).unwrap();
  let expected = indoc! {"
        ╭─[Tokens: ../../examples/fibonacci.bang]
      0 │ let
      4 │ Identifier (length: 14)
     19 │ =
     21 │ Identifier (length: 1)
     23 │ =>
     26 │ match
     32 │ Identifier (length: 1)
     33 │ New Line
     36 │ |
     38 │ ..
     40 │ Number (length: 1)
     42 │ ->
     45 │ Number (length: 1)
     46 │ New Line
     49 │ |
     51 │ Identifier (length: 1)
     53 │ ->
     56 │ Identifier (length: 14)
     70 │ (
     71 │ Identifier (length: 1)
     73 │ -
     75 │ Number (length: 1)
     76 │ )
     78 │ +
     80 │ Identifier (length: 14)
     94 │ (
     95 │ Identifier (length: 1)
     97 │ -
     99 │ Number (length: 1)
    100 │ )
    101 │ New Line
    102 │ New Line
    103 │ let
    107 │ Identifier (length: 11)
    119 │ =
    121 │ Identifier (length: 1)
    123 │ =>
    126 │ if
    129 │ (
    130 │ Identifier (length: 1)
    132 │ <=
    135 │ Number (length: 1)
    136 │ )
    138 │ {
    140 │ Number (length: 1)
    142 │ }
    144 │ else
    149 │ {
    150 │ New Line
    153 │ Identifier (length: 11)
    164 │ (
    165 │ Identifier (length: 1)
    167 │ -
    169 │ Number (length: 1)
    170 │ )
    172 │ +
    174 │ Identifier (length: 11)
    185 │ (
    186 │ Identifier (length: 1)
    188 │ -
    190 │ Number (length: 1)
    191 │ )
    192 │ New Line
    193 │ }
    194 │ New Line
    195 │ New Line
    196 │ Identifier (length: 5)
    201 │ (
    202 │ Identifier (length: 11)
    213 │ (
    214 │ Number (length: 2)
    216 │ )
    217 │ )
    218 │ New Line
    219 │ Identifier (length: 5)
    224 │ (
    225 │ Identifier (length: 14)
    239 │ (
    240 │ Number (length: 2)
    242 │ )
    243 │ )
    244 │ New Line
    246 │ End of File
    ────╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn ast_fibonacci_example() {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["print", "ast", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stderr.is_empty());

  let output = String::from_utf8(output.stdout).unwrap();
  let expected = indoc! {"
    ╭─[Abstract Syntax Tree: ../../examples/fibonacci.bang]
    ├─ Let 'fibonacciMatch' =
    │  ╰─ Function (fibonacciMatch): n =>
    │     ╰─ Match
    │        ├─ Variable (n)
    │        ╰─ Cases:
    │           ├─ Pattern ─ .. 2
    │           │  ╰─ Number (1)
    │           ╰─ Pattern ─ n
    │              ╰─ Binary (+)
    │                 ├─ Call
    │                 │  ├─ Callee
    │                 │  │  ╰─ Variable (fibonacciMatch)
    │                 │  ╰─ Argument
    │                 │     ╰─ Binary (-)
    │                 │        ├─ Variable (n)
    │                 │        ╰─ Number (1)
    │                 ╰─ Call
    │                    ├─ Callee
    │                    │  ╰─ Variable (fibonacciMatch)
    │                    ╰─ Argument
    │                       ╰─ Binary (-)
    │                          ├─ Variable (n)
    │                          ╰─ Number (2)
    ├─ Let 'fibonacciIf' =
    │  ╰─ Function (fibonacciIf): n =>
    │     ╰─ If
    │        ├─ Condition
    │        │  ╰─ Binary (<=)
    │        │     ├─ Variable (n)
    │        │     ╰─ Number (2)
    │        ├─ Then
    │        │  ╰─ Block
    │        │     ╰─ Number (1)
    │        ╰─ Otherwise
    │           ╰─ Block
    │              ╰─ Binary (+)
    │                 ├─ Call
    │                 │  ├─ Callee
    │                 │  │  ╰─ Variable (fibonacciIf)
    │                 │  ╰─ Argument
    │                 │     ╰─ Binary (-)
    │                 │        ├─ Variable (n)
    │                 │        ╰─ Number (1)
    │                 ╰─ Call
    │                    ├─ Callee
    │                    │  ╰─ Variable (fibonacciIf)
    │                    ╰─ Argument
    │                       ╰─ Binary (-)
    │                          ├─ Variable (n)
    │                          ╰─ Number (2)
    ├─ Call
    │  ├─ Callee
    │  │  ╰─ Variable (print)
    │  ╰─ Argument
    │     ╰─ Call
    │        ├─ Callee
    │        │  ╰─ Variable (fibonacciIf)
    │        ╰─ Argument
    │           ╰─ Number (25)
    ├─ Call
    │  ├─ Callee
    │  │  ╰─ Variable (print)
    │  ╰─ Argument
    │     ╰─ Call
    │        ├─ Callee
    │        │  ╰─ Variable (fibonacciMatch)
    │        ╰─ Argument
    │           ╰─ Number (25)
    ╯
  "};
  assert_eq!(output, expected);
}

#[test]
fn bytecode_fibonacci_example() {
  let output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
    .unwrap()
    .args(["print", "bytecode", "../../examples/fibonacci.bang"])
    .output()
    .unwrap();

  assert!(output.status.success());
  assert!(output.stderr.is_empty());

  let output = String::from_utf8(output.stdout).unwrap();
  let expected = indoc! {"
         ╭─[Bytecode: main]
    0000 │ Constant <function fibonacciMatch> (0)
    0002 │ DefineGlobal 'fibonacciMatch' (0)
    0004 │ Constant <function fibonacciIf> (1)
    0006 │ DefineGlobal 'fibonacciIf' (1)
    0008 │ GetGlobal 'print' (2)
    0010 │ GetGlobal 'fibonacciIf' (1)
    0012 │ Number 25.0
    0021 │ Call
    0022 │ Call
    0023 │ Pop
    0024 │ GetGlobal 'print' (2)
    0026 │ GetGlobal 'fibonacciMatch' (0)
    0028 │ Number 25.0
    0037 │ Call
    0038 │ Call
    0039 │ Pop
    0040 │ Halt
   ──────╯
  "};
  assert_eq!(output, expected);
}
