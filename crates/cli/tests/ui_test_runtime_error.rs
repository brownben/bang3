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

  assert_eq!(output, indoc! {"
    ✕ Error: Undefined Variable
    variable `a` is not defined

        ╭─[STDIN:1]
      1 │ a
    ────╯

    at line 1
  "});
}

#[test]
fn expected_number() {
  let output = run("5 + 'hello'");
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected `number`, got `string`

        ╭─[STDIN:1]
      1 │ 5 + 'hello'
    ────╯

    at line 1
  "});

  let output = run("false + 5");
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected `number`, got `boolean`

        ╭─[STDIN:1]
      1 │ false + 5
    ────╯

    at line 1
  "});
}

#[test]
fn expected_ordered() {
  let output = run("5 > 'hello'");
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected two numbers or two strings, got a `number` and a `string`

        ╭─[STDIN:1]
      1 │ 5 > 'hello'
    ────╯

    at line 1
  "});

  let output = run("false <= 5");
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected two numbers or two strings, got a `boolean` and a `number`

        ╭─[STDIN:1]
      1 │ false <= 5
    ────╯

    at line 1
  "});
}

#[test]
fn import_not_found() {
  let output = run("from unknown import { item }");
  assert_eq!(output, indoc! {"
    ✕ Error: Module Not Found
    could not find module `unknown`

        ╭─[STDIN:1]
      1 │ from unknown import { item }
    ────╯

    at line 1
  "});

  let output = run("from maths import { unknown }");
  assert_eq!(output, indoc! {"
    ✕ Error: Item Not Found
    could not find `unknown` in `maths`

        ╭─[STDIN:1]
      1 │ from maths import { unknown }
    ────╯

    at line 1
  "});
}

#[test]
fn not_callable() {
  let output = run("5()");
  assert_eq!(output, indoc! {"
    ✕ Error: Not Callable
    `number` is not callable, only functions are callable

        ╭─[STDIN:1]
      1 │ 5()
    ────╯

    at line 1
  "});
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
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected `number`, got `boolean`

        ╭─[STDIN:3]
      3 │     let problem = _ => 5 * false
    ────╯

    in function 'problem' at line 3
    in function 'innerFunction' at line 4
    in function 'outerFunction' at line 6
    at line 8
  "});

  let source = indoc! {"
    let outerFunction = _ => {
      let innerFunction = _ => _ => 5 * false
      innerFunction
    }
    outerFunction()()()
  "};
  let output = run(source);
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected `number`, got `boolean`

        ╭─[STDIN:2]
      2 │   let innerFunction = _ => _ => 5 * false
    ────╯

    in anonymous function at line 2
    at line 5
  "});
}

#[test]
fn traceback_native_functions() {
  let source = indoc! {"
    from option import { Some, None, map }

    Some(5) >> map(x => x ++ '')
  "};
  let output = run(source);
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected two strings, got a number and a string

        ╭─[STDIN:3]
      3 │ Some(5) >> map(x => x ++ '')
    ────╯

    in anonymous function at line 3
    in native function 'option::map'
    at line 3
  "});

  let source = indoc! {"
    from option import { None, Some, map }

    let inner = x => x ++ ''
    Some(5) >> map(inner)
  "};
  let output = run(source);
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected two strings, got a number and a string

        ╭─[STDIN:3]
      3 │ let inner = x => x ++ ''
    ────╯

    in function 'inner' at line 3
    in native function 'option::map'
    at line 4
  "});

  let source = indoc! {"
    from option import { None, Some, map }

    let inner = x => x ++ ''
    let mapX = map(inner)

    mapX(Some(5))
  "};
  let output = run(source);
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected two strings, got a number and a string

        ╭─[STDIN:3]
      3 │ let inner = x => x ++ ''
    ────╯

    in function 'inner' at line 3
    in native function 'option::map'
    at line 6
  "});

  let source = indoc! {"
    from option import { None, Some, map }

    map(string::length)(Some(5))
  "};
  let output = run(source);
  assert_eq!(output, indoc! {"
    ✕ Error: Type Error
    expected `string`, got `number`

        ╭─[STDIN:3]
      3 │ map(string::length)(Some(5))
    ────╯

    in native function 'string::length'
    in native function 'option::map'
    at line 3
  "});
}

#[test]
fn panics() {
  let output = run("panic('did something wrong')");
  assert_eq!(output, indoc! {"
    ✕ Error: Panic
    did something wrong

        ╭─[STDIN:1]
      1 │ panic('did something wrong')
    ────╯

    in native function 'panic'
    at line 1
  "});
}

#[test]
fn assertions() {
  let output = run("assert::equal('hello')('world')");
  assert_eq!(output, indoc! {"
    ✕ Error: Assertion Error
    expected both values to be equal — 'hello' [type: string] != 'world' [type: string]

        ╭─[STDIN:1]
      1 │ assert::equal('hello')('world')
    ────╯

    in native function 'assert::equal'
    at line 1
  "});

  let output = run("assert::true('')");
  assert_eq!(output, indoc! {"
    ✕ Error: Assertion Error
    expected a truthy value, but got '' [type: string]

        ╭─[STDIN:1]
      1 │ assert::true('')
    ────╯

    in native function 'assert::true'
    at line 1
  "});

  let output = run("assert::false(17)");
  assert_eq!(output, indoc! {"
    ✕ Error: Assertion Error
    expected a falsy value, but got 17 [type: number]

        ╭─[STDIN:1]
      1 │ assert::false(17)
    ────╯

    in native function 'assert::false'
    at line 1
  "});
}

#[test]
fn stack_overflow() {
  let output = run("let infiniteLoop = x => infiniteLoop(x)\ninfiniteLoop(1)");
  assert_eq!(output, indoc! {"
    ✕ Error: Stack Overflow\nthe stack has overflowed. consider increasing the stack size

        ╭─[STDIN:1]
      1 │ let infiniteLoop = x => infiniteLoop(x)
    ────╯

    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    ... 48 frames hidden
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    in function 'infiniteLoop' at line 1
    at line 2
  "});
}
