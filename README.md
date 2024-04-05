<img src="./logo.svg" height="175px">

# Bang

My attempt at creating my own language. A strongly typed, functional, bytecode interpreter written in Rust. Based on the syntax and style of the languages I have liked using. Complete with a custom opinionated code formatter, linter, type-checker.

Inspired by the awesome [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

Looking for an older version?

- [v2 (a slower Rust interpreter, with slightly different syntax)](https://github.com/brownben/bang2)
- [v1 (a tree walk interpreter in TypeScript)](https://github.com/brownben/bang/releases/tag/JS)

### Examples

```bang
// Recursive Fibonacci

let fibonacci = n => match n
  | ..2 -> 1
  | n -> fibonacci(n - 1) + fibonacci(n - 2)

fibonacci(10)
```

A quick walkthrough of the language can be found [here](/examples/syntax.bang).

More examples can be found in the [/examples](./examples/) folder.

### Usage

```
Usage: bang.exe <COMMAND>

Commands:
  run        Runs a file
  format     Formats source files
  lint       Checks for lint warnings
  typecheck  Checks the file for type errors
  print      Prints debugging information
  help       Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

### Development

```sh
# To format the codebase:
cargo fmt

# To lint the codebase:
cargo clippy

# To run the tests:
cargo test

# To build Bang:
cargo build --release
```

### License

The code in this repository is covered by the [Apache License 2.0](./LICENSE).
