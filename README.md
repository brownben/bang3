<img src="./logo.svg" height="175px">

# Bang

My attempt at creating my own language. A strongly typed, functional, bytecode interpreter written in Rust. Based on the syntax and style of the languages I have liked using. Complete with a custom opinionated code formatter, linter, type-checker.

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
  lsp        Run the language server
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

### Resources

I have been inspired by/ used the following guides/ projects when creating Bang.

- [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom
- [A prettier printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) by Philip Wadler
- [Ruff](https://github.com/astral-sh/ruff)
- [Oxc Performance Guide](https://oxc-project.github.io/docs/learn/performance.html)
- [Printing Trees in Rust](https://www.georgevreilly.com/blog/2023/01/24/TreeInRust2PrintingTrees.html)
- [Fault Tolerant Parsing](https://chevrotain.io/docs/tutorial/step4_fault_tolerance.html) from Chevrotain
- [Alkyne GC](https://mcyoung.xyz/2022/06/07/alkyne-gc/) by Miguel Young de la Sota

### Looking for an older version?

I am no longer working on previous versions, but you can find them here:

- [v2 (a slower Rust interpreter, with slightly different syntax)](https://github.com/brownben/bang2)
- [v1 (a tree walk interpreter in TypeScript)](https://github.com/brownben/bang/releases/tag/JS)

### License

The code in this repository is covered by the [Apache License 2.0](./LICENSE).
