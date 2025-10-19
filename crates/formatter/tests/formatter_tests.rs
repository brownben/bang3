//! # Formatter Tests
//!
//! Check that the output of the formatter matches the expected output.

use bang_formatter::config::{Config, LineEnding};
use bang_syntax::parse;
use indoc::indoc;

fn format(source: &str, print_width: u16) -> String {
  let config = Config {
    print_width,
    line_ending: LineEnding::LineFeed,
    ..Config::default()
  };
  let ast = parse(source.to_owned());

  bang_formatter::format(&ast, config)
}

macro_rules! assert_format {
  ($source:expr, $expected:expr, $print_width:expr) => {
    let output = format($source, $print_width);
    assert_eq!(output.trim_end(), $expected.trim_end());
    assert_eq!(format(&output, $print_width).trim_end(), output.trim_end());
  };
}

#[test]
fn config_indentation() {
  let ast = parse("(a)".to_owned());

  let mut config = Config {
    print_width: 1,
    single_quotes: true,
    indentation: 2.into(),
    line_ending: LineEnding::LineFeed,
    sort_imports: true,
  };

  assert_eq!(bang_formatter::format(&ast, config), "(\n  a\n)\n");
  config.indentation = 4.into();
  assert_eq!(bang_formatter::format(&ast, config), "(\n    a\n)\n");
  config.indentation = 0.into();
  assert_eq!(bang_formatter::format(&ast, config), "(\n\ta\n)\n");
}

#[test]
fn config_quote() {
  let ast = parse("'string'".to_owned());

  let mut config = Config {
    print_width: 1,
    single_quotes: true,
    indentation: 2.into(),
    line_ending: LineEnding::LineFeed,
    sort_imports: true,
  };

  assert_eq!(bang_formatter::format(&ast, config), "'string'\n");
  config.single_quotes = false;
  assert_eq!(bang_formatter::format(&ast, config), "\"string\"\n");

  assert_format!("\"who's who\"", "\"who's who\"", 100);
}

#[test]
fn config_sort_imports() {
  let ast = parse("from maths import { sin, cos, tan, }".to_owned());
  let mut config = Config {
    print_width: 100,
    single_quotes: true,
    indentation: 2.into(),
    line_ending: LineEnding::LineFeed,
    sort_imports: false,
  };

  assert_eq!(
    bang_formatter::format(&ast, config),
    "from maths import { sin, cos, tan }\n"
  );

  config.sort_imports = true;
  assert_eq!(
    bang_formatter::format(&ast, config),
    "from maths import { cos, sin, tan }\n"
  );
}

#[test]
fn binary() {
  assert_format!("a + b", "a + b", 25);
  assert_format!("a    - b", "a - b", 25);
  assert_format!("a *    b", "a * b", 25);
  assert_format!("a    /     b", "a / b", 25);

  assert_format!("a % b", "a % b", 2);
  assert_format!("a and b && c", "a and b and c", 2);
  assert_format!("a || b or c", "a or b or c", 6);

  assert_format!("a > b", "a > b", 25);
  assert_format!("a    < b", "a < b", 25);
  assert_format!("a >=    b", "a >= b", 25);
  assert_format!("a    <=    b", "a <= b", 25);
}

#[test]
fn binary_pipeline() {
  assert_format!("a >> b", "a >> b", 25);
  assert_format!("a    >> b", "a >> b", 25);
  assert_format!("a >>    b", "a >> b", 25);
  assert_format!("a    >>     b", "a >> b", 25);

  assert_format!("a >> b", "a\n  >> b", 2);
  assert_format!("a >> b >> c", "a\n  >> b\n  >> c", 2);
  assert_format!("a >> b >> c", "a\n  >> b\n  >> c", 6);

  let new_lines = indoc! {"
    input
      >> addOne
      >> addTwo
      >> x => x * 5
      >> addThree
  "};
  let single_line = "input >> addOne >> addTwo >> x => x * 5 >> addThree";
  assert_format!(new_lines, new_lines, 20);
  assert_format!(single_line, new_lines, 20);
  assert_format!(new_lines, single_line, 80);
  assert_format!(single_line, single_line, 80);

  let source = indoc! {"
    input
      >> addOne
      >> x => {
        let a = x + 1
        let b = x + 2
        a / b
      }
      >> print
  "};
  assert_format!(source, source, 20);

  let source = indoc! {"
    iter::integers()
      >> iter::map(x => x * 2)
      >> iter::take(5)
      >> iter::toList // [0, 2, 4, 6, 8]
  "};
  assert_format!(source, source, 80);
}

#[test]
fn block() {
  assert_format!("{ a }", "{ a }", 25);
  assert_format!("{a}", "{ a }", 25);
  assert_format!("{\n  a }", "{ a }", 25);
  assert_format!("{\na\n\n}", "{ a }", 25);

  assert_format!("{ a }", "{\n  a\n}", 2);
  assert_format!("{ a }", "{\n  a\n}", 4);
  assert_format!("{ a }", "{ a }", 6);
  assert_format!("{{ a }}", "{ a }", 6);
  assert_format!("{{ a }}", "{\n  a\n}", 2);

  assert_format!("{ a\n b }", "{\n  a\n  b\n}", 6);
  assert_format!("{let a=false\n a+b}", "{\n  let a = false\n  a + b\n}", 6);
  assert_format!("{{let a=false\n a+b}}", "{\n  let a = false\n  a + b\n}", 6);
  assert_format!(
    "{{let a=false\n\n a+b}}",
    "{\n  let a = false\n  \n  a + b\n}",
    6
  );
  assert_format!("{let a=false}", "{\n  let a = false\n}", 6);
}

#[test]
fn call() {
  assert_format!("function()", "function()", 25);
  assert_format!("function(a)", "function(a)", 25);
  assert_format!("function(a)", "function(\n  a\n)", 8);
  assert_format!("function((a))", "function(a)", 25);
  assert_format!("function(((a)))", "function(a)", 25);
  assert_format!("function(((a)))", "function(\n  a\n)", 8);

  assert_format!("function({ a })", "function(a)", 20);
  assert_format!(
    "function({\na\n// comment\n})",
    "function({\n  a\n  // comment\n})",
    25
  );
  assert_format!("function(((a)))", "function(\n  a\n)", 5);

  let source = indoc! {"
    function({
      let x = 5
      let y = 2
      x + y
    })

    function([
      5555555555555555555555555555555555,
      'some really long string which would cause the line to break for this',
    ])

    module::item(x => y => {
      match x
        | Some(x) -> 1
        | None -> 2
    })
    module::item(x => y => (
      match x
        | Some(x) -> 1
        | None -> 2
    ))
    module::item(x => y => [
      match x
        | Some(x) -> 1
        | None -> 2,
    ])

    module::item(
      x => y => match x
        | Some(x) -> 1
        | None -> 2
    )
    module::item(
      x => y => if (x) y + 55555 else y + 100000000000000000000000000000000
    )
    module::item(function(
      x => y => match x
        | Some(x) -> 1
        | None -> 2
    ))

    iter::something
      >> and_then::do
      >> module::item(x => y => {
        let a = x + y
        let b = x - y
        a / b
      })
      >> something(
        match x
          | Some(x) -> 1
          | None -> 2
      )
  "};
  assert_format!(source, source, 80);
}

#[test]
fn comment() {
  assert_format!("variable // comment", "variable // comment", 100);
  assert_format!("variable //      comment", "variable // comment", 100);
  assert_format!("variable      // comment", "variable // comment", 100);
  assert_format!("variable       //      comment", "variable // comment", 100);
  assert_format!("variable //// comment", "variable // comment", 100);
}

#[test]
fn comment_statement() {
  assert_format!("// comment", "// comment", 100);
  assert_format!("//    comment", "// comment", 100);
  assert_format!("   // comment", "// comment", 100);
  assert_format!("   //    comment", "// comment", 100);

  let multiline_comments = indoc! {"
    // all of these
    // comments
    // form a single block

    // this is a different block
  "};
  assert_format!(multiline_comments, multiline_comments, 100);

  let doc_comments = indoc! {"
    /// all of these
    /// comments
    /// form a single block
    let a = 1

    /// this is a different block
    let function = x => x + 1

    // this is not a doc comment

    let b = 1
    // neither is this
  "};
  assert_format!(doc_comments, doc_comments, 100);

  let doc_comments_broken = indoc! {"
    // all of these
    /// comments
    //// form a single block
    let a = 1

    // this is a different block
    let function = x => x + 1

    /// this is not a doc comment

    let b = 1
    //// neither is this
  "};
  assert_format!(doc_comments_broken, doc_comments, 100);
}

#[test]
fn function() {
  assert_format!("a => b", "a => b", 25);
  assert_format!("a =>    b", "a => b", 25);
  assert_format!("a    => b", "a => b", 25);
  assert_format!("a => (b)", "a => (\n  b\n)", 5);
}

#[test]
fn group() {
  assert_format!("(a)", "(a)", 25);
  assert_format!("(a   )", "(a)", 25);
  assert_format!("(   a)", "(a)", 25);
  assert_format!("(   a   )", "(a)", 25);
  assert_format!("(a)", "(\n  a\n)", 1);
  assert_format!("((a))", "(a)", 25);
  assert_format!("(((a)))", "(a)", 25);
}

#[test]
fn group_with_forced_line() {
  assert_format!("({ a\n b })", "({\n  a\n  b\n})", 6);
  assert_format!("(({ a\n b }))", "({\n  a\n  b\n})", 6);
  assert_format!(
    "({let a=false\n a+b})",
    "({\n  let a = false\n  a + b\n})",
    6
  );
  assert_format!("(\n  '' + ({\n      a\n\n    })\n)", "('' + ({ a }))", 80);

  let immediately_called_function = indoc! {"
    let b = (_ => {
      let x = if (false) { 5 } else { 6 }
      x + 4
    })()
  "};
  assert_format!(immediately_called_function, immediately_called_function, 80);
}

#[test]
fn if_() {
  assert_format!("if (true) false else 7", "if (true) false else 7", 25);
  assert_format!("if   (true) false else 7", "if (true) false else 7", 25);
  assert_format!("if (  true) false else 7", "if (true) false else 7", 25);
  assert_format!("if (true  ) false else 7", "if (true) false else 7", 25);
  assert_format!("if (true)   false else 7", "if (true) false else 7", 25);
  assert_format!("if   ( true )  false  else 7", "if (true) false else 7", 25);
  assert_format!(
    "if (true !=false) 6 else 7",
    "if (true != false) 6 else 7",
    25
  );
  assert_format!("if ((true)) false else 7", "if (true) false else 7", 25);
  assert_format!("if (({(true)})) false else 7", "if (true) false else 7", 25);

  let source = indoc! {"
    let _z = if (a) b else {
      aReallyReallyLongMethod()
    }
  "};
  assert_format!(source, source, 30);

  let expected = indoc! {"
    let function = _ => {
      if (a) {
        let a = 4
        a
      } else 4
    }
  "};
  assert_format!(expected, expected, 80);

  assert_format!("if (true) false", "if (true) false", 25);
}

#[test]
fn literal() {
  // booleans
  assert_format!("true", "true", 25);
  assert_format!("false", "false", 25);

  // strings
  assert_format!("'string with a space'", "'string with a space'", 1);
  assert_format!("\"string\"", "'string'", 1);

  // simple numbers
  assert_format!("7", "7", 25);
  assert_format!("1_000", "1_000", 25);
  assert_format!("7.0", "7.0", 25);
  assert_format!("3.14", "3.14", 25);

  // add zeros to the front of decimal points
  assert_format!(".14", "0.14", 25);
  assert_format!(".140000", "0.14", 25);

  // trim leading/ trailing zeros
  assert_format!("0", "0", 25);
  assert_format!("0.0", "0.0", 25);
  assert_format!("000.0", "0.0", 25);
  assert_format!("000.000", "0.0", 25);
  assert_format!("0.000", "0.0", 25);
  assert_format!("0.01400", "0.014", 25);
  assert_format!("0.14", "0.14", 25);
  assert_format!("00.14", "0.14", 25);
  assert_format!("003.14", "3.14", 25);
  assert_format!("003.140000", "3.14", 25);

  // trim leading/ trailing zeros with negative numbers
  assert_format!("-0", "-0", 25);
  assert_format!("-0.0", "-0.0", 25);
  assert_format!("-000.0", "-0.0", 25);
  assert_format!("-000.000", "-0.0", 25);
  assert_format!("-0.000", "-0.0", 25);
  assert_format!("-0.01400", "-0.014", 25);
  assert_format!("-0.14", "-0.14", 25);
  assert_format!("-00.14", "-0.14", 25);
  assert_format!("-003.14", "-3.14", 25);
  assert_format!("-003.140000", "-3.14", 25);

  // if there are separators, just bail out
  assert_format!("0001_000", "0001_000", 25);
  assert_format!("0001_000.000", "0001_000.000", 25);
  assert_format!("1_000.000_000", "1_000.000_000", 25);
  assert_format!("000_1_000.000_000", "000_1_000.000_000", 25);
}

#[test]
fn let_statement() {
  let expected_output = "let variable = stuff";
  assert_format!("let variable = stuff", expected_output, 25);
  assert_format!("let    variable = stuff", expected_output, 25);
  assert_format!("let variable     = stuff", expected_output, 25);
  assert_format!("let variable =     stuff", expected_output, 25);
  assert_format!("let    variable   =   stuff", expected_output, 25);

  assert_format!(
    "let variable = (long_long_long_long_stuff)",
    "let variable = (long_long_long_long_stuff)",
    100
  );
  assert_format!(
    "let variable = (long_long_long_long_stuff)",
    "let variable = (\n  long_long_long_long_stuff\n)",
    20
  );
  assert_format!(
    "let variable = (long_long_long_long_stuff) // comment",
    "let variable = (\n  long_long_long_long_stuff\n) // comment",
    20
  );
}

#[test]
fn match_() {
  assert_format!(
    "match n | ''.. -> a | b -> ''",
    "match n\n  | ''.. -> a\n  | b -> ''",
    100
  );
  assert_format!(
    "match    n   | ''  ..   ->   a   | b     -> ''",
    "match n\n  | ''.. -> a\n  | b -> ''",
    100
  );
  assert_format!(
    "(match n | ''.. -> a | b -> '')",
    "(\n  match n\n    | ''.. -> a\n    | b -> ''\n)",
    100
  );
  assert_format!(
    "(match n | ''.. if x > 5 -> a | b -> '')",
    "(\n  match n\n    | ''.. if x > 5 -> a\n    | b -> ''\n)",
    100
  );
  assert_format!(
    "match n | ..1 -> a |1..2 -> b | 2.. -> c",
    "match n\n  | ..1 -> a\n  | 1..2 -> b\n  | 2.. -> c",
    100
  );
  assert_format!(
    "match n | 1 -> a | 2.25 -> b | 'hello' -> c",
    "match n\n  | 1 -> a\n  | 2.25 -> b\n  | 'hello' -> c",
    100
  );

  let match_lists = indoc! {"
    match x
      | [] -> 0
      | [x] -> 1
      | [variable] -> 1
      | [x, ..xs] -> {
        let hello = 3 + x
        length(xs) * hello
      }
      | [..whole] -> maths::NAN
      | [x, y] -> 77
      | [x, y, x, ..alphabet] -> None
  "};
  let match_lists_misformatted = indoc! {"
    match x
      | [ ] -> 0
    | [  x] -> 1
      | [variable,] -> 1
          |    [x  ,..  xs ]   -> {
        let hello = 3  + x
        length(xs ) * hello
      }
      |  [..whole   ]          -> maths::NAN
            | [x  , y   ,] ->   77
      | [x  ,    y,    x, .. alphabet]   -> None
  "};
  assert_format!(match_lists, match_lists, 80);
  assert_format!(match_lists_misformatted, match_lists, 80);

  let match_options = indoc! {"
    match x
      | None -> 0
      | Some(x) -> 1
  "};
  let match_options_misformatted = indoc! {"
    match x
      |   None -> 0
      | Some(  x ) -> 1
  "};
  assert_format!(match_options, match_options, 80);
  assert_format!(match_options_misformatted, match_options, 80);

  // extra dot in pattern is removed
  assert_format!("match n | 1... -> a", "match n\n  | 1.. -> a", 100);
}

#[test]
fn match_and_comment() {
  let source = indoc! {"
    let fibonnacciMatch = n => match n
      | x -> 1

    // comment
    print(fibonnacciMatch(25))
    print(fibonnaciIf(25))
  "};

  assert_format!(source, source, 80);
}

#[test]
fn unary() {
  assert_format!("!true", "!true", 25);
  assert_format!("!!true", "!!true", 25);
  assert_format!("!!!true", "!true", 25);
  assert_format!("!!!!true", "!!true", 25);
  assert_format!("!!!!!true", "!true", 25);

  assert_format!("!    false", "!false", 25);

  assert_format!("- 4", "-4", 25);
  assert_format!(" --3", "--3", 25);
  assert_format!("---89", "-89", 25);
}

#[test]
fn expression_break_line() {
  let source = indoc! {"
    print(
      worlddddddddddddddddddd
    ) + print(5)
  "};
  assert_format!(source, source, 25);

  let source = indoc! {"
    print(hello) + type(
      world
    ) - (5)
  "};
  assert_format!(source, source, 25);

  let source = indoc! {"
    print(hello) + type(
      world
    ) - function(5)
  "};
  assert_format!(source, source, 25);
}

#[test]
fn format_string() {
  assert_format!("`hello {7}`", "`hello {7}`", 50);
  assert_format!("`hello {   7 }`", "`hello {7}`", 50);
  assert_format!("`hello {3 + 1} world`", "`hello {3 + 1} world`", 50);
  assert_format!("`{7} world'`", "`{7} world'`", 50);
  assert_format!(
    "`hello {7} world {false}!`",
    "`hello {7} world {false}!`",
    50
  );
}

#[test]
fn comments_in_groups() {
  let group = indoc! {"
    (
      5 // hello world
    )
  "};
  assert_format!(group, group, 80);

  let call = indoc! {"
    print(
      5 // hello world
    )
  "};
  assert_format!(call, call, 80);

  let block = indoc! {"
    {
      5 // hello world
    }
  "};
  assert_format!(block, block, 80);

  let if_ = indoc! {"
    if (
      5 // hello world
    ) 4 else 7
  "};
  assert_format!(if_, if_, 80);
}

#[test]
fn return_statement() {
  let code = indoc! {"
    let function = argument => {
      let a = if (x > 5) { return argument } else { 0 }
      a + 1
    }

    let other = a => {
      let a = 5
      return a
      // comment
    }

    let function = argument => {
      let a = if (x > 5) {
        return argument // a
      } else { 0 }
      a + 1
    }
  "};
  assert_format!(code, code, 80);
}

#[test]
fn import_statement() {
  let code = indoc! {"
    from maths import { cos, sin, tan }
    from string import { split }

    from maths import { abs as absolute, cbrt as cubeRoot, sqrt }
  "};
  assert_format!(code, code, 80);

  assert_format!(
    "from maths   import {  sin   }",
    "from maths import { sin }",
    50
  );

  assert_format!(
    "from maths import { sin }",
    "from maths import {\n  sin,\n}",
    10
  );
  assert_format!(
    "from maths   import   { cos, sin,    tan, }",
    "from maths import {\n  cos,\n  sin,\n  tan,\n}",
    10
  );

  assert_format!(
    "from maths import { abs as absolute, cbrt as cubeRoot, sqrt }",
    "from maths import {\n  abs as absolute,\n  cbrt as cubeRoot,\n  sqrt,\n}",
    10
  );

  assert_format!(
    "from maths import { sqrt, abs as absolute, cbrt as cubeRoot }",
    "from maths import {\n  abs as absolute,\n  cbrt as cubeRoot,\n  sqrt,\n}",
    10
  );

  assert_format!("from maths import {}", "from maths import {  }", 80);
  assert_format!(
    "from maths import { abs as abs }",
    "from maths import { abs }",
    80
  );
}

#[test]
fn no_leading_space_before_first_statement() {
  let before = "\n\nlet a = 5\nlet b = 6";
  let after = "let a = 5\nlet b = 6";
  assert_format!(before, after, 80);

  let before = "\n\nlet a = 5\n\n\nlet b = 6";
  let after = "let a = 5\n\nlet b = 6";
  assert_format!(before, after, 80);

  let before = "{\n\n\n\nlet a = 5\nlet b = 6}";
  let after = "{\n  let a = 5\n  let b = 6\n}";
  assert_format!(before, after, 80);

  let before = "{\n\n\n\nlet a = 5\n\n\nlet b = 6}";
  let after = "{\n  let a = 5\n  \n  let b = 6\n}";
  assert_format!(before, after, 80);
}

#[test]
fn module_access() {
  let code = indoc! {"
    from maths import { PI }

    let a = maths::sin(PI)
    print(`
    {string::TAB}{a}
    `)
  "};
  assert_format!(code, code, 80);

  assert_format!("maths  :: sin ", "maths::sin", 80);
}

#[test]
fn lists() {
  // Leaves basic correct lists alone
  assert_format!("[1, 2, 3]", "[1, 2, 3]", 80);
  assert_format!("[]", "[]", 80);

  // Corrects spacing
  assert_format!("[  ]", "[]", 80);
  assert_format!("[   1  , 2  , 3 ]", "[1, 2, 3]", 80);

  // Removes trailing comma
  assert_format!("[1, 2, 3,]", "[1, 2, 3]", 80);

  // Splits if too long for line
  assert_format!("[1, 2, 3]", "[\n  1,\n  2,\n  3,\n]", 5);
}

#[test]
fn pipeline_with_long_function() {
  let code = indoc! {"
    input
      >> someReallyLongFunctionNameThatExceedsTheLineLimit('hello world this is long')
      >> anotherLongFunctionNameThatAlsoExceedsTheLineLimit(10)
      >> finalFunction()
  "};
  assert_format!(code, code, 80);

  let code = indoc! {"
    let b = list::iter([1, 2, 3])
      >> iter::filterMap(x => if (x % 2 == 1) Some(x + 1) else None)
      >> iter::sum
  "};
  assert_format!(code, code, 80);
}

mod types {
  use super::*;

  #[test]
  fn primitives() {
    assert_format!("let a: boolean = true", "let a: boolean = true", 80);
    assert_format!("let a: string = 'hello'", "let a: string = 'hello'", 80);
    assert_format!("let a:number = 3.324", "let a: number = 3.324", 80);
    assert_format!("let a    :unknown = 3.324", "let a: unknown = 3.324", 80);
    assert_format!(
      "let a      :         never =       3.324",
      "let a: never = 3.324",
      80
    );
  }

  #[test]
  fn type_variable() {
    assert_format!("let a: ^a = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a:     ^a = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a:    ^    a = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a  :   ^  a  = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a  :^a = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let hello: ^did = 5", "let hello: ^did = 5", 80);
  }

  #[test]
  fn functions() {
    assert_format!(
      "let a: number => number = a => a",
      "let a: number => number = a => a",
      80
    );
    assert_format!(
      "let a: number     =>number = a => a",
      "let a: number => number = a => a",
      80
    );
    assert_format!(
      "let a: number=>   number = a => a",
      "let a: number => number = a => a",
      80
    );
    assert_format!(
      "let a:number    =>    number=a=>a",
      "let a: number => number = a => a",
      80
    );
  }

  #[test]
  fn groups() {
    assert_format!("let a: (^a) = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a: ((^a)) = 'hello'", "let a: ^a = 'hello'", 80);
    assert_format!("let a: (number) = 'hello'", "let a: number = 'hello'", 80);
    assert_format!("let a: ((number)) = 'hello'", "let a: number = 'hello'", 80);

    assert_format!(
      "let a: (number => number) = a => a",
      "let a: (number => number) = a => a",
      80
    );
    assert_format!(
      "let a: ((number => number)) = a => a",
      "let a: (number => number) = a => a",
      80
    );
  }

  #[test]
  fn structures() {
    assert_format!("let a: list<string> = []", "let a: list<string> = []", 80);
    assert_format!(
      "let a: list  <   string  > = []",
      "let a: list<string> = []",
      80
    );
  }
}
