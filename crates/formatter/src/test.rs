use indoc::indoc;

use super::config::{Config, LineEnding};
use super::formatter::Formatter;
use bang_parser::{parse, Allocator};

fn format(source: &str, print_width: u16) -> String {
  let config = Config {
    print_width,
    line_ending: LineEnding::LineFeed,
    ..Config::default()
  };
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);

  crate::format(&ast, config)
}

macro assert_format($source:expr, $expected:expr, $print_width:expr) {
  let output = format($source, $print_width);
  assert_eq!(output.trim(), $expected.trim());
  assert_eq!(format(&output, $print_width).trim(), output.trim());
}

#[test]
fn config_indentation() {
  let allocator = Allocator::new();
  let ast = parse("(a)", &allocator);
  let mut formatter = Formatter::new(Config::default(), &allocator);

  formatter.config.line_ending = LineEnding::LineFeed;
  formatter.config.print_width = 1;
  formatter.config.indentation = 2.into();
  assert_eq!(formatter.print(&ast), "(\n  a\n)\n");
  formatter.config.indentation = 4.into();
  assert_eq!(formatter.print(&ast), "(\n    a\n)\n");
  formatter.config.indentation = 0.into();
  assert_eq!(formatter.print(&ast), "(\n\ta\n)\n");
}

#[test]
fn config_quote() {
  let allocator = Allocator::new();
  let ast = parse("'string'", &allocator);
  let mut formatter = Formatter::new(Config::default(), &allocator);

  formatter.config.line_ending = LineEnding::LineFeed;
  formatter.config.single_quotes = true;
  assert_eq!(formatter.print(&ast), "'string'\n");
  formatter.config.single_quotes = false;
  assert_eq!(formatter.print(&ast), "\"string\"\n");

  assert_format!("\"who's who\"", "\"who's who\"", 100);
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
  assert_format!("a >> b >> c", "a >> b\n  >> c", 6);
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
}

#[test]
fn comment() {
  assert_format!("variable // comment", "variable // comment", 100);
  assert_format!("variable //      comment", "variable // comment", 100);
  assert_format!("variable      // comment", "variable // comment", 100);
  assert_format!("variable       //      comment", "variable // comment", 100);
}

#[test]
fn comment_statement() {
  assert_format!("// comment", "// comment", 100);
  assert_format!("//    comment", "// comment", 100);
  assert_format!("   // comment", "// comment", 100);
  assert_format!("   //    comment", "// comment", 100);
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
}

#[test]
fn literal() {
  assert_format!("true", "true", 25);
  assert_format!("false", "false", 25);
  assert_format!("'string with a space'", "'string with a space'", 1);
  assert_format!("\"string\"", "'string'", 1);
  assert_format!("7", "7", 25);
  assert_format!("1_000", "1_000", 25);
  assert_format!("7.0", "7.0", 25);
  assert_format!("3.14", "3.14", 25);
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
