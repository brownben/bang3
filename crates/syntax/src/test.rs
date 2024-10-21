use crate::{ast::AST, parse};
use indoc::indoc;

impl AST<'_> {
  fn is_ok(&self) -> bool {
    self.errors.is_empty()
  }

  fn is_err(&self) -> bool {
    !self.is_ok()
  }
}

fn parse_to_string(source: &str) -> String {
  parse(source).to_string()
}

#[test]
fn space_at_end() {
  assert!(parse("22 + 44 ").is_ok());
  assert!(parse("22 + 44    ").is_ok());
  assert!(parse("22 + 44  \t  ").is_ok());
}

#[test]
fn empty_string() {
  assert!(parse("\n\n\n").is_ok());
  assert!(parse("    ").is_ok());
  assert!(parse("").is_ok());
  assert!(parse("  \n    \n   \n ").is_ok());
}

#[test]
fn unterminated_string() {
  assert!(parse("'unterminated string").is_err());
  assert!(parse("\"un").is_err());
  assert!(parse("`").is_err());

  assert!(parse("``").is_ok());
  assert!(parse("`hello world`").is_ok());
}

#[test]
fn unknown_character() {
  assert!(parse("¬").is_err());
  assert!(parse("3 $ 4").is_err());
  assert!(parse("3 ¬ 4").is_err());
  assert!(parse("🤗").is_err());
  assert!(parse(".-2").is_err());
  assert!(parse("&").is_err());

  // Having unknown characters in strings are fine
  assert!(parse("'¬'").is_ok());
  assert!(parse("'🤗'").is_ok());
}

#[test]
fn binary() {
  let ast = parse_to_string("4 + 23");
  let expected = indoc! {"
    ├─ Binary (+)
    │  ├─ Number (4)
    │  ╰─ Number (23)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("'hello ' ++ 'world'");
  let expected = indoc! {"
    ├─ Binary (++)
    │  ├─ String 'hello '
    │  ╰─ String 'world'
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("4 * 56 >> 32");
  let expected = indoc! {"
    ├─ Binary (>>)
    │  ├─ Binary (*)
    │  │  ├─ Number (4)
    │  │  ╰─ Number (56)
    │  ╰─ Number (32)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("false or 6.12");
  let expected = indoc! {"
    ├─ Binary (or)
    │  ├─ Boolean (false)
    │  ╰─ Number (6.12)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("5 % 2 == 1 && 3 / 1");
  let expected = indoc! {"
    ├─ Binary (and)
    │  ├─ Binary (==)
    │  │  ├─ Binary (%)
    │  │  │  ├─ Number (5)
    │  │  │  ╰─ Number (2)
    │  │  ╰─ Number (1)
    │  ╰─ Binary (/)
    │     ├─ Number (3)
    │     ╰─ Number (1)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn binary_precedence() {
  let ast = parse_to_string("1 + 2 * 3");
  let expected = indoc! {"
    ├─ Binary (+)
    │  ├─ Number (1)
    │  ╰─ Binary (*)
    │     ├─ Number (2)
    │     ╰─ Number (3)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("5 + 3 * 2");
  let expected = indoc! {"
    ├─ Binary (+)
    │  ├─ Number (5)
    │  ╰─ Binary (*)
    │     ├─ Number (3)
    │     ╰─ Number (2)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn binary_missing_left_expression() {
  assert!(parse("+ 5").is_err());
  assert!(parse("*").is_err());
  assert!(parse(" || 7").is_err());
}

#[test]
fn binary_missing_right_expression() {
  assert!(parse("4 +").is_err());
  assert!(parse("'hello' *").is_err());
  assert!(parse("false ||").is_err());
}

#[test]
fn blocks() {
  let ast = parse_to_string("{false}");
  let expected = indoc! {"
    ├─ Block
    │  ╰─ Boolean (false)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string(indoc! {"{
    let x = 4
    x + 2
  }"});
  let expected = indoc! {"
    ├─ Block
    │  ├─ Let 'x' =
    │  │  ╰─ Number (4)
    │  ╰─ Binary (+)
    │     ├─ Variable (x)
    │     ╰─ Number (2)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("{call(true)}");
  let expected = indoc! {"
    ├─ Block
    │  ╰─ Call
    │     ├─ Callee
    │     │  ╰─ Variable (call)
    │     ╰─ Argument
    │        ╰─ Boolean (true)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn blocks_incompelete() {
  assert!(parse("{let x = 4\nx + 2").is_err());
  assert!(parse("{}").is_err());
  assert!(parse("{7").is_err());

  // must end in expression
  assert!(parse("{let a = 1}").is_err());
  assert!(parse("{let a = 1\n// comment\n}").is_err());
  assert!(parse("{1\n// comment\n}").is_ok());
}

#[test]
fn call() {
  let ast = parse_to_string("function('hello world')");
  let expected = indoc! {"
    ├─ Call
    │  ├─ Callee
    │  │  ╰─ Variable (function)
    │  ╰─ Argument
    │     ╰─ String 'hello world'
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("f ( 7 ) + 5");
  let expected = indoc! {"
    ├─ Binary (+)
    │  ├─ Call
    │  │  ├─ Callee
    │  │  │  ╰─ Variable (f)
    │  │  ╰─ Argument
    │  │     ╰─ Number (7)
    │  ╰─ Number (5)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("f(x) + 32.1");
  let expected = indoc! {"
    ├─ Binary (+)
    │  ├─ Call
    │  │  ├─ Callee
    │  │  │  ╰─ Variable (f)
    │  │  ╰─ Argument
    │  │     ╰─ Variable (x)
    │  ╰─ Number (32.1)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn call_missing_brackets_or_expression() {
  assert!(parse("f(").is_err());
  assert!(parse("f(6").is_err());

  assert!(parse("f()").is_ok());
  assert!(parse("f ()").is_ok());
  assert!(parse("f(\n)").is_ok());

  assert!(parse("f(\n'hello')").is_ok());
  assert!(parse("f('hello'\n)").is_ok());
  assert!(parse("f(\n'hello'\n)").is_ok());
}

#[test]
fn comment() {
  let ast = parse_to_string("5 // hello world");
  let expected = indoc! {"
    ├─ Comment (hello world)
    │  ╰─ Number (5)
  "};
  assert_eq!(ast, expected);

  let comment_in_call = parse_to_string(indoc! {"
    print(
      5 // hello world
    )
  "});
  let expected = indoc! {"
    ├─ Call
    │  ├─ Callee
    │  │  ╰─ Variable (print)
    │  ╰─ Argument
    │     ╰─ Comment (hello world)
    │        ╰─ Number (5)
  "};
  assert_eq!(comment_in_call, expected);
}

#[test]
fn function() {
  let ast = parse_to_string("x => x + 1");
  let expected = indoc! {"
    ├─ Function: x =>
    │  ╰─ Binary (+)
    │     ├─ Variable (x)
    │     ╰─ Number (1)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("let plusOne = a => { a + 1 }");
  let expected = indoc! {"
    ├─ Let 'plusOne' =
    │  ╰─ Function (plusOne): a =>
    │     ╰─ Block
    │        ╰─ Binary (+)
    │           ├─ Variable (a)
    │           ╰─ Number (1)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn group() {
  let ast = parse_to_string("('hello world')");
  let expected = indoc! {"
    ├─ Group
    │  ╰─ String 'hello world'
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("(4)");
  let expected = indoc! {"
    ├─ Group
    │  ╰─ Number (4)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn if_() {
  let ast = parse_to_string("if (true) 7 else 5");
  let expected = indoc! {"
    ├─ If
    │  ├─ Condition
    │  │  ╰─ Boolean (true)
    │  ├─ Then
    │  │  ╰─ Number (7)
    │  ╰─ Otherwise
    │     ╰─ Number (5)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("if (7 > x) doStuff else false");
  let expected = indoc! {"
    ├─ If
    │  ├─ Condition
    │  │  ╰─ Binary (>)
    │  │     ├─ Number (7)
    │  │     ╰─ Variable (x)
    │  ├─ Then
    │  │  ╰─ Variable (doStuff)
    │  ╰─ Otherwise
    │     ╰─ Boolean (false)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("if (true) false");
  let expected = indoc! {"
    ├─ If
    │  ├─ Condition
    │  │  ╰─ Boolean (true)
    │  ╰─ Then
    │     ╰─ Boolean (false)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn if_missing_parts() {
  assert!(parse("if 'hello')").is_err());
  assert!(parse("if'hello')").is_err());
  assert!(parse("if ('hello')").is_err());
  assert!(parse("if('hello')").is_err());
  assert!(parse("if ('hello'").is_err());
  assert!(parse("if('hello') then else").is_err());

  assert!(parse("if ('hello') then else false").is_ok());
  assert!(parse("if('hello') then else true").is_ok());
  assert!(parse("if(\n'hello') then else x").is_ok());
  assert!(parse("if('hello'\n) then else stuff").is_ok());
  assert!(parse("if(\n'hello'\n) then else otherwise").is_ok());

  // no else branch
  assert!(parse("if ('hello') then").is_ok());
  assert!(parse("if('hello') then").is_ok());
  assert!(parse("if(\n'hello') then").is_ok());
}

#[test]
fn group_no_end_bracket() {
  assert!(parse("(4 + 5 ").is_err());
  assert!(parse("('hello'").is_err());
  assert!(parse("(").is_err());

  assert!(parse("('hello')").is_ok());
  assert!(parse("(5)").is_ok());
}

#[test]
fn literal() {
  let ast = parse_to_string("false");
  let expected = "├─ Boolean (false)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("true");
  let expected = "├─ Boolean (true)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("5.6");
  let expected = "├─ Number (5.6)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("5_000.03");
  let expected = "├─ Number (5000.03)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("'string'");
  let expected = "├─ String 'string'\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("'string\nnew\nlines'");
  let expected = "├─ String 'string\nnew\nlines'\n";
  assert_eq!(ast, expected);
}

#[test]
fn match_() {
  let ast = parse_to_string("match n | 1 -> 0");
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (n)
    │  ╰─ Cases:
    │     ╰─ Pattern ─ 1
    │        ╰─ Number (0)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string(indoc! {"
      match n
        | 1 -> 0
        | 2 -> 1
        | n -> n * 5
  "});
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (n)
    │  ╰─ Cases:
    │     ├─ Pattern ─ 1
    │     │  ╰─ Number (0)
    │     ├─ Pattern ─ 2
    │     │  ╰─ Number (1)
    │     ╰─ Pattern ─ n
    │        ╰─ Binary (*)
    │           ├─ Variable (n)
    │           ╰─ Number (5)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string(indoc! {"
    match x
      | 0..1 -> 1
      | n -> n * 5
  "});
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (x)
    │  ╰─ Cases:
    │     ├─ Pattern ─ 0 .. 1
    │     │  ╰─ Number (1)
    │     ╰─ Pattern ─ n
    │        ╰─ Binary (*)
    │           ├─ Variable (n)
    │           ╰─ Number (5)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("match x | 0 -> 1\n// comment");
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (x)
    │  ╰─ Cases:
    │     ╰─ Pattern ─ 0
    │        ╰─ Number (1)
    ├─ Comment (comment)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn match_with_guard() {
  let ast = parse_to_string("match n | 1 if x -> 0");
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (n)
    │  ╰─ Cases:
    │     ╰─ Pattern ─ 1
    │        ├─ Guard
    │        │  ╰─ Variable (x)
    │        ╰─ Number (0)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("match n | 1 if x -> 0 | 2 if y -> 1");
  let expected = indoc! {"
    ├─ Match
    │  ├─ Variable (n)
    │  ╰─ Cases:
    │     ├─ Pattern ─ 1
    │     │  ├─ Guard
    │     │  │  ╰─ Variable (x)
    │     │  ╰─ Number (0)
    │     ╰─ Pattern ─ 2
    │        ├─ Guard
    │        │  ╰─ Variable (y)
    │        ╰─ Number (1)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn match_missing_parts() {
  assert!(parse("match | 1 -> 2").is_err());
  assert!(parse("match 3 | -5").is_err());
  assert!(parse("match 3 1 -> 3").is_err());
  assert!(parse("match 3 | 1 -> 3, 2 -> 4").is_err());
  assert!(parse("match 3 | 1 -> 3\n 2 -> 4").is_err());
  assert!(parse("match 3 | 1,").is_err());
  assert!(parse("match true | .. -> 2").is_err());

  assert!(parse("match true | 1 -> 2").is_ok());
  assert!(parse("match 4 + 5 | ..9 -> 2").is_ok());
  assert!(parse("match 4 + 5 | ..'z' -> 2").is_ok());
  assert!(parse("match char | 'a'..'z' -> true | _ -> false").is_ok());
  assert!(parse("match false | 1.. -> 2").is_ok());
}

#[test]
fn unary() {
  let ast = parse_to_string("!false");
  let expected = indoc! {"
    ├─ Unary (!)
    │  ╰─ Boolean (false)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("-.5");
  let expected = indoc! {"
    ├─ Unary (-)
    │  ╰─ Number (0.5)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn variable() {
  let ast = parse_to_string("a_long_variable");
  let expected = "├─ Variable (a_long_variable)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("aLongVariable");
  let expected = "├─ Variable (aLongVariable)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("count");
  let expected = "├─ Variable (count)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("leaf");
  let expected = "├─ Variable (leaf)\n";
  assert_eq!(ast, expected);

  let ast = parse_to_string("igloo");
  let expected = "├─ Variable (igloo)\n";
  assert_eq!(ast, expected);
}

#[test]
fn comment_statement() {
  let ast = parse_to_string("// comments");
  let expected = "├─ Comment (comments)\n";
  assert_eq!(ast, expected);
}

#[test]
fn let_statement() {
  let ast = parse_to_string("let variable = 4 + 33");
  let expected = indoc! {"
    ├─ Let 'variable' =
    │  ╰─ Binary (+)
    │     ├─ Number (4)
    │     ╰─ Number (33)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn let_statement_missing_parts() {
  assert!(parse("let = 7").is_err());
  assert!(parse("let var").is_err());
  assert!(parse("let var =").is_err());
  assert!(parse("let var 7").is_err());
  assert!(parse("let false = 7").is_err());

  assert!(parse("let var = 7").is_ok());
}

#[test]
fn pipeline_can_break_lines() {
  let ast = parse_to_string("5 \n    >> add3");
  let expected = indoc! {"
    ├─ Binary (>>)
    │  ├─ Number (5)
    │  ╰─ Variable (add3)
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("let x = 5 \n    >> add3\n>>times2");
  let expected = indoc! {"
    ├─ Let 'x' =
    │  ╰─ Binary (>>)
    │     ├─ Binary (>>)
    │     │  ├─ Number (5)
    │     │  ╰─ Variable (add3)
    │     ╰─ Variable (times2)
  "};
  assert_eq!(ast, expected);
}

#[test]
fn format_string() {
  let skip_no_format = parse_to_string("`standard string`");
  let expected = indoc! {"
    ├─ String 'standard string'
  "};
  assert_eq!(skip_no_format, expected);

  let standard_string = parse_to_string("'standard {string}}'");
  let expected = indoc! {"
    ├─ String 'standard {string}}'
  "};
  assert_eq!(standard_string, expected);

  let one_field = parse_to_string("`hello {5} people`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ 'hello '
    │  ├─ Number (5)
    │  ╰─ ' people'
  "};
  assert_eq!(one_field, expected);

  let no_front_padding = parse_to_string("`{  5 } people`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ ''
    │  ├─ Number (5)
    │  ╰─ ' people'
  "};
  assert_eq!(no_front_padding, expected);

  let no_end_padding = parse_to_string("`hello {5  }`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ 'hello '
    │  ├─ Number (5)
    │  ╰─ ''
  "};
  assert_eq!(no_end_padding, expected);

  let with_group = parse_to_string("`hello {(5)}`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ 'hello '
    │  ├─ Group
    │  │  ╰─ Number (5)
    │  ╰─ ''
  "};
  assert_eq!(with_group, expected);

  let with_block = parse_to_string("`hello {{5}}`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ 'hello '
    │  ├─ Block
    │  │  ╰─ Number (5)
    │  ╰─ ''
  "};
  assert_eq!(with_block, expected);

  let two_fields = parse_to_string("`hello {5} + {1} people`");
  let expected = indoc! {"
    ├─ Format String
    │  ├─ 'hello '
    │  ├─ Number (5)
    │  ├─ ' + '
    │  ├─ Number (1)
    │  ╰─ ' people'
  "};
  assert_eq!(two_fields, expected);

  let unterminated = parse("`hello {5} + {1} peop");
  assert!(unterminated.is_err());

  let no_expression = parse("`hello {}`");
  assert!(no_expression.is_err());

  let missing_end_curly = parse("`hello {5 `");
  assert!(missing_end_curly.is_err());
}

#[test]
fn return_statement() {
  let ast = parse_to_string("_ => { return 4 }");
  let expected = indoc! {"
    ├─ Function: _ =>
    │  ╰─ Block
    │     ╰─ Return
    │        ╰─ Number (4)
  "};
  assert_eq!(ast, expected);

  // outside of a function
  assert!(parse("return 5").is_err());
  assert!(parse("{ return 5 }").is_err());

  // no return value
  assert!(parse("_ => { return }").is_err());
  assert!(parse("_ => { return & }").is_err());
}

#[test]
fn import_statement() {
  let ast = parse_to_string("from maths import { sin, cos, tan }");
  let expected = indoc! {"
    ├─ From 'maths' Import
    │  ├─ sin
    │  ├─ cos
    │  ╰─ tan
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("from maths import {\n sin,\n cos,\n tan,\n}");
  let expected = indoc! {"
    ├─ From 'maths' Import
    │  ├─ sin
    │  ├─ cos
    │  ╰─ tan
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("from strings import { split }");
  let expected = indoc! {"
    ├─ From 'strings' Import
    │  ╰─ split
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("from strings import { TAB as tab }");
  let expected = indoc! {"
    ├─ From 'strings' Import
    │  ╰─ TAB as tab
  "};
  assert_eq!(ast, expected);

  let ast = parse_to_string("from strings import {  }");
  let expected = indoc! {"
    ├─ From 'strings' Import
  "};
  assert_eq!(ast, expected);

  assert!(parse("from 4 import { x }").is_err());
  assert!(parse("from import { x }").is_err());
  assert!(parse("import x").is_err());
  assert!(parse("from maths { sin, cos }").is_err());

  // Block can't end with import
  assert!(parse("{ from maths import { sin } }").is_err());
  assert!(parse("{ from maths import { sin }\n sin(5) }").is_ok());
  assert!(parse("{ from maths import { sin, }\n sin(5) }").is_ok());

  // aliasing imports
  assert!(parse("from maths import { sin as }").is_err());
  assert!(parse("from maths import { sin as 7 }").is_err());
}

mod fault_tolerant {
  use super::parse;
  use indoc::indoc;

  #[test]
  fn let_statement_missing_identfier() {
    let ast = parse("let = 4 + 33");
    let expected = indoc! {"
      ├─ Let '' =
      │  ╰─ Binary (+)
      │     ├─ Number (4)
      │     ╰─ Number (33)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn let_double_identifier() {
    let ast = parse("let x x + 33");
    let expected = indoc! {"
       ├─ Let 'x' =
       │  ╰─ Binary (+)
       │     ├─ Variable (x)
       │     ╰─ Number (33)
     "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn let_missing_equals() {
    let ast = parse("let x  4 + 33");
    let expected = indoc! {"
      ├─ Let 'x' =
      │  ╰─ Binary (+)
      │     ├─ Number (4)
      │     ╰─ Number (33)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
    assert_eq!(ast.errors.len(), 1);
  }

  #[test]
  fn let_wrong_identifier() {
    let ast = parse("let 4 = 5 + 33");
    let expected = indoc! {"
      ├─ Let '4' =
      │  ╰─ Binary (+)
      │     ├─ Number (5)
      │     ╰─ Number (33)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn call_closing_bracket() {
    let ast = parse("func(4");
    let expected = indoc! {"
      ├─ Call
      │  ├─ Callee
      │  │  ╰─ Variable (func)
      │  ╰─ Argument
      │     ╰─ Number (4)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("func(\n");
    let expected = indoc! {"
      ├─ Call
      │  ├─ Callee
      │  │  ╰─ Variable (func)
      │  ╰─ Argument
      │     ╰─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn group_closing_bracket() {
    let ast = parse("(4 + 1");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Binary (+)
      │     ├─ Number (4)
      │     ╰─ Number (1)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("('hello'");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ String 'hello'
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn if_missing_brackets_or_else() {
    let expected = indoc! {"
      ├─ If
      │  ├─ Condition
      │  │  ╰─ Boolean (true)
      │  ├─ Then
      │  │  ╰─ Boolean (false)
      │  ╰─ Otherwise
      │     ╰─ Boolean (true)
    "};

    let ast = parse("if (true false else true");
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("if true) false else true");
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn match_missing_initial_pipe_or_arrow() {
    let ast = parse("match 7 2.. -> true | _ false");
    let expected = indoc! {"
      ├─ Match
      │  ├─ Number (7)
      │  ╰─ Cases:
      │     ├─ Pattern ─ 2 ..
      │     │  ╰─ Boolean (true)
      │     ╰─ Pattern ─ _
      │        ╰─ Boolean (false)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn group_recovers() {
    let ast = parse("(¬)");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("(2 /+/ 5)");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Binary (/)
      │     ├─ Binary (/)
      │     │  ├─ Number (2)
      │     │  ╰─ Invalid
      │     ╰─ Number (5)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn let_statement_recovers() {
    let ast = parse("let x = ¬\nlet b = 8");
    let expected = indoc! {"
      ├─ Let 'x' =
      │  ╰─ Invalid
      ├─ Let 'b' =
      │  ╰─ Number (8)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn expression_statement_recovers() {
    let ast = parse("(5 5)+4\n3");
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Number (5)
      ├─ Number (5)
      ├─ Invalid
      ├─ Number (3)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("3 ¬ 4");
    let expected = indoc! {"
      ├─ Number (3)
      ├─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn block_expression_recovers() {
    let ast = parse("{5\n¬4784¬}");
    let expected = indoc! {"
      ├─ Block
      │  ├─ Number (5)
      │  ╰─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("{5\n¬4784¬\n4}");
    let expected = indoc! {"
      ├─ Block
      │  ├─ Number (5)
      │  ├─ Invalid
      │  ╰─ Number (4)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("{let x = 5}");
    let expected = indoc! {"
      ├─ Block
      │  ╰─ Let 'x' =
      │     ╰─ Number (5)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn known_token_misused_as_binary_operator() {
    let ast = parse("a -> 5");
    let expected = indoc! {"
      ├─ Variable (a)
      ├─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn single_equals_as_binary_operator() {
    let ast = parse("a = 5");
    let expected = indoc! {"
      ├─ Binary (=)
      │  ├─ Variable (a)
      │  ╰─ Number (5)
    "};
    assert!(ast.is_err());
    assert!(ast.errors.len() == 1);
    assert_eq!(ast.to_string(), expected);

    let ast = parse("4 = 5");
    let expected = indoc! {"
      ├─ Binary (=)
      │  ├─ Number (4)
      │  ╰─ Number (5)
    "};
    assert!(ast.is_err());
    assert!(ast.errors.len() == 1);
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn import_statement() {
    let ast = parse("from 4 import { xx");
    let expected = indoc! {"
      ├─ From '4' Import
      │  ╰─ xx
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }
}

#[test]
fn multibyte_utf8_characters() {
  let two_byte_a = "a ± b";
  let error_span = parse(two_byte_a).errors.first().unwrap().span();
  assert_eq!(error_span.source_text(two_byte_a), "±");

  let two_byte_b = "a © b";
  let error_span = parse(two_byte_b).errors.first().unwrap().span();
  assert_eq!(error_span.source_text(two_byte_b), "©");

  let three_byte = "c ࠀ b";
  let error_span = parse(three_byte).errors.first().unwrap().span();
  assert_eq!(error_span.source_text(three_byte), "ࠀ");

  let four_byte = "d 🌈 b";
  let error_span = parse(four_byte).errors.first().unwrap().span();
  assert_eq!(error_span.source_text(four_byte), "🌈");
}