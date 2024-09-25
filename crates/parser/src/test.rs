use crate::{parse, Allocator, AST};
use indoc::indoc;

impl AST<'_, '_> {
  fn is_ok(&self) -> bool {
    self.errors.is_empty()
  }
  fn is_err(&self) -> bool {
    !self.errors.is_empty()
  }
}

fn parse_to_string(source: &str) -> String {
  let allocator = Allocator::new();
  let ast = parse(source, &allocator);
  ast.to_string()
}

#[test]
fn unterminated_string() {
  let allocator = Allocator::new();
  assert!(parse("'unterminated string", &allocator).is_err());
  assert!(parse("\"un", &allocator).is_err());
  assert!(parse("`", &allocator).is_err());

  assert!(parse("``", &allocator).is_ok());
  assert!(parse("`hello world`", &allocator).is_ok());
}

#[test]
fn unknown_character() {
  let allocator = Allocator::new();
  assert!(parse("¬", &allocator).is_err());
  assert!(parse("3 $ 4", &allocator).is_err());
  assert!(parse("3 ¬ 4", &allocator).is_err());
  assert!(parse("🤗", &allocator).is_err());
  assert!(parse(".-2", &allocator).is_err());
  assert!(parse("&", &allocator).is_err());

  // Having unknown characters in strings are fine
  assert!(parse("'¬'", &allocator).is_ok());
  assert!(parse("'🤗'", &allocator).is_ok());
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
  let allocator = Allocator::new();
  assert!(parse("+ 5", &allocator).is_err());
  assert!(parse("*", &allocator).is_err());
  assert!(parse(" || 7", &allocator).is_err());
}

#[test]
fn binary_missing_right_expression() {
  let allocator = Allocator::new();
  assert!(parse("4 +", &allocator).is_err());
  assert!(parse("'hello' *", &allocator).is_err());
  assert!(parse("false ||", &allocator).is_err());
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
  let allocator = Allocator::new();
  assert!(parse("{let x = 4\nx + 2", &allocator).is_err());
  assert!(parse("{}", &allocator).is_err());
  assert!(parse("{let a = 1}", &allocator).is_err());
  assert!(parse("{7", &allocator).is_err());
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
  let allocator = Allocator::new();
  assert!(parse("f(", &allocator).is_err());
  assert!(parse("f(6", &allocator).is_err());

  assert!(parse("f()", &allocator).is_ok());
  assert!(parse("f ()", &allocator).is_ok());
  assert!(parse("f(\n)", &allocator).is_ok());

  assert!(parse("f(\n'hello')", &allocator).is_ok());
  assert!(parse("f('hello'\n)", &allocator).is_ok());
  assert!(parse("f(\n'hello'\n)", &allocator).is_ok());
}

#[test]
fn comment() {
  let ast = parse_to_string("5 // hello world");
  let expected = indoc! {"
    ├─ Comment (hello world)
    │  ╰─ Number (5)
  "};
  assert_eq!(ast, expected);
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
}

#[test]
fn if_missing_parts() {
  let allocator = Allocator::new();
  assert!(parse("if 'hello')", &allocator).is_err());
  assert!(parse("if'hello')", &allocator).is_err());
  assert!(parse("if ('hello')", &allocator).is_err());
  assert!(parse("if('hello')", &allocator).is_err());
  assert!(parse("if ('hello'", &allocator).is_err());
  assert!(parse("if ('hello') then", &allocator).is_err());
  assert!(parse("if('hello') then", &allocator).is_err());
  assert!(parse("if(\n'hello') then", &allocator).is_err());
  assert!(parse("if('hello') then else", &allocator).is_err());

  assert!(parse("if ('hello') then else false", &allocator).is_ok());
  assert!(parse("if('hello') then else true", &allocator).is_ok());
  assert!(parse("if(\n'hello') then else x", &allocator).is_ok());
  assert!(parse("if('hello'\n) then else stuff", &allocator).is_ok());
  assert!(parse("if(\n'hello'\n) then else otherwise", &allocator).is_ok());
}

#[test]
fn group_no_end_bracket() {
  let allocator = Allocator::new();
  assert!(parse("(4 + 5 ", &allocator).is_err());
  assert!(parse("('hello'", &allocator).is_err());
  assert!(parse("(", &allocator).is_err());

  assert!(parse("('hello')", &allocator).is_ok());
  assert!(parse("(5)", &allocator).is_ok());
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
fn match_missing_parts() {
  let allocator = Allocator::new();
  assert!(parse("match | 1 -> 2", &allocator).is_err());
  assert!(parse("match 3 | -5", &allocator).is_err());
  assert!(parse("match 3 1 -> 3", &allocator).is_err());
  assert!(parse("match 3 | 1 -> 3, 2 -> 4", &allocator).is_err());
  assert!(parse("match 3 | 1 -> 3\n 2 -> 4", &allocator).is_err());
  assert!(parse("match 3 | 1,", &allocator).is_err());
  assert!(parse("match true | .. -> 2", &allocator).is_err());

  assert!(parse("match true | 1 -> 2", &allocator).is_ok());
  assert!(parse("match 4 + 5 | ..9 -> 2", &allocator).is_ok());
  assert!(parse("match 4 + 5 | ..'z' -> 2", &allocator).is_ok());
  assert!(parse("match char | 'a'..'z' -> true | _ -> false", &allocator).is_ok());
  assert!(parse("match false | 1.. -> 2", &allocator).is_ok());
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
  let allocator = Allocator::new();
  assert!(parse("let = 7", &allocator).is_err());
  assert!(parse("let var", &allocator).is_err());
  assert!(parse("let var =", &allocator).is_err());
  assert!(parse("let var 7", &allocator).is_err());
  assert!(parse("let false = 7", &allocator).is_err());

  assert!(parse("let var = 7", &allocator).is_ok());
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

mod fault_tolerant {
  use super::{parse, Allocator};
  use indoc::indoc;

  #[test]
  fn let_statement_missing_identfier() {
    let allocator = Allocator::new();
    let ast = parse("let = 4 + 33", &allocator);
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
    let allocator = Allocator::new();
    let ast = parse("let x x + 33", &allocator);
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
    let allocator = Allocator::new();
    let ast = parse("let x  4 + 33", &allocator);
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
    let allocator = Allocator::new();
    let ast = parse("let 4 = 5 + 33", &allocator);
    let expected = indoc! {"
      ├─ Let '' =
      │  ╰─ Number (4)
      ├─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn call_closing_bracket() {
    let allocator = Allocator::new();

    let ast = parse("func(4", &allocator);
    let expected = indoc! {"
      ├─ Call
      │  ├─ Callee
      │  │  ╰─ Variable (func)
      │  ╰─ Argument
      │     ╰─ Number (4)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("func(\n", &allocator);
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
    let allocator = Allocator::new();

    let ast = parse("(4 + 1", &allocator);
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Binary (+)
      │     ├─ Number (4)
      │     ╰─ Number (1)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("('hello'", &allocator);
    let expected = indoc! {"
      ├─ Group
      │  ╰─ String 'hello'
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn if_missing_brackets_or_else() {
    let allocator = Allocator::new();

    let expected = indoc! {"
      ├─ If
      │  ├─ Condition
      │  │  ╰─ Boolean (true)
      │  ├─ Then
      │  │  ╰─ Boolean (false)
      │  ╰─ Otherwise
      │     ╰─ Boolean (true)
    "};

    let ast = parse("if (true false else true", &allocator);
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("if true) false else true", &allocator);
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("if (true) false true", &allocator);
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn match_missing_initial_pipe_or_arrow() {
    let allocator = Allocator::new();

    let ast = parse("match 7 2.. -> true | _ false", &allocator);
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
    let allocator = Allocator::new();

    let ast = parse("(¬)", &allocator);
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("(2 /+/ 5)", &allocator);
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
    let allocator = Allocator::new();

    let ast = parse("let x = ¬\nlet b = 8", &allocator);
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
    let allocator = Allocator::new();

    let ast = parse("(5 5)+4\n3", &allocator);
    let expected = indoc! {"
      ├─ Group
      │  ╰─ Number (5)
      ├─ Number (5)
      ├─ Invalid
      ├─ Number (3)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("3 ¬ 4", &allocator);
    let expected = indoc! {"
      ├─ Number (3)
      ├─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn block_expression_recovers() {
    let allocator = Allocator::new();

    let ast = parse("{5\n¬4784¬}", &allocator);
    let expected = indoc! {"
      ├─ Block
      │  ├─ Number (5)
      │  ╰─ Invalid
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);

    let ast = parse("{5\n¬4784¬\n4}", &allocator);
    let expected = indoc! {"
      ├─ Block
      │  ├─ Number (5)
      │  ├─ Invalid
      │  ╰─ Number (4)
    "};
    assert!(ast.is_err());
    assert_eq!(ast.to_string(), expected);
  }
}
