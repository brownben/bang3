//! # Compiler Error Tests
//!
//! Generates inputs (generally pathlogical input cases) which results in a failure to
//! create bytecode.

use bang_interpreter::{CompileError, compile};
use bang_syntax::parse;

fn integer_to_identifier(integer: u32) -> String {
  fn to_char(integer: u32) -> char {
    char::from_u32(u32::from('a') + integer).unwrap()
  }

  if integer > 26 {
    let prefix = integer_to_identifier(integer / 26);
    format!("{prefix}{}", to_char(integer % 26))
  } else {
    to_char(integer).to_string()
  }
}

#[test]
fn invalid_ast() {
  // expression
  let ast = parse("&hello".to_owned());
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::InvalidAST) => {}
    Err(e) => panic!("Expected InvalidAST, got {e}"),
  }

  // binary operator
  let ast = parse("hello = 5".to_owned());
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::InvalidAST) => {}
    Err(e) => panic!("Expected InvalidAST, got {e}"),
  }

  // pattern
  let ast = parse("match x | £ -> 5 | _ -> 4".to_owned());
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::InvalidAST) => {}
    Err(e) => panic!("Expected InvalidAST, got {e}"),
  }
}

#[test]
#[cfg_attr(miri, ignore)] // reason: test has pathological input, so are very slow
fn too_many_local_variables() {
  fn generate_local_variables(count: u32) -> String {
    let mut source = "{\n".to_owned();
    for _ in 0..=count {
      source.push_str("let a = 1\n");
    }
    source.push_str("  a\n}");
    source
  }

  let source = generate_local_variables(254);
  let ast = parse(source);
  assert!(compile(&ast).is_ok());

  let source = generate_local_variables(257);
  let ast = parse(source);
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::TooManyLocalVariables) => {}
    Err(e) => panic!("Expected TooManyLocalVariables, got {e}"),
  }
}

#[test]
#[cfg_attr(miri, ignore)] // reason: test has pathological input, so are very slow
fn too_many_symbols() {
  fn generate_symbols(count: u32) -> String {
    let mut source = String::new();
    for i in 0..=count {
      source.push_str(&format!("let {} = 1\n", integer_to_identifier(i)));
    }
    source
  }

  let source = generate_symbols(255);
  let ast = parse(source);
  assert!(compile(&ast).is_ok());

  let source = generate_symbols(256);
  let ast = parse(source);
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::TooManySymbols) => {}
    Err(e) => panic!("Expected TooManySymbols, got {e}"),
  }
}

#[test]
#[cfg_attr(miri, ignore)] // reason: test has pathological input, so are very slow
fn too_many_constants() {
  fn generate_constants(count: u32) -> String {
    let mut source = String::new();
    for i in 0..=count {
      source.push_str(&format!("'string: {i}'\n"));
    }
    source
  }

  let source = generate_constants(u32::from(u16::MAX));
  let ast = parse(source);
  assert!(compile(&ast).is_ok());

  let source = generate_constants(u32::from(u16::MAX) + 1);
  let ast = parse(source);
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::TooManyConstants) => {}
    Err(e) => panic!("Expected TooManyConstants, got {e}"),
  }
}

#[test]
#[cfg_attr(miri, ignore)] // reason: test has pathological input, so are very slow
fn too_big_jump() {
  let mut source = "if (x) {".to_owned();
  for _ in 0..=u16::MAX {
    source.push_str("  true == true\n");
  }
  source.push_str("true }");

  let ast = parse(source);
  match compile(&ast) {
    Ok(_) => panic!("Expected an error"),
    Err(CompileError::TooBigJump) => {}
    Err(e) => panic!("Expected TooBigJump, got {e}"),
  }
}
