//! # Standard Library Tests
//!
//! Check that the standard library works as expected.

use bang_gc::HeapSize;
use bang_interpreter::{StandardContext, VM, Value, compile};
use bang_syntax::parse;
use indoc::indoc;

fn run(source: &str) -> Result<VM, ()> {
  let ast = parse(source.to_owned());
  assert!(ast.is_valid());
  let chunk = compile(&ast).unwrap();
  let mut vm = VM::new(HeapSize::Small, &StandardContext).unwrap();
  vm.run(&chunk).map_err(|_| ())?;
  Ok(vm)
}

macro_rules! assert_variable {
  ($vm:expr; $name:ident, string $string:literal) => {
    let vm = $vm.as_mut().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    let string = vm.allocate_string($string);

    assert!(
      vm.equals(value, string),
      "{} (type {}) != {} (type {})",
      value.display(&vm),
      value.get_type(&vm),
      string.display(&vm),
      string.get_type(&vm),
    );
  };
  ($vm:expr; $name:ident, $value:expr) => {
    let vm = $vm.as_ref().unwrap();
    let value = vm.get_global(stringify!($name)).unwrap();
    assert_eq!(value, Value::from($value));
  };
}
use assert_variable;

mod builtin_functions {
  use super::{Value, assert_variable, run};

  #[test]
  fn print() {
    let mut works_as_identity = run("let a = print('hello')\nlet b = print(5)");
    assert_variable!(works_as_identity; a, string "hello");
    assert_variable!(works_as_identity; b, 5.0);
  }

  #[test]
  fn type_of() {
    let mut string = run("let a = type('hello')");
    assert_variable!(string; a, string "string");

    let mut allocated_string = run("let a = type('hello' ++ ' world')");
    assert_variable!(allocated_string; a, string "string");

    let mut number = run("let a = type(55.2)");
    assert_variable!(number; a, string "number");

    let mut r#false = run("let a = type(false)");
    assert_variable!(r#false; a, string "boolean");

    let mut r#true = run("let a = type(true)");
    assert_variable!(r#true; a, string "boolean");

    let mut builtin_function = run("let a = type(print)");
    assert_variable!(builtin_function; a, string "function");

    let mut named_function = run("let identity = x => x\nlet a = type(identity)");
    assert_variable!(named_function; a, string "function");

    let mut anonymous_function = run("let a = type(x => x)");
    assert_variable!(anonymous_function; a, string "function");

    let mut closure = run("let a = type((x => _ => x)())");
    assert_variable!(closure; a, string "function");

    let mut native_closure = run("from maths import { pow }\nlet a = type(pow(3))");
    assert_variable!(native_closure; a, string "function");
  }

  #[test]
  fn panic() {
    let panics = run("panic('Should end execution')");
    assert!(panics.is_err());
  }
}

mod maths {
  use super::{Value, assert_variable, indoc, run};

  #[test]
  fn constants() {
    let constants = run(indoc! {"
      from maths import { PI, E, INFINITY }

      let bigger = INFINITY > 1000000000000000000000000
    "});
    assert_variable!(constants; PI, std::f64::consts::PI);
    assert_variable!(constants; E, std::f64::consts::E);
    assert_variable!(constants; bigger, true);
  }

  #[test]
  fn rounding() {
    let rounding = run(indoc! {"
      from maths import { ceil, floor, round }

      let ceil_a = ceil(1)
      let ceil_b = ceil(1.01)
      let ceil_c = ceil(1.5)
      let ceil_d = ceil(72.3)

      let floor_a = floor(1)
      let floor_b = floor(1.01)
      let floor_c = floor(1.5)
      let floor_d = floor(72.3)

      let round_a = round(1)
      let round_b = round(1.01)
      let round_c = round(1.5)
      let round_d = round(72.3)
    "});
    assert_variable!(rounding; ceil_a, 1.0);
    assert_variable!(rounding; ceil_b, 2.0);
    assert_variable!(rounding; ceil_c, 2.0);
    assert_variable!(rounding; ceil_d, 73.0);

    assert_variable!(rounding; floor_a, 1.0);
    assert_variable!(rounding; floor_b, 1.0);
    assert_variable!(rounding; floor_c, 1.0);
    assert_variable!(rounding; floor_d, 72.0);

    assert_variable!(rounding; round_a, 1.0);
    assert_variable!(rounding; round_b, 1.0);
    assert_variable!(rounding; round_c, 2.0);
    assert_variable!(rounding; round_d, 72.0);
  }

  #[test]
  fn absolute_value() {
    let abs = run(indoc! {"
      from maths import { abs }

      let d = abs(1)
      let e = abs(-1)
      let f = abs(0)
      let g = abs(1.1)
    "});
    assert_variable!(abs; d, 1.0);
    assert_variable!(abs; e, 1.0);
    assert_variable!(abs; f, 0.0);
    assert_variable!(abs; g, 1.1);
  }

  #[test]
  fn trigonometric() {
    let trig = run(indoc! {"
      from maths import { sin, cos, tan, PI }

      let a = sin(0)
      let b = cos(0)
      let c = tan(0)
      let d = sin(PI / 6)

      let dSmall = d > 0.49
      let dBig = d < 0.51
    "});
    assert_variable!(trig; a, 0.0);
    assert_variable!(trig; b, 1.0);
    assert_variable!(trig; c, 0.0);
    assert_variable!(trig; dSmall, true);
    assert_variable!(trig; dBig, true);

    let inverse_trig = run(indoc! {"
      from maths import { asin, acos, atan, isNan }

      let a = asin(0)
      let b = acos(1)
      let c = atan(0)
      let d = isNan(asin(55))
    "});
    assert_variable!(inverse_trig; a, 0.0);
    assert_variable!(inverse_trig; b, 0.0);
    assert_variable!(inverse_trig; c, 0.0);
    assert_variable!(inverse_trig; d, true);

    let hyperbolic = run(indoc! {"
      from maths import { sinh, cosh, tanh, asinh, acosh, atanh }

      let a = sinh(0)
      let b = cosh(0)
      let c = tanh(0)
      let d = asinh(0)
      let e = acosh(1)
      let f = atanh(0)
    "});
    assert_variable!(hyperbolic; a, 0.0);
    assert_variable!(hyperbolic; b, 1.0);
    assert_variable!(hyperbolic; c, 0.0);
    assert_variable!(hyperbolic; d, 0.0);
    assert_variable!(hyperbolic; e, 0.0);
    assert_variable!(hyperbolic; f, 0.0);
  }

  #[test]
  fn exponentials_and_logarithms() {
    let root = run(indoc! {"
      from maths import { sqrt, cbrt }

      let a = sqrt(4)
      let b = cbrt(8)
    "});
    assert_variable!(root; a, 2.0);
    assert_variable!(root; b, 2.0);

    let exp = run(indoc! {"
      from maths import { exp }

      let a = exp(0)
      let b = exp(1)
    "});
    assert_variable!(exp; a, 1.0);
    assert_variable!(exp; b, std::f64::consts::E);

    let pow = run(indoc! {"
      from maths import { pow }

      let a = pow(2)(3)
      let b = 4 >> pow(2)

      let power = pow(5)
      let c = power(3)
    "});
    assert_variable!(pow; a, 8.0);
    assert_variable!(pow; b, 16.0);
    assert_variable!(pow; c, 125.0);

    let ln = run(indoc! {"
      from maths import { ln, E }

      let a = ln(1)
      let b = ln(E)
    "});
    assert_variable!(ln; a, 0.0);
    assert_variable!(ln; b, 1.0);

    let log = run(indoc! {"
      from maths import { log }

      let a = log(10)(1)
      let b = 100 >> log(10)
    "});
    assert_variable!(log; a, 0.0);
    assert_variable!(log; b, 2.0);

    let log_wrong_type = run("maths::log('')");
    assert!(log_wrong_type.is_err());
  }

  #[test]
  fn angle_conversions() {
    let vm = run(indoc! {"
      from maths import { degreesToRadians, radiansToDegrees, PI }

      let a = degreesToRadians(180)
      let b = radiansToDegrees(PI)
    "});
    assert_variable!(vm; a, std::f64::consts::PI);
    assert_variable!(vm; b, 180.0);
  }

  #[test]
  fn wrong_types() {
    let wrong_type = run("from maths import { sin }\nlet a = sin(false)");
    assert!(wrong_type.is_err());
    let wrong_type = run("from maths import { pow }\nlet a = pow(false)(5)");
    assert!(wrong_type.is_err());
    let wrong_type = run("from maths import { pow }\nlet a = pow(5)(false)");
    assert!(wrong_type.is_err());
  }
}

mod string {
  use super::{Value, assert_variable, indoc, run};

  #[test]
  fn to_string() {
    let mut vm = run(indoc! {"
      from string import { toString }

      let a = toString('hello')
      let b = string::from(55.2)
      let c = toString(false)
      let d = string::from(true)
      let e = toString(print)

      let identity = x => x
      let f = string::from(identity)
      let g = toString(x => x)
      let h = string::from((x => _ => x)())

      from maths import { pow }
      let i = toString(pow(2))
      let j = string::from('world')
    "});
    assert_variable!(vm; a, string "hello");
    assert_variable!(vm; b, string "55.2");
    assert_variable!(vm; c, string "false");
    assert_variable!(vm; d, string "true");
    assert_variable!(vm; e, string "<function print>");
    assert_variable!(vm; f, string "<function identity>");
    assert_variable!(vm; g, string "<function>");
    assert_variable!(vm; h, string "<closure <function>>");
    assert_variable!(vm; i, string "<closure <function pow>>");
    assert_variable!(vm; j, string "world");
  }

  #[test]
  fn length() {
    let length = run(indoc! {"
      from string import { length }

      let a = length('  hello  ')
      let b = length('hello')
      let c = length('')
      let d = length('🏃')
      let e = length('🏃‍♂️‍➡️')
    "});
    assert_variable!(length; a, 9.0);
    assert_variable!(length; b, 5.0);
    assert_variable!(length; c, 0.0);
    assert_variable!(length; d, 1.0);
    assert_variable!(length; e, 7.0);

    let byte_length = run(indoc! {"
      from string import { byteLength }

      let a = byteLength('  hello  ')
      let b = byteLength('hello')
      let c = byteLength('')
      let d = byteLength('🏃')
      let e = byteLength('🏃‍♂️‍➡️')
    "});
    assert_variable!(byte_length; a, 9.0);
    assert_variable!(byte_length; b, 5.0);
    assert_variable!(byte_length; c, 0.0);
    assert_variable!(byte_length; d, 4.0);
    assert_variable!(byte_length; e, 22.0);

    let is_empty = run(indoc! {"
      from string import { isEmpty }

      let a = isEmpty('hello')
      let b = isEmpty('')
      let c = isEmpty('🏃')
    "});
    assert_variable!(is_empty; a, false);
    assert_variable!(is_empty; b, true);
    assert_variable!(is_empty; c, false);
  }

  #[test]
  fn is_ascii() {
    let is_ascii = run(indoc! {"
      from string import { isAscii }

      let a = isAscii('hello')
      let b = isAscii('')
      let c = isAscii('🏃')
    "});
    assert_variable!(is_ascii; a, true);
    assert_variable!(is_ascii; b, true);
    assert_variable!(is_ascii; c, false);
  }

  #[test]
  fn change_case() {
    let mut change_case = run(indoc! {"
      from string import { toLowercase, toUppercase }

      let a = toUppercase('hello')
      let b = toUppercase('')
      let c = toUppercase('🏃')
      let d = toUppercase('HELLO')
      let e = toUppercase('Hello')

      let f = toLowercase('hello')
      let g = toLowercase('')
      let h = toLowercase('🏃')
      let i = toLowercase('HELLO')
      let j = toLowercase('Hello')
    "});
    assert_variable!(change_case; a, string "HELLO");
    assert_variable!(change_case; b, string "");
    assert_variable!(change_case; c, string "🏃");
    assert_variable!(change_case; d, string "HELLO");
    assert_variable!(change_case; e, string "HELLO");
    assert_variable!(change_case; f, string "hello");
    assert_variable!(change_case; g, string "");
    assert_variable!(change_case; h, string "🏃");
    assert_variable!(change_case; i, string "hello");
    assert_variable!(change_case; j, string "hello");
  }

  #[test]
  fn contains() {
    let contains = run(indoc! {"
      from string import { contains, startsWith, endsWith }

      let a = 'hello' >> contains('x')
      let b = 'hello' >> contains('l')
      let c = contains('h')('hello')
      let d = 'whats up' >> contains('up')

      let e = 'starts' >> startsWith('start')
      let f = 'starts' >> endsWith('s')
      let g = 'starts' >> startsWith('t')
      let h = 'starts' >> endsWith('farts')
    "});
    assert_variable!(contains; a, false);
    assert_variable!(contains; b, true);
    assert_variable!(contains; d, true);
    assert_variable!(contains; e, true);
    assert_variable!(contains; f, true);
    assert_variable!(contains; g, false);
    assert_variable!(contains; h, false);

    let starts_with_non_string = run("string::startsWith(1)");
    assert!(starts_with_non_string.is_err());
    let ends_with_non_string = run("string::endsWith(1)");
    assert!(ends_with_non_string.is_err());
  }

  #[test]
  fn trim() {
    let mut trim = run(indoc! {"
      from string import { trim, trimStart, trimEnd }

      let a = trim('  hello  ')
      let b = trim('  hello')
      let c = trim('hello  ')
      let d = trim('hello')
      let e = trim('')

      let f = trimStart('  hello  ')
      let g = trimStart('  hello')
      let h = trimStart('hello  ')
      let i = trimStart('hello')
      let j = trimStart('')

      let k = trimEnd('  hello  ')
      let l = trimEnd('  hello')
      let m = trimEnd('hello  ')
      let n = trimEnd('hello')
      let o = trimEnd('')

      let p = 'hello' == trim('hello  ')
      let q = trim('hello') == 'hello'
      let r = trim('hello  ') ++ 'world'
      let s = trim('  ' ++ 'hello  ')
      let t = (' hello ' >> trimStart >> trimEnd) == 'hello'
    "});
    assert_variable!(trim; a, string "hello");
    assert_variable!(trim; b, string "hello");
    assert_variable!(trim; c, string "hello");
    assert_variable!(trim; d, string "hello");
    assert_variable!(trim; e, string "");
    assert_variable!(trim; f, string "hello  ");
    assert_variable!(trim; g, string "hello");
    assert_variable!(trim; h, string "hello  ");
    assert_variable!(trim; i, string "hello");
    assert_variable!(trim; j, string "");
    assert_variable!(trim; k, string "  hello");
    assert_variable!(trim; l, string "  hello");
    assert_variable!(trim; m, string "hello");
    assert_variable!(trim; n, string "hello");
    assert_variable!(trim; o, string "");
    assert_variable!(trim; p, true);
    assert_variable!(trim; q, true);
    assert_variable!(trim; r, string "helloworld");
    assert_variable!(trim; s, string "hello");
    assert_variable!(trim; t, true);
  }

  #[test]
  fn replace() {
    let mut replace = run(indoc! {"
      from string import { replaceAll, replaceOne }

      let a = replaceAll('l')('x')('hello')
      let b = replaceAll('l')('l')('hello')
      let c = replaceAll('l')('')('hello')
      let d = replaceAll('farts')('')('hello')

      let e = replaceOne('l')('x')('hello')
      let f = replaceOne('l')('l')('hello')
      let g = replaceOne('l')('')('hello')
      let h = replaceOne('farts')('')('hello')
    "});
    assert_variable!(replace; a, string "hexxo");
    assert_variable!(replace; b, string "hello");
    assert_variable!(replace; c, string "heo");
    assert_variable!(replace; d, string "hello");
    assert_variable!(replace; e, string "hexlo");
    assert_variable!(replace; f, string "hello");
    assert_variable!(replace; g, string "helo");
    assert_variable!(replace; h, string "hello");

    let replace_one_arg_a_non_string = run("string::replaceOne(1)");
    assert!(replace_one_arg_a_non_string.is_err());
    let replace_one_arg_b_non_string = run("string::replaceOne('')(5)");
    assert!(replace_one_arg_b_non_string.is_err());

    let replace_all_arg_a_non_string = run("string::replaceAll(1)");
    assert!(replace_all_arg_a_non_string.is_err());
    let replace_all_arg_b_non_string = run("string::replaceAll('')(5)");
    assert!(replace_all_arg_b_non_string.is_err());
  }

  #[test]
  fn chars() {
    let mut chars = run(indoc! {"
      let a = 'hello' >> string::chars >> iter::toList >> string::from
      let b = 'hi 🏃' >> string::chars >> iter::toList >> string::from
      let c = '' >> string::chars >> iter::count

      let two_byte = 'a © b' >> string::chars >> iter::toList >> string::from
      let three_byte = 'c ࠀ b' >> string::chars >> iter::toList >> string::from
      let four_byte = 'd 🌈 b' >> string::chars >> iter::toList >> string::from
    "});
    assert_variable!(chars; a, string "['h', 'e', 'l', 'l', 'o']");
    assert_variable!(chars; b, string "['h', 'i', ' ', '🏃']");
    assert_variable!(chars; c, 0.0);
    assert_variable!(chars; two_byte, string "['a', ' ', '©', ' ', 'b']");
    assert_variable!(chars; three_byte, string "['c', ' ', 'ࠀ', ' ', 'b']");
    assert_variable!(chars; four_byte, string "['d', ' ', '🌈', ' ', 'b']");

    let not_string = run("string::chars(5)");
    assert!(not_string.is_err());
  }

  #[test]
  fn lines() {
    let mut lines = run(indoc! {"
      let linesToString = x => (x >> string::lines >> iter::toList >> string::from)

      let a = 'hello\nworld\nthis has three lines' >> linesToString
      let b = 'hello\r\nworld\r\nthis has three lines\n' >> linesToString
      let c = 'hello' >> linesToString
      let d = '' >> linesToString
      let e = 'a © b\nc ࠀ b\nd 🌈 b' >> linesToString
    "});
    assert_variable!(lines; a, string "['hello', 'world', 'this has three lines']");
    assert_variable!(lines; b, string "['hello', 'world', 'this has three lines']");
    assert_variable!(lines; c, string "['hello']");
    assert_variable!(lines; d, string "[]");
    assert_variable!(lines; e, string "['a © b', 'c ࠀ b', 'd 🌈 b']");

    let not_string = run("string::lines(5)");
    assert!(not_string.is_err());
  }

  #[test]
  fn parse_number() {
    let mut parse_number = run(indoc! {"
      let a = string::parseNumber('3') >> string::from
      let b = string::parseNumber('3.14') >> string::from
      let c = string::parseNumber('3.14e-2') >> string::from
      let d = string::parseNumber('three') >> string::from
      let e = string::parseNumber('3.14e') >> string::from
      let f = string::parseNumber('-77') >> string::from
      let g = string::parseNumber('+77') >> string::from
      let h = string::parseNumber('NaN') >> string::from
      let i = string::parseNumber('Infinity') >> string::from
      let j = string::parseNumber('Inf') >> string::from
      let k = string::parseNumber('3.0') >> string::from
    "});
    assert_variable!(parse_number; a, string "Some(3)");
    assert_variable!(parse_number; b, string "Some(3.14)");
    assert_variable!(parse_number; c, string "Some(0.0314)");
    assert_variable!(parse_number; d, string "None");
    assert_variable!(parse_number; e, string "None");
    assert_variable!(parse_number; f, string "Some(-77)");
    assert_variable!(parse_number; g, string "Some(77)");
    assert_variable!(parse_number; h, string "Some(NaN)");
    assert_variable!(parse_number; i, string "Some(inf)");
    assert_variable!(parse_number; j, string "Some(inf)");
    assert_variable!(parse_number; k, string "Some(3)");

    let not_string = run("string::parseNumber(5)");
    assert!(not_string.is_err());
  }

  #[test]
  fn wrong_types() {
    let length_wrong_type = run("from string import { length }\nlet a = length(5)");
    assert!(length_wrong_type.is_err());
    let to_uppercase_wrong_type = run("from string import { toUppercase }\nlet a = toUppercase(5)");
    assert!(to_uppercase_wrong_type.is_err());
    let is_empty_wrong_type = run("from string import { isEmpty }\nlet a = isEmpty(5)");
    assert!(is_empty_wrong_type.is_err());
    let contains_wrong_type = run("from string import { contains }\nlet a = contains(5)('')");
    assert!(contains_wrong_type.is_err());
    let contains_wrong_type = run("from string import { contains }\nlet a = contains('')(5)");
    assert!(contains_wrong_type.is_err());
  }
}

mod list {
  use super::{Value, assert_variable, indoc, run};

  #[test]
  fn length() {
    let length = run(indoc! {"
      from list import { length }

      let a = length([])
      let b = length(['hello'])
      let c = length([1,2,3,])
    "});
    assert_variable!(length; a, 0.0);
    assert_variable!(length; b, 1.0);
    assert_variable!(length; c, 3.0);

    let is_empty = run(indoc! {"
      from list import { isEmpty }

      let a = isEmpty([])
      let b = isEmpty(['hello'])
      let c = isEmpty([1,2,3,])
    "});
    assert_variable!(is_empty; a, true);
    assert_variable!(is_empty; b, false);
    assert_variable!(is_empty; c, false);

    let not_list = run("list::length(x => x + 1)");
    assert!(not_list.is_err());
  }

  #[test]
  fn contains() {
    let contains = run(indoc! {"
      from list import { contains }

      let a = [] >> contains([])
      let b = [[]] >> contains([])
      let c = [1, 3, 5, 7] >> contains(3)
      let d = [1, 3, 5, 7] >> contains(2)
      let e = [1, 1, 1] >> contains(1)
    "});
    assert_variable!(contains; a, false);
    assert_variable!(contains; b, true);
    assert_variable!(contains; c, true);
    assert_variable!(contains; d, false);
    assert_variable!(contains; e, true);
  }

  #[test]
  fn get() {
    let get = run(indoc! {"
      from list import { get }
      from option import { isNone, unwrap }

      let a = [] >> get(0) >> isNone
      let b = [] >> get(-1) >> isNone
      let c = [1, 2, 3] >> get(0) >> unwrap
      let d = [1, 2, 3] >> get(1) >> unwrap
      let e = [1, 2, 3] >> get(2) >> unwrap
      let f = [1, 2, 3] >> get(3) >> isNone
      let g = [1, 2, 3] >> get(-1) >> unwrap
      let h = [1, 2, 3] >> get(-2) >> unwrap
      let i = [1, 2, 3] >> get(-3) >> unwrap
      let j = [1, 2, 3] >> get(-4) >> isNone
    "});
    assert_variable!(get; a, true);
    assert_variable!(get; b, true);
    assert_variable!(get; c, 1.0);
    assert_variable!(get; d, 2.0);
    assert_variable!(get; e, 3.0);
    assert_variable!(get; f, true);
    assert_variable!(get; g, 3.0);
    assert_variable!(get; h, 2.0);
    assert_variable!(get; i, 1.0);
    assert_variable!(get; j, true);
  }
}

mod option {
  use super::{Value, assert_variable, indoc, run};

  #[test]
  fn basic_types() {
    let mut to_string = run(indoc! {"
      from option import { None, Some }

      let a = Some(1) >> string::from
      let b = Some('hello world') >> string::from
      let c = None >> string::from
      let d = Some(None) >> string::from
    "});
    assert_variable!(to_string; a, string "Some(1)");
    assert_variable!(to_string; b, string "Some('hello world')");
    assert_variable!(to_string; c, string "None");
    assert_variable!(to_string; d, string "Some(None)");

    let is_falsy = run(indoc! {"
      from option import { None, Some }

      let a = if (None) 5 else 10
      let b = if (Some(0)) 5 else 10
      let c = if (Some(1)) 5 else 10
    "});
    assert_variable!(is_falsy; a, 10.0);
    assert_variable!(is_falsy; b, 5.0);
    assert_variable!(is_falsy; c, 5.0);

    let equality = run(indoc! {"
      from option import { None, Some }

      let a = None == None
      let b = Some(1) == Some(1)
      let c = Some(1) == Some(0)
      let d = Some(1) == None
      let e = Some(None) == None
      let f = None == Some(1)
      let g = Some([1, '', None]) == Some([1, '', None])
    "});
    assert_variable!(equality; a, true);
    assert_variable!(equality; b, true);
    assert_variable!(equality; c, false);
    assert_variable!(equality; d, false);
    assert_variable!(equality; e, false);
    assert_variable!(equality; f, false);
    assert_variable!(equality; g, true);
  }

  #[test]
  fn is_option() {
    let is_none = run(indoc! {"
      from option import { None, Some, isNone }

      let a = Some(1) >> isNone
      let b = None >> isNone
      let c = Some(None) >> isNone
      let d = 5 >> isNone
    "});
    assert_variable!(is_none; a, false);
    assert_variable!(is_none; b, true);
    assert_variable!(is_none; c, false);
    assert_variable!(is_none; d, false);

    let is_some = run(indoc! {"
      from option import { None, Some, isSome }

      let a = Some(1) >> isSome
      let b = None >> isSome
      let c = Some(None) >> isSome
      let d = 5 >> isSome
    "});
    assert_variable!(is_some; a, true);
    assert_variable!(is_some; b, false);
    assert_variable!(is_some; c, true);
    assert_variable!(is_some; d, false);
  }

  #[test]
  fn unwrap() {
    let of_none = run(indoc! {"
      from option import { None, Some, unwrap }

      unwrap(None)
    "});
    assert!(of_none.is_err());

    let of_some = run(indoc! {"
      from option import { None, Some, unwrap }

      let a = Some(1) >> unwrap
      let b = (Some(None) >> unwrap) == None
    "});
    assert_variable!(of_some; a, 1.0);
    assert_variable!(of_some; b, true);

    let wrong_type = run("option::unwrap(5)");
    assert!(wrong_type.is_err());

    let unwrap_or = run(indoc! {"
      from option import { None, Some, unwrapOr }

      let a = None >> unwrapOr(5)
      let b = Some(1) >> unwrapOr(5)
      let c = (Some(None) >> unwrapOr(5)) == None
    "});
    assert_variable!(unwrap_or; a, 5.0);
    assert_variable!(unwrap_or; b, 1.0);
    assert_variable!(unwrap_or; c, true);

    let unwrap_or_wrong_type = run("option::unwrapOr(5)(3)");
    assert!(unwrap_or_wrong_type.is_err());
  }

  #[test]
  fn flatten() {
    let mut flatten = run(indoc! {"
      from option import { None, Some, flatten }

      let a = Some(1) >> flatten >> string::from
      let b = None >> flatten >> string::from
      let c = Some(None) >> flatten >> string::from
      let d = Some(Some(5)) >> flatten >> string::from
    "});
    assert_variable!(flatten; a,string "1");
    assert_variable!(flatten; b,string "None");
    assert_variable!(flatten; c,string "None");
    assert_variable!(flatten; d,string "Some(5)");

    let not_option = run("option::flatten(5)");
    assert!(not_option.is_err());
  }

  #[test]
  fn map() {
    let mut simple = run(indoc! {"
      from option import { None, Some, map }

      let a = Some(8) >> map(x => x + 1) >> string::from
      let b = None >> map(x => x + 1) >> string::from
    "});
    assert_variable!(simple; a, string "Some(9)");
    assert_variable!(simple; b, string "None");

    let mut closure = run(indoc! {"
      from option import { None, Some, map }

      let timesBy = x => y => x * y

      let a = Some(8) >> map(timesBy(2)) >> string::from
      let b = None >> map(timesBy(2)) >> string::from
    "});
    assert_variable!(closure; a, string "Some(16)");
    assert_variable!(closure; b, string "None");

    let mut native_functions = run(indoc! {"
      from option import { None, Some, map }

      let a = Some('example') >> map(string::length) >> string::from
      let b = None >> map(string::length) >> string::from

      let c = Some(5) >> map(maths::abs) >> string::from
      let d = Some(-5) >> map(maths::abs) >> string::from
      let e = None >> map(maths::abs) >> string::from
    "});
    assert_variable!(native_functions; a, string "Some(7)");
    assert_variable!(native_functions; b, string "None");
    assert_variable!(native_functions; c, string "Some(5)");
    assert_variable!(native_functions; d, string "Some(5)");
    assert_variable!(native_functions; e, string "None");

    let mut native_closures = run(indoc! {"
      from option import { None, Some, map }

      let a = Some(8) >> map(maths::log(2)) >> string::from
      let b = None >> map(maths::log(2)) >> string::from

      let c = Some('hello') >> map(string::replaceOne('l')('L')) >> string::from
      let d = None >> map(maths::abs) >> string::from
    "});
    assert_variable!(native_closures; a, string "Some(3)");
    assert_variable!(native_closures; b, string "None");
    assert_variable!(native_closures; c, string "Some('heLlo')");
    assert_variable!(native_closures; d, string "None");

    let not_callable = run("option::map(5)");
    assert!(not_callable.is_err());
    let not_option = run("option::map(x => x + 1)(5)");
    assert!(not_option.is_err());
  }
}

mod iter {
  use core::f64;

  use super::{Value, assert_variable, indoc, run};

  #[test]
  fn basic_types() {
    let mut to_string = run(indoc! {"
      let a = iter::empty() >> string::from
      let b = iter::once() >> string::from
      let c = iter::repeat(1) >> string::from
    "});
    assert_variable!(to_string; a, string "<iterator>");
    assert_variable!(to_string; b, string "<iterator>");
    assert_variable!(to_string; c, string "<iterator>");

    let is_falsy = run(indoc! {"
      let a = if (iter::empty()) 5 else 10
      let b = if (iter::repeat(0)) 5 else 10
      let c = if (iter::repeat(1)) 5 else 10
    "});
    assert_variable!(is_falsy; a, 5.0);
    assert_variable!(is_falsy; b, 5.0);
    assert_variable!(is_falsy; c, 5.0);

    let equality = run(indoc! {"
      let a = iter::empty() == iter::empty()
      let b = iter::repeat(1) == iter::repeat(1)
      let c = iter::repeat(2) == iter::repeat(1)
      let d = iter::once(1) == iter::repeat(1)

      let empty = iter::empty()
      let one = iter::repeat(1)
      let e = empty == empty
      let f = one == one
      let g = empty == one
    "});
    assert_variable!(equality; a, false);
    assert_variable!(equality; b, false);
    assert_variable!(equality; c, false);
    assert_variable!(equality; d, false);
    assert_variable!(equality; e, true);
    assert_variable!(equality; f, true);
    assert_variable!(equality; g, false);
  }

  #[test]
  fn to_from_list() {
    let mut round_trip = run(indoc! {"
      let a = list::iter([]) >> iter::toList >> string::from
      let b = list::iter([1, 5, 10]) >> iter::toList >> string::from
    "});
    assert_variable!(round_trip; a, string "[]");
    assert_variable!(round_trip; b, string "[1, 5, 10]");

    let mut from_unknown_sized_iterator = run(indoc! {"
      let a = iter::integers() >> iter::takeWhile(x => x < 16) >> iter::toList >> string::from
    "});
    assert_variable!(from_unknown_sized_iterator; a, string "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]");

    let mut with_local_variables = run(indoc! {"
      let a = 'hello world'
      let b = iter::integers()
        >> iter::takeWhile(x => x < 10)
        >> iter::map(x => {
          let b = 0
          x + 0
        })
        >> iter::toList
        >> string::from
      let c = 1
    "});
    assert_variable!(with_local_variables; a, string "hello world");
    assert_variable!(with_local_variables; b, string "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]");
    assert_variable!(with_local_variables; c, 1.0);

    let to_list_infinite = run("iter::repeat(1) >> iter::toList");
    assert!(to_list_infinite.is_err());

    let to_list_non_iter = run("6 >> iter::toList");
    assert!(to_list_non_iter.is_err());

    let list_iter_non_list = run("6 >> list::iter");
    assert!(list_iter_non_list.is_err());
  }

  #[test]
  fn first_last() {
    let mut first = run(indoc! {"
      let a = iter::empty() >> iter::first >> string::from
      let b = iter::once(false) >> iter::first >> string::from
      let c = iter::repeat(77) >> iter::first >> string::from
      let d = iter::integers() >> iter::first >> string::from
    "});
    assert_variable!(first; a, string "None");
    assert_variable!(first; b, string "Some(false)");
    assert_variable!(first; c, string "Some(77)");
    assert_variable!(first; d, string "Some(0)");

    let mut last = run(indoc! {"
      let a = iter::empty() >> iter::last >> string::from
      let b = iter::once(false) >> iter::last >> string::from
      let c = list::iter([1, 2, 3]) >> iter::last >> string::from
    "});
    assert_variable!(last; a, string "None");
    assert_variable!(last; b, string "Some(false)");
    assert_variable!(last; c, string "Some(3)");

    let last_infinite = run("iter::repeat(1) >> iter::last");
    assert!(last_infinite.is_err());
    let first_not_iter = run("4 >> iter::first");
    assert!(first_not_iter.is_err());
    let last_not_iter = run("4 >> iter::last");
    assert!(last_not_iter.is_err());
  }

  #[test]
  fn count() {
    let count = run(indoc! {"
      let a = iter::empty() >> iter::count
      let b = iter::once(false) >> iter::count
      let c = iter::repeat(77) >> iter::count
      let d = iter::integers() >> iter::count
      let e = list::iter([1, 2, 4]) >> iter::count
    "});
    assert_variable!(count; a, 0.0);
    assert_variable!(count; b, 1.0);
    assert_variable!(count; c, f64::INFINITY);
    assert_variable!(count; d, f64::INFINITY);
    assert_variable!(count; e, 3.0);

    let count_not_iter = run("4 >> iter::count");
    assert!(count_not_iter.is_err());
  }

  #[test]
  fn any_all() {
    let any = run(indoc! {"
      let a = iter::empty() >> iter::any
      let b = iter::once(false) >> iter::any
      let c = iter::once(true) >> iter::any
      let d = iter::repeat(true) >> iter::any
      let e = list::iter([false, false, true]) >> iter::any
      let f = list::iter([false, false, false]) >> iter::any
    "});
    assert_variable!(any; a, false);
    assert_variable!(any; b, false);
    assert_variable!(any; c, true);
    assert_variable!(any; d, true);
    assert_variable!(any; e, true);
    assert_variable!(any; f, false);

    let all = run(indoc! {"
      let a = iter::empty() >> iter::all
      let b = iter::once(false) >> iter::all
      let c = iter::once(true) >> iter::all
      let d = iter::repeat(false) >> iter::all
      let e = list::iter([true, true, true]) >> iter::all
      let f = list::iter([true, false, true]) >> iter::all
    "});
    assert_variable!(all; a, true);
    assert_variable!(all; b, false);
    assert_variable!(all; c, true);
    assert_variable!(all; d, false);
    assert_variable!(all; e, true);
    assert_variable!(all; f, false);

    let any_not_iter = run("4 >> iter::any");
    assert!(any_not_iter.is_err());
    let all_not_iter = run("4 >> iter::all");
    assert!(all_not_iter.is_err());
  }

  #[test]
  fn find_position() {
    let mut find = run(indoc! {"
      let a = iter::empty() >> iter::find(x => x % 3 == 0) >> string::from
      let b = iter::repeat(6) >> iter::find(x => x % 3 == 0) >> string::from
      let c = iter::once(77) >> iter::find(x => x % 3 == 0) >> string::from
      let d = list::iter([1, 2, 4]) >> iter::find(x => x % 3 == 0) >> string::from
      let e = list::iter([1, 2, 3]) >> iter::find(x => x % 3 == 0) >> string::from

      let f = 'hello world' >> string::chars >> iter::find(char => char > 't') >> string::from
      let g = 'hello world' >> string::chars >> iter::find(char => char > 'z') >> string::from
    "});
    assert_variable!(find; a, string "None");
    assert_variable!(find; b, string "Some(6)");
    assert_variable!(find; c, string "None");
    assert_variable!(find; d, string "None");
    assert_variable!(find; e, string "Some(3)");
    assert_variable!(find; f, string "Some('w')");
    assert_variable!(find; g, string "None");

    let find_not_iter = run("4 >> iter::find(x => x)");
    assert!(find_not_iter.is_err());
    let find_not_callable = run("iter::empty() >> iter::find(5)");
    assert!(find_not_callable.is_err());

    let mut position = run(indoc! {"
      let a = iter::empty() >> iter::position(x => x % 3 == 0) >> string::from
      let b = iter::repeat(6) >> iter::position(x => x % 3 == 0) >> string::from
      let c = iter::once(77) >> iter::position(x => x % 3 == 0) >> string::from
      let d = list::iter([1, 2, 4]) >> iter::position(x => x % 3 == 0) >> string::from
      let e = list::iter([1, 2, 3]) >> iter::position(x => x % 3 == 0) >> string::from

      let f = 'hello world' >> string::chars >> iter::position(char => char > 't') >> string::from
      let g = 'hello world' >> string::chars >> iter::position(char => char > 'z') >> string::from
    "});
    assert_variable!(position; a, string "None");
    assert_variable!(position; b, string "Some(0)");
    assert_variable!(position; c, string "None");
    assert_variable!(position; d, string "None");
    assert_variable!(position; e, string "Some(2)");
    assert_variable!(position; f, string "Some(6)");
    assert_variable!(position; g, string "None");

    let position_not_iter = run("4 >> iter::position(x => x)");
    assert!(position_not_iter.is_err());
    let position_not_callable = run("iter::empty() >> iter::position(5)");
    assert!(position_not_callable.is_err());
  }

  #[test]
  fn map_inspect_filter() {
    let mut map = run(indoc! {"
      let a = iter::empty() >> iter::map(x => x + 1) >> iter::toList >> string::from
      let b = iter::once(false) >> iter::map(x => !x) >> iter::toList >> string::from

      let c = string::chars('hello') >> iter::map(string::toUppercase) >> iter::toList >> string::from
    "});
    assert_variable!(map; a, string "[]");
    assert_variable!(map; b, string "[true]");
    assert_variable!(map; c, string "['H', 'E', 'L', 'L', 'O']");

    let map_not_iter = run("4 >> iter::map(x => x)");
    assert!(map_not_iter.is_err());
    let map_not_callable = run("iter::empty() >> iter::map(5)");
    assert!(map_not_callable.is_err());

    let mut inspect = run(indoc! {"
      let a = iter::empty() >> iter::inspect(x => panic('not called')) >> iter::count
      let b = list::iter([1, 2, 3]) >> iter::inspect(x => !x) >> iter::toList >> string::from
      let c = list::iter([1, 2]) >> iter::inspect(x => `{x}`) >> iter::toList >> string::from
      let d = list::iter(['hello', 'hi']) >> iter::inspect(x => x) >> iter::toList >> string::from
    "});
    assert_variable!(inspect; a, 0.0);
    assert_variable!(inspect; b, string "[1, 2, 3]");
    assert_variable!(inspect; c, string "[1, 2]");
    assert_variable!(inspect; d, string "['hello', 'hi']");

    let inspect_not_iter = run("4 >> iter::inspect(x => x)");
    assert!(inspect_not_iter.is_err());
    let inspect_not_callable = run("iter::once(1) >> iter::inspect(5)");
    assert!(inspect_not_callable.is_err());

    let mut filter = run(indoc! {"
      let a = 'Hi 👋'
        >> string::chars
        >> iter::filter(string::isAscii)
        >> iter::toList
        >> string::from
      let b = list::iter([1, 2, 3]) >> iter::filter(_ => false) >> iter::count
      let c = list::iter([1, 2, 3]) >> iter::filter(_ => true) >> iter::count
    "});
    assert_variable!(filter; a, string "['H', 'i', ' ']");
    assert_variable!(filter; b, 0.0);
    assert_variable!(filter; c, 3.0);

    let filter_not_iter = run("4 >> iter::filter(x => x)");
    assert!(filter_not_iter.is_err());
    let filter_not_callable = run("iter::once(1) >> iter::filter(5)");
    assert!(filter_not_callable.is_err());
  }

  #[test]
  fn reduce_fold() {
    let mut reduce = run(indoc! {"
      let sum = iter::reduce(acc => x => acc + x)
      let product = iter::reduce(acc => x => acc * x)

      let a = iter::empty() >> sum >> string::from
      let b = iter::once(1) >> sum >> string::from
      let c = iter::once(1) >> product >> string::from
      let d = list::iter([1, 2, 4]) >> sum >> string::from
      let e = list::iter([1, 2, 4]) >> product >> string::from
      let f = string::chars('hello') >> iter::reduce(acc => x => acc ++ x) >> string::from
      let g = list::iter([1, 2, 4]) >> iter::map(x => x) >> sum >> string::from
      let h = list::iter([1, 2, 4]) >> iter::map(string::from) >> iter::reduce(acc => x => acc ++ x) >> string::from
    "});
    assert_variable!(reduce; a, string "None");
    assert_variable!(reduce; b, string "Some(1)");
    assert_variable!(reduce; c, string "Some(1)");
    assert_variable!(reduce; d, string "Some(7)");
    assert_variable!(reduce; e, string "Some(8)");
    assert_variable!(reduce; f, string "Some('hello')");
    assert_variable!(reduce; g, string "Some(7)");
    assert_variable!(reduce; h, string "Some('124')");

    let reduce_infinite = run("iter::integers() >> iter::reduce(x => x => x)");
    assert!(reduce_infinite.is_err());

    let reduce_not_iter = run("4 >> iter::reduce(x => x => x)");
    assert!(reduce_not_iter.is_err());
    let reduce_not_callable = run("iter::reduce(5)");
    assert!(reduce_not_callable.is_err());
    let reduce_not_callable_inner = run("list::iter([1, 2]) >> iter::reduce(x => 5)");
    assert!(reduce_not_callable_inner.is_err());

    let mut fold = run(indoc! {"
      let a = list::iter([1, 2, 3]) >> iter::fold(0)(acc => x => acc + x)
      let b = list::iter([1, 2, 3]) >> iter::fold(4)(acc => x => acc + x)
      let c = iter::empty() >> iter::fold(0)(acc => x => acc + x)
      let d = iter::empty() >> iter::fold('')(acc => x => acc ++ x)
      let e = string::chars('hello world') >> iter::fold('')(acc => x => acc ++ x)
      let f = list::iter([1, 2, 4]) >> iter::map(x => x) >> iter::fold(0)(acc => x => acc + x)
      let f = list::iter([1, 2, 4]) >> iter::map(string::from) >> iter::fold('')(acc => x => acc ++ x)
    "});
    assert_variable!(fold; a, 6.0);
    assert_variable!(fold; b, 10.0);
    assert_variable!(fold; c, 0.0);
    assert_variable!(fold; d, string "");
    assert_variable!(fold; e, string "hello world");
    assert_variable!(fold; f, string "124");

    let fold_infinite = run("iter::integers() >> iter::fold(0)(x => x => x)");
    assert!(fold_infinite.is_err());

    let fold_not_iter = run("4 >> iter::fold(0)(x => x => x)");
    assert!(fold_not_iter.is_err());
    let fold_not_callable = run("iter::empty() >> iter::fold(5)(5)");
    assert!(fold_not_callable.is_err());
    let fold_not_callable_inner = run("list::iter([1, 2]) >> iter::fold(0)(x => 5)");
    assert!(fold_not_callable_inner.is_err());
  }

  #[test]
  fn take_while() {
    let mut take_while = run(indoc! {"
      let a = (iter::empty() >> iter::takeWhile(x => x) >> iter::toList) == []
      let b = iter::integers() >> iter::takeWhile(x => x < 4) >> iter::toList >> string::from
    "});
    assert_variable!(take_while; a, true);
    assert_variable!(take_while; b, string "[0, 1, 2, 3]");

    let take_while_not_iter = run("4 >> iter::takeWhile(x => x)");
    assert!(take_while_not_iter.is_err());
    let take_while_not_callable = run("iter::empty() >> iter::takeWhile(5)");
    assert!(take_while_not_callable.is_err());
  }

  #[test]
  fn sum_product() {
    let sum = run(indoc! {"
      let a = iter::empty() >> iter::sum
      let b = iter::once(1) >> iter::sum
      let c = iter::integers() >> iter::takeWhile(x => x < 5) >> iter::sum
      let d = list::iter([1, 3, 5]) >> iter::sum
    "});
    assert_variable!(sum; a, 0.0);
    assert_variable!(sum; b, 1.0);
    assert_variable!(sum; c, 10.0);
    assert_variable!(sum; d, 9.0);

    let sum_not_iter = run("4 >> iter::sum");
    assert!(sum_not_iter.is_err());
    let sum_not_numbers = run("iter::once('') >> iter::sum");
    assert!(sum_not_numbers.is_err());
    let sum_infinfite = run("iter::repeat(5) >> iter::sum");
    assert!(sum_infinfite.is_err());

    let product = run(indoc! {"
      let a = iter::empty() >> iter::product
      let b = iter::once(9) >> iter::product
      let c = list::iter([1, 3, 5]) >> iter::product
      let d = list::iter([5, 4, 3]) >> iter::product
    "});
    assert_variable!(product; a, 1.0);
    assert_variable!(product; b, 9.0);
    assert_variable!(product; c, 15.0);
    assert_variable!(product; d, 60.0);

    let product_not_iter = run("4 >> iter::product");
    assert!(product_not_iter.is_err());
    let product_not_numbers = run("iter::once('') >> iter::product");
    assert!(product_not_numbers.is_err());
    let product_infinite = run("iter::repeat(5) >> iter::product");
    assert!(product_infinite.is_err());
  }

  #[test]
  fn join() {
    let mut simple = run(indoc! {"
      let a = list::iter([]) >> iter::join('')
      let b = list::iter([]) >> iter::join('-')
      let c = string::chars('abc') >> iter::join('-')
      let d = string::chars('abc') >> iter::join('')
    "});
    assert_variable!(simple; a, string "");
    assert_variable!(simple; b, string "");
    assert_variable!(simple; c, string "a-b-c");
    assert_variable!(simple; d, string "abc");

    let mut from_unknown_sized_iterator = run(indoc! {"
      let a = iter::integers()
        >> iter::takeWhile(x => x < 4)
        >> iter::map(string::from)
        >> iter::join('')
    "});
    assert_variable!(from_unknown_sized_iterator; a, string "0123");

    let infinite_iter = run("iter::repeat(1) >> iter::join('')");
    assert!(infinite_iter.is_err());

    let non_iterator = run("6 >> iter::join('')");
    assert!(non_iterator.is_err());

    let no_joining_part = run("iter::empty() >> iter::join");
    assert!(no_joining_part.is_err());

    let non_string_joining_part = run("iter::empty() >> iter::join(7)");
    assert!(non_string_joining_part.is_err());
  }
}

mod docs {
  use bang_interpreter::stdlib::{ITER_ITEMS, LIST_ITEMS, MATHS_ITEMS, OPTION_ITEMS, STRING_ITEMS};
  use bang_interpreter::stdlib::{iter_docs, list_docs, maths_docs, option_docs, string_docs};
  use indoc::indoc;

  #[test]
  #[cfg_attr(miri, ignore)]
  fn typecheck() {
    for item in STRING_ITEMS {
      typecheck_docs("string", item, string_docs(item));
    }

    for item in MATHS_ITEMS {
      typecheck_docs("maths", item, maths_docs(item));
    }

    for item in LIST_ITEMS {
      typecheck_docs("list", item, list_docs(item));
    }

    for item in OPTION_ITEMS {
      typecheck_docs("option", item, option_docs(item));
    }

    for item in ITER_ITEMS {
      typecheck_docs("iter", item, iter_docs(item));
    }
  }

  #[test]
  #[cfg_attr(miri, ignore)]
  fn lint() {
    for item in STRING_ITEMS {
      lint_docs("string", item, string_docs(item));
    }

    for item in MATHS_ITEMS {
      lint_docs("maths", item, maths_docs(item));
    }

    for item in LIST_ITEMS {
      lint_docs("list", item, list_docs(item));
    }

    for item in OPTION_ITEMS {
      lint_docs("option", item, option_docs(item));
    }

    for item in ITER_ITEMS {
      lint_docs("iter", item, iter_docs(item));
    }
  }

  #[test]
  #[cfg_attr(miri, ignore)]
  fn format() {
    for item in STRING_ITEMS {
      format_docs("string", item, string_docs(item));
    }

    for item in MATHS_ITEMS {
      format_docs("maths", item, maths_docs(item));
    }

    for item in LIST_ITEMS {
      format_docs("list", item, list_docs(item));
    }

    for item in OPTION_ITEMS {
      format_docs("option", item, option_docs(item));
    }

    for item in ITER_ITEMS {
      format_docs("iter", item, iter_docs(item));
    }
  }

  /// Gets an example from a docstring if it exists
  fn get_doc_example(docs: &str) -> String {
    docs
      .lines()
      .skip_while(|line| !line.trim_end().ends_with("```bang"))
      .skip(1)
      .take_while(|line| !line.trim_end().ends_with("```"))
      .map(|line| if line.is_empty() { "" } else { &line[1..] })
      .fold(String::new(), |a, b| a + b + "\n")
      .trim_end()
      .to_string()
  }

  fn generate_error_message(
    module: &str,
    item: &str,
    source: &str,
    error: &str,
    error_kind: &str,
  ) -> String {
    format!(
      indoc! {"
      `{}::{}`'s documentation example {}

      ```bang
      {}
      ```

      Errors:

      {}
    "},
      module,
      item,
      error_kind,
      source,
      error.trim_end()
    )
  }

  fn parse_docs(module: &str, item: &str, docs: &str) -> bang_syntax::AST {
    let doc_example = get_doc_example(docs);

    let ast = bang_syntax::parse(doc_example);
    if !ast.is_valid() {
      let errors = (ast.errors.iter())
        .map(|error| error.full_message())
        .fold(String::new(), |a, b| a + b.as_str() + "\n\n");

      panic!(
        "{}",
        generate_error_message(module, item, &ast.source, &errors, "could not be parsed")
      );
    }
    ast
  }

  fn typecheck_docs(module: &str, item: &str, docs: Option<&'static str>) {
    let ast = parse_docs(module, item, docs.unwrap_or(""));

    let type_checker = bang_typechecker::TypeChecker::check(&ast);
    if !type_checker.problems().is_empty() {
      let errors = (type_checker.problems().iter())
        .map(|error| error.full_message())
        .fold(String::new(), |a, b| a + b.as_str() + "\n\n");

      panic!(
        "{}",
        generate_error_message(module, item, &ast.source, &errors, "has a type error")
      );
    }
  }

  fn lint_docs(module: &str, item: &str, docs: Option<&'static str>) {
    let ast = parse_docs(module, item, docs.unwrap());

    let problems = bang_linter::lint(&ast);
    if !problems.is_empty() {
      let errors = problems
        .iter()
        .map(|error| error.full_message())
        .fold(String::new(), |a, b| a + b.as_str() + "\n\n");

      panic!(
        "{}",
        generate_error_message(module, item, &ast.source, &errors, "has a lint warning")
      );
    }
  }

  fn format_docs(module: &str, item: &str, docs: Option<&'static str>) {
    use bang_formatter::{FormatterConfig, config::LineEnding};

    let ast = parse_docs(module, item, docs.unwrap());

    let format_config = FormatterConfig {
      print_width: 80,
      single_quotes: true,
      indentation: 2.into(),
      line_ending: LineEnding::LineFeed,
      sort_imports: true,
    };
    let formatted_code = bang_formatter::format(&ast, format_config);

    assert_eq!(
      ast.source.trim(),
      formatted_code.trim(),
      "`{module}::{item}`'s documentation example is not formatted"
    );
  }
}
