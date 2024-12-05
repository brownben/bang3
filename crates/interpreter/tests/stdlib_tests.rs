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
