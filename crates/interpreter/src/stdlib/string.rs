use super::macros::{module, prelude::*};

module!(string, STRING_ITEMS, string_docs, {
  const /// A newline character
        NEW_LINE: String = "\n";
  const // A tab character
        TAB: String = "\t";
  const // A carriage return character
        CARRIAGE_RETURN: String = "\r";

  /// Converts a value to a string
  ///
  /// ```bang
  /// from string import { toString }
  ///
  /// toString(false) // 'false'
  /// toString('hello') // 'hello'
  /// toString(x => x + 1) // '<function>'
  /// ```
  fn toString(Any) -> StringOrSelf = |vm, value| Value::display(&value, vm);
  /// Converts a value to a string
  ///
  /// Re-export of `string::toString` to make it more natural to use with
  /// module access expressions. (e.g. `string::from(5)`).
  ///
  /// ```bang
  /// string::from(false) // 'false'
  /// string::from('hello') // 'hello'
  /// string::from(x => x + 1) // '<function>'
  /// ```
  fn from(Any) -> StringOrSelf = |vm, value| Value::display(&value, vm);

  /// The number of characters in the string
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::length // 5
  /// '🏃' >> string::length // 1
  /// ```
  fn length(String) -> usize = |s: &str| s.chars().count();
  /// The size of the string's buffer in bytes
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::byteLength // 5
  /// '🏃' >> string::byteLength // 4
  /// ```
  fn byteLength(String) -> usize = str::len;

  /// Is the string empty?
  ///
  /// ## Example
  /// ```bang
  /// '' >> string::isEmpty // true
  /// 'a' >> string::isEmpty // false
  /// ```
  fn isEmpty(String) -> bool = str::is_empty;
  /// Are all the characters in the string ASCII?
  ///
  /// ## Example
  /// ```bang
  /// 'hello!' >> string::isAscii // true
  /// 'hiya 👋' >> string::isAscii // false
  /// ```
  fn isAscii(String) -> bool = str::is_ascii;

  /// Returns a string with all characters converted to lowercase
  ///
  /// (`Lowercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::toLowercase // 'hello'
  /// 'HELLO' >> string::toLowercase // 'hello'
  /// ```
  fn toLowercase(String) -> String = str::to_lowercase;
  /// Returns a string with all characters converted to uppercase
  ///
  /// (`Uppercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::toUppercase // 'HELLO'
  /// 'HELLO' >> string::toUppercase // 'HELLO'
  /// ```
  fn toUppercase(String) -> String = str::to_uppercase;

  /// Is the string a substring of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'welcome to earth' >> string::contains('come') // true
  /// 'welcome to earth' >> string::contains('hello') // false
  /// ```
  fn contains(String, String) -> bool = |pattern, string| str::contains(string, pattern);
  /// Is a string a prefix of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::startsWith('he') // true
  /// 'hello' >> string::startsWith('lo') // false
  /// ```
  fn startsWith(String, String) -> bool = |pattern, string| str::starts_with(string, pattern);
  /// Is a string a suffix of the given string?
  ///
  /// ## Example
  /// ```bang
  /// 'hello' >> string::endsWith('lo') // true
  /// string::endsWith('he')('hello') // false
  /// ```
  fn endsWith(String, String) -> bool = |pattern, string| str::ends_with(string, pattern);

  /// Returns a string with leading and trailing whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trim('  hello  ') // 'hello'
  /// ```
  fn trim(String) -> StringSlice = str::trim;
  /// Returns a string with leading whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trimStart('  hello') // 'hello'
  /// ```
  fn trimStart(String) -> StringSlice = str::trim_start;
  /// Returns a string with trailing whitespace removed
  ///
  /// (`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)
  ///
  /// ## Example
  /// ```bang
  /// string::trimEnd('hello   ') // 'hello'
  /// ```
  fn trimEnd(String) -> StringSlice = str::trim_end;


  /// Replace all occurrences of a pattern in a string with a replacement.
  /// Returns a new string.
  ///
  /// ## Example
  /// ```bang
  /// 'this is old' >> string::replaceAll('old')('new') // 'this is new'
  /// 'this is old' >> string::replaceAll('i')('I') // 'thIs Is new'
  /// ```
  fn replaceAll(String, String, String) -> String = |pat, rep, str| str::replace(str, pat, rep);

  /// Replace the first occurrence of a pattern in a string with a replacement.
  /// Returns a new string.
  ///
  /// ## Example
  /// ```bang
  /// 'this is old' >> string::replaceOne('old')('new') // 'this is new'
  /// 'this is old' >> string::replaceOne('i')('I') // 'thIs is new'
  /// ```
  fn replaceOne(String, String, String) -> String = |pat, rep, str| str::replacen(str, pat, rep, 1);
});
