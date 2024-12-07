# Bang Standard Library

Documentation for the Bang Language Standard Library.

**Modules:**
- [string](#string)
- [maths](#maths)
- [list](#list)
- [option](#option)


## string

### NEW_LINE *string*

A newline character

### TAB *string*

A tab character

### CARRIAGE_RETURN *string*

A carriage return character

### toString *(^a => string)*

Converts a value to a string

```bang
from string import { toString }

toString(false) // 'false'
toString('hello') // 'hello'
toString(x => x + 1) // '<function>'
```

### from *(^a => string)*

Converts a value to a string

Re-export of `string::toString` to make it more natural to use with
module access expressions. (e.g. `string::from(5)`).

```bang
string::from(false) // 'false'
string::from('hello') // 'hello'
string::from(x => x + 1) // '<function>'
```

### length *(string => number)*

The number of characters in the string

```bang
'hello' >> string::length // 5
'🏃' >> string::length // 1
```

### byteLength *(string => number)*

The size of the string's buffer in bytes

```bang
'hello' >> string::byteLength // 5
'🏃' >> string::byteLength // 4
```

### isEmpty *(string => boolean)*

Is the string empty?

```bang
'' >> string::isEmpty // true
'a' >> string::isEmpty // false
```

### isAscii *(string => boolean)*

Are all the characters in the string ASCII?

```bang
'hello!' >> string::isAscii // true
'hiya 👋' >> string::isAscii // false
```

### toLowercase *(string => string)*

Returns a string with all characters converted to lowercase

(`Lowercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)

```bang
'hello' >> string::toLowercase // 'hello'
'HELLO' >> string::toLowercase // 'hello'
```

### toUppercase *(string => string)*

Returns a string with all characters converted to uppercase

(`Uppercase` is defined according to the terms of the Unicode Derived Core Property Lowercase)

```bang
'hello' >> string::toUppercase // 'HELLO'
'HELLO' >> string::toUppercase // 'HELLO'
```

### contains *(string => string => boolean)*

Is the string a substring of the given string?

```bang
'welcome to earth' >> string::contains('come') // true
'welcome to earth' >> string::contains('hello') // false
```

### startsWith *(string => string => boolean)*

Is a string a prefix of the given string?

```bang
'hello' >> string::startsWith('he') // true
'hello' >> string::startsWith('lo') // false
```

### endsWith *(string => string => boolean)*

Is a string a suffix of the given string?

```bang
'hello' >> string::endsWith('lo') // true
string::endsWith('he')('hello') // false
```

### trim *(string => string)*

Returns a string with leading and trailing whitespace removed

(`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)

```bang
string::trim('  hello  ') // 'hello'
```

### trimStart *(string => string)*

Returns a string with leading whitespace removed

(`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)

```bang
string::trimStart('  hello') // 'hello'
```

### trimEnd *(string => string)*

Returns a string with trailing whitespace removed

(`Whitespace` is defined according to the terms of the Unicode Derived Core Property White_Space, which includes newlines)

```bang
string::trimEnd('hello   ') // 'hello'
```

### replaceAll *(string => string => string => string)*

Replace all occurrences of a pattern in a string with a replacement.
Returns a new string.

```bang
'this is old' >> string::replaceAll('old')('new') // 'this is new'
'this is old' >> string::replaceAll('i')('I') // 'thIs Is new'
```

### replaceOne *(string => string => string => string)*

Replace the first occurrence of a pattern in a string with a replacement.
Returns a new string.

```bang
'this is old' >> string::replaceOne('old')('new') // 'this is new'
'this is old' >> string::replaceOne('i')('I') // 'thIs is new'
```

## maths

### PI *number*

The mathematical constant `π` (~3.141592…)

### E *number*

The mathematical constant `e` (Euler's number, ~2.718281…)

### INFINITY *number*

Floating point infinity (`∞`)

### NAN *number*

Not a Number (NaN)

### floor *(number => number)*

Returns the largest integer less than or equal to a number

```bang
maths::floor(1.2) // 1
maths::floor(1) // 1
maths::floor(-3.4) // -4
```

### ceil *(number => number)*

Returns the smallest integer greater than or equal to a number

```bang
maths::ceil(1.2) // 2
maths::ceil(1) // 1
maths::ceil(-3.4) // -3
```

### round *(number => number)*

Returns the closest integer to a number
If a value is half-way between two integers, round away from 0.0.

```bang
maths::round(1.2) // 1
maths::round(1) // 1
maths::round(-3.4) // -3
maths::round(1.5) // 2
maths::round(-1.5) // -2
```

### abs *(number => number)*

Returns the absolute value of a number

```bang
maths::abs(1.2) // 1.2
maths::abs(-1.2) // 1.2
```

### isNan *(number => boolean)*

Is the value `NaN`?

```bang
maths::isNan(1.2) // false
maths::isNan(maths::NAN) // true
```

### sin *(number => number)*

Computes the sine of a number (in radians)

```bang
maths::sin(0) // 0
maths::sin(maths::PI / 2) // 1
```

### cos *(number => number)*

Computes the cosine of a number (in radians)

```bang
maths::cos(0) // 1
maths::cos(maths::PI / 2) // 0
```

### tan *(number => number)*

Computes the tangent of a number (in radians)

```bang
maths::tan(0) // 0
maths::tan(maths::PI / 4) // 1
```

### asin *(number => number)*

Computes the arcsine of a number

Return value is in radians in the range [-PI/2, PI/2] or NaN if the number is outside the range [-1, 1].

```bang
maths::asin(0) // 0
maths::asin(1) // PI / 2
```

### acos *(number => number)*

Computes the arccosine of a number

Return value is in radians in the range [0, PI] or NaN if the number is outside the range [-1, 1].

```bang
maths::acos(1) // 0
maths::acos(0) // PI / 2
```

### atan *(number => number)*

Computes the arctangent of a number

Return value is in radians in the range [-PI/2, PI/2].

```bang
maths::atan(0) // 0
maths::atan(maths::tan(1)) // 1
```

### sinh *(number => number)*

Hyperbolic sine function

### cosh *(number => number)*

Hyperbolic cosine function

### tanh *(number => number)*

Hyperbolic tangent function

### asinh *(number => number)*

Inverse hyperbolic sine function

### acosh *(number => number)*

Inverse hyperbolic cosine function

### atanh *(number => number)*

Inverse hyperbolic tangent function

### sqrt *(number => number)*

Returns the square root of a number

Returns NaN if self is a negative number other than -0.0.

```bang
maths::sqrt(9) // 3
maths::sqrt(0) // 0
maths::sqrt(-1) >> maths::isNan // true
```

### cbrt *(number => number)*

Returns the cube root of a number

Returns NaN if self is a negative number other than -0.0.

```bang
maths::cbrt(8) // 2
maths::cbrt(0) // 0
maths::cbrt(-1) >> maths::isNan // true
```

### exp *(number => number)*

Returns e raised to the power of a number
(where e = 2.718281…, the base of natural logarithms)

```bang
maths::exp(0) // 1
maths::exp(1) // ~2.718281…
```

### ln *(number => number)*

Returns the natural logarithm of a number

```bang
maths::ln(1) // 0
maths::ln(maths::E) // 1
```

### pow *(number => number => number)*

Raises a number to the power of a number

```bang
maths::pow(2)(3) // 8
maths::pow(2)(4) // 16
maths::pow(13)(2) // 169
```

### log *(number => number => number)*

Returns the logarithm of the number with respect to an arbitrary base

The first argument is the base, and the second is the number.

```bang
maths::log(2)(8) // 3

let log10 = maths::log(10)
log10(100) // 2
```

### radiansToDegrees *(number => number)*

Convert an angle from radians to degrees

```bang
maths::radiansToDegrees(maths::PI) // 180.0
maths::radiansToDegrees(2 * maths::PI) // 360.0
```

### degreesToRadians *(number => number)*

Convert an angle from degrees to radians

```bang
maths::degreesToRadians(180) // PI
maths::degreesToRadians(360) // 2 * PI
```

## list

### length *(list => number)*

The number of elements in a list

```bang
[2, 3, 5, 7, 11] >> list::length // 5
[2] >> list::length // 1
```

### isEmpty *(list => boolean)*

Is the list empty?

```bang
[] >> list::isEmpty // true
[3, 4] >> list::isEmpty // false
```

### contains *(^a => list<^a> => boolean)*

Is the given item in the list?

```bang
[1, 3, 5] >> list::contains(3) // true
['a', 'big', 'dog'] >> list::contains('hello') // false
```

## option

### None *option*

No value

### Some *(^a => option<^a>)*

Some value

### isNone *(option => boolean)*

Is the value `None`?

```bang
option::isNone(option::None) // true
option::isNone(option::Some(1)) // false
```

### isSome *(option => boolean)*

Is the value `Some`?

```bang
option::isSome(option::Some(1)) // true
option::isSome(option::None) // false
```

### unwrap *(option<^a> => ^a)*

Returns the contained `Some` value, panics if the value is a `None`.

```bang
option::Some(5) >> option::unwrap // 5
option::None >> option::unwrap // panic, execution stopped
```

### unwrapOr *(^a => option<^a> => ^a)*

Returns the contained Some value or a provided default.

```bang
option::Some(5) >> option::unwrapOr(0) // 5
option::None >> option::unwrapOr(0) // 0
```

### flatten *(option<option<^a>> => option<^a>)*

Merges a nested Option into a single layer.

Converts from `option<option<T>>` to `option<T>`

```bang
option::flatten(option::Some(option::None)) // None
option::flatten(option::Some(option::Some(5))) // Some(5)
```

### map *((^a => ^b) => option<^a> => option<^b>)*

Maps an `Option<T>` to a `Option<U>` by applying a function to a contained value
(if `Some`) or returns `None` (if `None`).

```bang
option::Some(5) >> option::map(x => x + 1) // Some(6)
option::None >> option::map(x => x + 1) // None

option::Some('example') >> option::map(string::length) // Some(7)
option::None >> option::map(string::length) // None
```

