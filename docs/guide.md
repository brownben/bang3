# A Guide to Bang

## Comments

Comments are anything that comes after two forward slashes `//`. They can be on their own line, or after an expression.

Bang is relatively strict about where they can occur. If an expression is broken across multiple lines comments can't appear in between.

```bang
// this is a comment
4 + 5 // this is a comment
```

## Basic Types

### Numbers

Numbers are IEEE-754 double-precision floating point numbers. They can have underscores between the numbers for readability.

```bang
1
2.0
155245.25
1_000_000
5_000_000.0
```

Two numbers are equal if they are within the difference of the smallest representable number.

```bang
0.1 + 0.2 == 0.3
```

#### Operators

| Operator | Name                   | Example                    |
| -------- | ---------------------- | -------------------------- |
| `+`      | Addition               | `1 + 4.5`                  |
| `-`      | Subtraction            | `55 - 21`                  |
| `-`      | Negation               | `-21`                      |
| `*`      | Multiplication         | `7 * 3`                    |
| `/`      | Division               | `21.4 / 0.2`               |
| `%`      | Remainder              | `3 % 2 == 1`               |
| `!`      | Not                    | `!0 == true` `!1 == false` |
| `>`      | Greater Than           | `7 > 2`                    |
| `>=`     | Greater Than or Equals | `8 >= 7.2`                 |
| `<`      | Less Than              | `85.12 < 1254.63`          |
| `<=`     | Less Than or Equals    | `-2553.2 <= 1254.63`       |

Various mathematical functions can be found in the [standard library](./stdlib_docs.md#maths).

### Strings

Strings are utf-8 encoded and can be created with single (`'`) or double quotes (`"`). There are no escape sequences.

Two strings are equal if they have the same contents.

```bang
'They can have single quotes'
"or they can have double quotes"
'they
  can
    even
      span
    multiple
  lines'
```

#### Format Strings

Backticks (`` ` ``) can be used to create format strings, where expressions between curly brackets in the string are evaluated and converted to a string in place.

```bang
let name = 'John'
`hello, {name}!` // 'hello, John!'
`9 + 10 = {9 + 10}` // '9 + 10 = 19'
```

#### Operators

| Operator | Name                   | Example                           |
| -------- | ---------------------- | --------------------------------- |
| `++`     | Concatenation          | `'a' ++ 'b' == 'ab'`              |
| `!`      | Not                    | `!'' == true` `!'hello' == false` |
| `>`      | Greater Than           | `'b' > 'a'`                       |
| `>=`     | Greater Than or Equals | `'a' >= 'a'`                      |
| `<`      | Less Than              | `'A' < 'a'`                       |
| `<=`     | Less Than or Equals    | `'cat' >= 'c'`                    |

More functions for operating on strings can be found in the [standard library](./stdlib_docs.md#string).

### Booleans

```bang
true
false
```

#### Operators

| Operator      | Name        | Example   |
| ------------- | ----------- | --------- |
| `!`           | Logical Not | `!x`      |
| `and` / `&&`  | Logical And | `a and b` |
| `or` / `\|\|` | Logical Or  | `x or y`  |

## Compound Types

### List

Lists are an ordered collection of values. They are immutable and are defined between square brackets.

```bang
[1, 2, 3]

// you can store different typed values, but the type-checker doesn't support this
[1, 'hello', false, []]
```

Two lists are equal if they are the same length, and all the inner values are equal to each other.
Lists are truthy if they are non-empty, and falsy if empty.

Functions for operating on lists can be found in the [standard library](./stdlib_docs.md#list).

### Options

Type `option` represents an optional value: every `option` is either `Some` and contains a value, or `None`, and does not.

They can be used for a variety of tasks, including:

- simple errors
- optional function arguments
- functions which aren't defined for all inputs

Options are commonly paired with pattern matching to query the presence of a value and take action, always accounting for the `None` case.

All `None` values are equal, and two `Some` values are equal if the values they contain are equal.
`None` is falsy, and all `Some` values are truthy.

`Some` and `None` are both in the prelude, so they are already defined and can be used without importing anything.

```bang
Some(5)
None
```

Functions for operating on options can be found in the [standard library](./stdlib_docs.md#option).

### Iterators

A series of values. The easiest way to work with values from some collection, and perform a series of operations on them. Iterators are lazy, and will not perform any action until an output is requested, this allows infinite iterators to be created and worked with.

Iterators only have referential equality and are only equal if they point to the same object.
All iterators are truthy.

Functions for operating on iterators can be found in the [standard library](./stdlib_docs.md#iter).

## Variables

`let` introduces a variable binding. Variables are immutable, so can't be mutated or reassigned. Variable declarations are statements that don't have a value and can't be used as an expression.

```bang
let x = 42
```

You can optionally specify a type annotation for the variable, these are unused by the runtime but can be used by the type-checker to verify your code. The type-checker can infer the type, so most of the time annotations are not recommended.

```bang
let x: number = 42
```

Variables are lexically scoped and can be used anywhere after they are defined until the end of the block they appear in.

```bang
let x = 5

// I can use `x` here

{
  let y = 4

  // I can use `x` or `y` here
}

// I can use `x` here
```

Variables are accessed/ used by just using their name/ identifier. Identifiers are anything which matches `/[_a-zA-Z][_a-zA-Z0-9]*/`.

```bang
let a = 5
let b = 4

a + b
```

Separate bindings with the same name are allowed, you can "shadow" a variable. This can be used to give the impression of mutability.

```
let x = 13
let x = x + 3
```

## Blocks

A pair of curly brackets declares a block.

```bang
{
  // some code
}
```

Blocks are expressions which evaluate to a value. They can contain multiple statements, however, the last item must be an expression which is what the whole block will evaluate to.

```bang
let x = 42

// is the same as
let x = { 42 }

let y = {
  let a = 2
  let b = 3

  a * b // `y` will be set to the result of this expression
}
```

## Functions

Functions take a single parameter and perform some expression with it.
Functions are made with `=>`, with the parameter identifier preceding it, and the body to evaluate to a result following it.

```bang
x => x + 1

// `x` is the parameter
// `x + 1` is the body to evaluate
```

If a function is declared in a variable declaration, it is named the variable name. This name is visible if you print the function, or in stack traces.

```bang
let addOne = x => x + 1
// the function has the name `addOne`
```

Functions are called with brackets (`(`, `)`), with the argument for the function to be called with between.

```bang
addOne(5)
```

If a function doesn't use the parameter, an argument can be omitted.

```bang
let alwaysFive = _ => 5
alwaysFive()
alwaysFive(2) // can pass an argument but it has no effect
```

Functions are always truthy and only have referential equality. So a function will only be equal if it is pointing to exactly the same object.

### Closures

Functions can reference values in higher scopes.

```bang
let outer = _ => {
  let value = 'hello'

  // inner captures `value`, even though when it is called `value` is not in scope
  let inner = _ => value

  inner
}

outer()()

```

This can be used to have functions with multiple arguments. As functions can return other functions which reference the value passed as an argument. These inner functions don't need to be called straight away giving partial application for functions.

```bang
let multipleArgs = x => y => x + y

// is the same as
let multipleArgs = x => {
  y => x + y
}
// a function which returns another function, which has captured the first argument

// therefore we can have partial application
let first = multipleArgs(5)
first(1) // 6
first(7) // 12
```

### Early Returns

If you want to return before the end of a block, you can use a `return` statement. It returns the expression after the return immediately.
As it is a statement not an expression, you may need to wrap it in curly braces to form a block around it.

```bang
let aFunction = x => {
  if (x < 0) { return x }

  // do something else, now we know x is not negative
  x + 2
}
```

### Pipeline Operator

You often want to pass the result of one function as the input to the next. If you have many nested functions they can be hard to read. The pipeline operator (`>>`) can make this clearer, by writing them from top to bottom.

The operator takes the result of the left expression and passes it as an argument to the function on the right.

Most functions in the standard library are designed to be convenient to work with function piping. If you want to pass it as the last argument to a function, you can introduce a new anonymous.

```bang
let input = '4
5
8
7'

// standard function calls
iter::toList(iter::filter(x => x % 2 == 0)(iter::filterMap(string::parseNumber)(string::lines(input))))

// the equivalent using the pipeline operator
input
  >> string::lines
  >> iter::filterMap(string::parseNumber)
  >> iter::filter(x => x % 2 == 0)
  >> iter::toList
```

## Control Flow

### If/ Else

If you want to change actions taken depending on a condition an `if`/`else` expression can be used. They can go anywhere that accepts an expression.

If the condition (in brackets after the `if`) is true the first expression is executed, otherwise the expression after the `else` is executed.

```bang
if (condition) doIfConditionIsTrue else doIfConditionIsFale
```

They can be chained together to get if/ else if/ else.

```bang
if (condition) then
else if (otherCondition) otherThen
else otherwise
```

If the value of the expression is not used, the `else` and otherwise expression can be omitted. This can be useful for early returns.

```bang
if (condition) doThis
// we don't need an else
```

To set a variable to the result of a condition, you use an `if` as an expression.

```bang
let kind = if (size > 5) 'large' else 'small'
```

### Match

Similar to a `switch` case in some other languages, a `match` expression allows execution to branch depending on the value. The value is compared against all of the patterns, and the first pattern to match has its arm executed.

The type-checker has support for exhaustiveness checking, it will ensure that every possible value is covered by an arm.

```bang
match value
  | pattern -> armExpression
  | pattern2 -> otherExpression
```

There are several different types of patterns:

- **Variable**: just an identifier, will match any value (a catch-all)
- **Literal**: a specific literal value (a boolean, number, or string)
- **Range**: a range of numbers/ strings (can have an upper bound, a lower bound, or both)
- **List**: matches list
- **Option**: either matches a `Some` with a value, or a `None`

Patterns cannot currently be nested inside of each other, so the inside of a `Some` has to be a variable pattern, and the elements of a list have to be variable patterns.

```bang
match x
  | variable -> 0 // `variable` is defined and is the match value `x`, would be the only arm ever executed
  | true -> 1 // literal pattern
  | 4.5 -> 2 // literal pattern
  | "hello" -> 3 // literal pattern
  | ..2 -> 4 // range pattern - numbers less than or equal to 2
  | 8.. -> 5 // range pattern - numbers greater than or equal to 8
  | 2..3 -> 6 // range pattern - numbers between 2 and 3 inclusive of both
  | [] -> 7 // list pattern - empty
  | [a] -> 8 // list pattern - single element, which is then bound to `a`
  | [a, ..b] -> 9 // list pattern - with at least one element, the first is bound to `a` and the rest of the list is bound to `b`
  | Some(c) -> 10 // option pattern
  | None -> 11 // option pattern
```

An `if` condition can go after the pattern as a "guard" and the arm will only execute if it matches the pattern, and the guard condition is true. Any variables bound in the pattern will be active for the guard condition.

```bang
match x
  | Some(x) if x > 10 -> 1 // this is only taken if is `Some` and `x > 10`
  | Some(_) -> 2
  | None -> 3
```

### Looping

Unlike other languages, there are no specialised concepts/ structures for looping in Bang.

Instead, you can use functions and recursion, or iterators!

````bang
// for example the following python:
// ```python
// total = 0
// for x in [1, 2, 3]:
//   print(x)
//   total += x
// ```

// could be written with iterators as:
let total = list::iter([1, 2, 3]) >> iter::inspect(print) >> iter::fold(0)(acc => x => acc + x)

// or with recursion as:
let sum = list => match list
  | [] -> 0
  | [x, ..rest] -> print(x) + sum(rest)
let total = sum([1, 2, 3])
````

## Imports

Bang doesn't yet have a way to link multiple Bang files together. All your code currently needs to be in the same file.

However, you can import values from the standard library.

```bang
// this imports `comma`, `separated`, and `items` from module and defines them
from module import { comma, separated, items }
```

Import statements can occur anywhere, they don't have to only be at the top of a file.

The `as` keyword can be used to rename what the item is defined as.

```bang
from maths import { asin as inverseSin }
// `inverseSin` is defined, `asin` is not
```

If you are only using the imported item a handful of times you might not want to import it. Instead, you can use it with module access.

```bang
maths::sin(0) // just imports `sin` from the `maths` module for this use
```

## Errors

Bang is strongly typed, and if an operation happens on a type which it doesn't expect it will raise an error which will stop all execution.
There is no way to recover from these errors, however they can be detected by the type-checker.

If an operation is fallible its API should indicate that and give you options of how to handle it safely, for example by using the `option` type.

Some of the standard library functions can also panic and stop execution if an invariant is not upheld (for example producing a list from an infinite integer). These cannot be statically detected, however, these invariants are clearly described in the item's documentation.

### Panic

If you want to stop execution because an error has occurred, you can use the `panic` function in the prelude.
It stops execution immediately and displays its argument as the error message.

```bang
if (somethingBad) panic("Something bad has happened")
// execution is stopped and
```
