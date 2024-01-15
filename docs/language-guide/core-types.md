# Core Types

The following types are provided by the core language runtime.

All of the core types have [Value semantics](https://en.wikipedia.org/wiki/Value_semantics). These are compared by value and are copied into functions by value.

## Boolean

The Boolean type is written as `Bool`. Values of this type are either `true` or `false`.

### Boolean - Examples

```rs
let is_tired = true
let likes_candy = false
```

### Equality

Two values that are both `true` or both `false` are equal to each other.

| Left  | Right | Result |
| ----- | ----- | ------ |
| true  | true  | true   |
| true  | false | false  |
| false | true  | false  |
| false | false | true   |

## Integer

The Integer type is written as `Int`. This is a 64-bit integer and values of this type are integers from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 .

### Integer - Examples

```rs
// Inferred types
let meters_in_km = 1000
let oldest_turtle_age = 188

// Underscores can help readability
let distance_from_earth = 150_000_000
```

### Integer - Equality

Integers may be compared for equality.

```rs
// These statements are true

100 == 100    // ✅
-33 != -99    // ✅
```

## Float

The

### Integer and Float - Equality

Integers and floating point numbers may not be compared for equality.

```rs

// These statements are a compile error from the type checker

1.6 == 1   // ❌  cannot compare Float to Int
1.0 == 1   // ❌  cannot compare Float to Int
```

## String

Strings are UTF-8 encoded, and can be declared using the `String` core type.

```rs
let message = "Hello, world!"
let message_jp = "こんにちは"
```

Strings do not allow for random access at a byte index unless the string is known to only contain ASCII characters, see the [Type Narrowing](../type-system/type-narrowing) section for more detail.

### String - Interpolation

[String interpolation](https://en.wikipedia.org/wiki/String_interpolation) is achieved by placing expressions inside the `{` and `}` characters as delimiters.

The value of the expression must be able to be converted to a String. See [String Conversion](TODO) for more details.

```rs
let message = "Greetings, {user}. Last logged in: {last_login_date}."
let high_score = "High score: {scores.max}"
```

The expression inside of the `{` and `}` delimiters can be any valid expression, including a [block](). However, carefully consider code readability and prefer to only use simple expressions in string interpolation.

```rs
// Hard to read, don't do this
let total_score_message = "Total score: {{
    let mut total = 0
    for score in scores {
        total += score
    }
    total
}}"

// Instead, consider separating complex expressions to a separate variable
let total_score = {
    let mut total = 0
    for score in scores {
        total += score
    }
    total
}
let total_score_message = "Total score: {total_score}"
```

### String - Escape Characters

- `\"`: strings are delimited by `"` characters, so any literal `"` character that should appear in a string must be escaped.
- `\{`: The `{` character in a string is used to create a block for string interpolation so any literal `{` character must be escaped.
- `\\`: The `\` character is used to escape the next character, and thus must be escaped itself.

```rs
// Escaping quotes
let message = "Then she said \"Hello, World\"!"

// Escaping left-curly
let docs = "The \{ and } characters are delimiters for a block.".

// Escaping backslash
let note = "Backslash is normally an escape character, so to write \\ in a String you need to escape it."
```

### String - Equality

String can be compared for equality.

```rs
"hello" == "hello"   // ✅ true
"hello" != "bye"     // ✅ true
```

## Array

Arrays are contiguous values of the same type with a fixed length and capacity. For a similar collection that can be dynamically resized during runtime, see [List](../standard-library/core.md) from the standard library.

> **Implementation Status**: Not Implemented

### Array - Syntax

Arrays are defined with the `[` and `]` delimiters with items separated by a `,` character.

The type notation of an array is the `[` and `]` delimiters with the type inbetween the delimiters.

For example, an array of integers has a type of `[Int]` and an array of `(Int, Int)` tuples has a type of `[(Int, Int)]`.

### Array - Examples

```rs
let colors = ["red", "green", "blue"]
```

### Array - Equality

Arrays are equal if all elements are equal, in-order.

```rs
let colors = ["red", "green", "blue"]
colors == ["red", "green", "blue"]       // ✅ true
colors != ["purple", "yellow", "orange"] // ✅ true
```

## Tuple

A tuple is a finite and immutable sequence of ordered items.

> **Implementation Status**: Not Implemented

## Tuple - Syntax

Tuples are defined with `(` and `)` as the opening and closing delimiters and the items separated by a `,` character.

Tuples elements are accessed with the `.` operator followed by the index of the item. For example, `my_tuple.0` accesses the element at index 0 and `my_tuple.4` accesses the element at index 4.

Type definitions for tuples also use the `(` and `)` characters as delimiters, such as `(int, int)`. Explicit type definitions are rarely used for variable assignment due to type inference.

```rs
let point = (4, 19)
let enemy_move = (3, "up")
```

### Tuple - Examples

```rs
let my_tuple = (21, true, "hello", ("tree", 50))
```

This example tuple has four elements, which are:

0. `Int` 21
1. `Bool` true
2. `String` hello
3. `(String, Int)` ("tree", 50)

The value `50`, which is at the 1-index of the inner tuple at the 3-index of the outer tuple would be accessed by `my_tuple.3.1`.

```rs
let my_tuple = (21, true, "hello", ("tree", 50))

my_tuple.3.1 == 50 // ✅ true
```

### Tuple - Destructuring Assignment

Multiple variable bindings can be created from a single assignment operator using tuple destructuring assignment.

```rs
let point = (4, 19)
let (x, y) = point

x == 4 // ✅ true
y == 19 // ✅ true
```

### Tuple - Equality

Tuples are equal if all elements are equal, in-order.

```rs
let point = (4, 19)
point == (4, 19)  // ✅ true
point != (5, 20)  // ✅ true
```

## Record

Records are key=value pairs where the key is an [identifier](TODO) and the value may be any of the core types.

> TODO: Likely relax the restriction on the value being a core type and allow the value to be a user-defined type too.

Records are defined with `(` and `)` as the opening and closing delimiters. `key=value` pairs are delimited by a comma `,`. This is a similar syntax to tuples, and records can be thought of as "named tuples".

### Record - Examples

A 2D point can be defined as:

```rs
let point = (
    x = 4,
    y = 19,
)
```

```rs
let book = (
    title = "The Canterbury Tales",
    author = (
        name = "Geoffrey Chaucer",
        birth_year = 1343,
        death_year = 1400,
    ),
    original_title = "Tales of Caunterbury",
)
```

### Record - Destructuring Assignment

Multiple variable bindings can be created from a single assignment operator using record destructuring assignment.

```rs
let point = (
    x = 4,
    y = 19,
)

let ( x, y ) = point

x == 4 // ✅ true
y == 19 // ✅ true
```
