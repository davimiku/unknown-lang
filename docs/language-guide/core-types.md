# Core Types

The following types are provided by the core language runtime.

All of the core types have [Value semantics](https://en.wikipedia.org/wiki/Value_semantics). These are compared by value and are copied into functions by value.

## Integer

The Integer type is written as `Int`. This is a 64-bit integer and values of this type are integers from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 .

## Boolean

The Boolean type is written as `Bool`. Values of this type are either `true` or `false`.

### Boolean - Examples

```rs
let is_tired = true
let likes_candy = false
```

### Equality

Two values that are both `true` or both `false` are equal to each other.

### Numeric - Examples

```rs
// Inferred types
let lbs_in_ton = 2_000  // 🧐 inferred type: Int
let km_in_mile = 1.609  // 🧐 inferred type: Float

// Explicit types
let oldest_turtle_age: Int = 188
let mole: Float = 6.022e23

// Type suffix on value
let distance_from_earth = 150_000_000
```

### Numeric - Equality

Integers may be compared for equality.

```rs
// These statements are true

100 == 100    // ✅
-33 != -99    // ✅
```

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

The value of the expression must implement the `Into<String>` trait (all core types such as `Int`, `Float`, `Bool`, and `String` itself implement this trait).

```rs
let message = "Greetings, {user}. Last logged in: {last_login_date}."
let high_score = "High score: {scores.max}"
```

The expression inside of the `{` and `}` delimiters can be any valid expression, including a [block](). However, carefully consider code readability and prefer to only use simple expressions in string interpolation.

```rs
// Hard to read
let total_score_message = "Total score: {{
    let mut total = 0
    for score in scores {
        total += score
    }
    score
}}"

// Consider separating complex expressions to a separate variable
let total_score = {
    let mut total = 0
    for score in scores {
        total += score
    }
    score
}
let total_score_message = "Total score: {total_score}"
```

### String - Escape Characters

`\"` - strings are delimited by `"` characters, so any literal `"` character that should appear in a string must be escaped.
`\{` - The `{` character in a string is used to create a block for string interpolation so any literal `{` character must be escaped.
`\\` - The `\` character is used to escape the next character, and thus must be escaped itself.

### String - Equality

String can be compared for equality.

```rs
"hello" == "hello"   // ✅ true
"hello" != "bye"     // ✅ true
```

## Array

Arrays are contiguous values of the same type with a fixed length and capacity. For a similar collection that can be dynamically resized during runtime, see [List](../standard-library/core.md) from the standard library.

> **Implementation Status**: Not implemented

### Array - Syntax

Arrays are defined with the `[` and `]` delimiters with items separated by a `,` character.

The type notation of an array is the `[]` characters followed by the type of the items.

For example, an array of `Int` is typed as `[]Int` and an array of `(Int, Int)` tuples is typed as `[](Int, Int)`.

### Array - Examples

```rs
let colors: []String = ["red", "green", "blue"] // explicit type

let colors = ["red", "green", "blue"]  // 🧐 inferred type: []String
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

> **Implementation Status**: Not implemented

## Tuple - Syntax

Tuples are defined with `(` and `)` as the opening and closing delimiters and the items separated by a `,` character.

Tuples elements are accessed with the `.` operator followed by the index of the item. For example, `my_tuple.0` accesses the element at index 0 and `my_tuple.4` accesses the element at index 4.

Type definitions for tuples also use the `(` and `)` characters as delimiters, such as `(int, int)`. Explicit type definitions are rarely used for variable assignment due to type inference.

```rs
let point: (Int, Int) = (4, 19)  // explicit type annotation
let enemy_move = (3, "up")       // 🧐 inferred type: (Int, String)
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
let point = (4, 19)
point.0 = 20  // ❌ cannot mutate a tuple
```

### Tuple - Destructuring Assignment

Multiple variable bindings can be created from a single assignment operator using tuple destructuring assignment.

```rs
let status = (100, true)  // 🧐 inferred type: (Int, Bool)
let (total, is_finished) = status

let point = (4, 19)  // 🧐 inferred type: (Int, Int)
let (x, y) = point
```

### Tuple - Equality

Tuples are equal if all elements are equal, in-order.

```rs
let point = (4, 19)
point == (4, 19)  // true
point == (5, 20)  // false
```

## Named Tuple

Named tuples are key=value pairs where the key is a `string` and the value may be any of the core types.

TODO: how to do structs then? something needs to have the ability to structurally hold references. Better to have records as value/copy types and use a separate `struct` keyword? or stay consistent and only use records?

Records are defined with `{{` and `}}` as the opening and closing delimiters. `key=value` pairs are delimited by a comma `,`.

Keys must be delimited by `"` (quotation marks) if the key contains a ` ` (space) or a `=` character.

### Record - Examples

```rs
let book = (
    title = "The Canterbury Tales",
    author = (
        name = "Geoffrey Chaucer",
        birth_year = 1343,
        death_year = 1400,
    ),
    "original title" = "Tales of Caunterbury",
)
```

### Record - Destructuring Assignment

Multiple variable bindings can be created from a single assignment operator using record destructuring assignment.

```rs
let point = (
    x = 2,
    y = 5,
)

let ( x, y ) = point
// x has a value of 2
// y has a value of 5
```

### Record - Shorthand Assignment

The keys for a key=value pair can be inferred using shorthand notation where the key will be given the string value of the identifier provided.

```rs
let x = 2
let y = 5
let shorthand = {{ x, y }}
# record has a key "x" with value 2 and a key "y" with value 5

let longhand = {{ x = x, y = y}}
```
