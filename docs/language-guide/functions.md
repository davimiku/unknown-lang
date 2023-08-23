# Functions

A function takes input values and produces an output value.

## Defining Functions

A function is defined by defining the input parameters, then an arrow `->`, and then the expression that produces the output value.

Below is a function that takes zero inputs and prints the string "Hello".

```rs
() -> print "Hello"
```

That function is syntactically valid, but not useful because it can't be called. Binding this function to a variable will allow it to be called later.

```rs
let print_hello = () -> print "Hello"
```

### Function Types

The type of a function is written with the input type(s), then an arrow `->`, then the output type. For example, a function with a type `Int -> String` has an input type of `Int` and an output type of `String`.

### Parameter and Output Types

Similar to elsewhere in the language, the type for parameters is defined after a `:` character. The output type is also defined after a `:` character after the parameter list.

```rs
// `num` has a type of `Int`
// `square` has an output type of `Int`
let square = (num: Int): Int -> num * num
```

The terminology used for the example above is:

- `square` has one **input parameter** of type `Int`
- `square` has an **output type** of `Int`
- The type of the `square` function is `Int -> Int`

When a function outputs the unit type, the output type can be omitted.

```rs
/// `shout_name` has a type of `String -> ()`
let shout_name = (name: String) -> print (name ++ "!!!")
```

The `print` function outputs the unit type, so the `shout_name` function also outputs the unit type.

> **Note**: An output that is the unit type can also be thought of as "not outputting a value", although that is not technically the case. All functions output a value, and outputting the unit type indicates to the caller of the function that the result isn't meaningful or there's nothing that needs to be done with it.

### Explicit Type Annotation on Variable Binding

Functions can also be explicitly typed on the left-hand side of the variable binding to remove the need for type annotations on the right-hand side.

```rs
let square: Int -> Int = num -> num^2
```

## Function Application

Functions are applied to data by using the identifer of the function followed by a space ` ` and then the data. This is sometimes referred to as "calling" or "invoking" a function.

```rs
// apply the `square` function to the value `3`
let squared_once = square 3
squared_once == 9 // ✅ true

// apply the `square` function to the variable `nine`
let squared_again = square nine

squared_again == 81 // ✅ true
```

Function application is right-associative.

```rs
let double: Int -> Int = num -> num * 2
let square: Int -> Int = num -> num^2

let result = double square 3   // result == 18

// is the same as this
let result = double (square 3) // result == 18
```

`square` is applied to `3`, then `double` on that result.

### Multiple Input Values

Functions may have multiple input parameters by separating the parameters with a comma `,` in the parameter list.

```rs
let pythagorean = (a: Float, b: Float): Float -> {
    let c_squared = a^2 + b^2

    Math.sqrt c_squared
}

let c = pythagorean (3.0, 4.0)
c == 5.0 // ✅ true
```

The terminology used for the example above is:

- `pythagorean` has two **input parameters** of type `Float` and `Float`
- `pythagorean` has an **output type** of `Float`
- The type of the `pythagorean` function is `(Float, Float) -> Float`

### Function Bodies with Curly Braces

Some function examples have been shown with curly braces (`{` and `}`) and some without. This is not a special syntax or a special rule, the function body must always be a single expression. Curly braces are the delimiters for a [Block Expression](TODO), so this block expression is the function body.

```rs
// Function body is a multiplication expression
let square = (n: Int): Int -> n * n

// Function body is a block expression
let square = (n: Int): Int -> { n * n }
```

### Multiple Output Values with a Tuple

A function may only have a single output, but that output can be a compound type such as a tuple.

```rs
let median_and_mode = (arr: [Int]) -> (Int, Int) {
    // ...
}

let nums = [3, 3, 3, 4, 5, 5, 6, 8, 9]

// Get the result back as a tuple
let result = median_and_mode nums

// Or destructure the tuple into two separate variables
let (median, mode) = median_and_mode nums

median == 5 // ✅ true
mode == 3   // ✅ true
```

See also

- [Tuples](TODO)
- [Destructuring](TODO)
- [Arrays](TODO)

### Early Output

The last expression of a function is its output value, however, the `return` keyword can also be used to stop the execution of the function and return the value early.

```rs
let min = (x: Int, y: Int): Int -> {
    if x < y {
        return x
    }
    y
}
```

Generally, using the last expression as the output value is the preferred style. This example could instead be written as:

```rs

let min = (x: Int, y: Int): Int -> {
    if x < y {
        x
    } else {
        y
    }
}
```

In the example above, the `if...else` is the final expression of the block, which is the final expression of the function `min`, so the `if...else` output value is the output value of the `min` function.

1. `if...else` outputs `Int`,
2. Therefore, the block outputs `Int`,
3. Therefore, the function `min` outputs `Int`

Although the expression-oriented style is preferred, the `return` keyword can still be convenient as an "early return" escape hatch for longer sections of procedural code.

### Type Parameters

> **Implementation Status**: Everything from here on is Not Implemented

Functions may defined type parameters for input and/or output types. Just as functions can define parameters for values, they may also define parameters for types.

In the example below, the type parameter `'a` is first defined, then it is used in the type for the first value parameter.

```rs
let most_common_element = ('a, arr: ['a]) -> 'a {
    // ...
}
```

> _Syntax Note_: Considering separating the type parameters from the value parameters with a `;`

The example can be described as "Given an input of an `array` of some type `'a`, the output will be a value of type `'a`".

The type signature of this function is `['a] -> 'a` where `'a` is a generic type.

Type parameters must follow these syntax rules:

- Begins with a `'` character
- Defined before value parameters in the parameter list
- Defined before it is used

### Applying Type Parameters

Given the previous `most_common_element` function, applying this function is the same as for functions without type parameters.

```rs
let mode = most_common_element [1, 2, 1, 3, 1, 4]

mode == 1 // ✅ true
```

In this case, and in most cases, the type parameter is _inferred_. The `'a` type parameter is inferred as `Int`, because the value parameter is `[Int]`.

In some cases, the type parameter can't be inferred, and so the type argument needs to be provided explicitly. This is done in the normal argument list. For example, the `parse` function from the standard library often requires an explicit type argument.

```rs
let parsed_int = "12345".parse Int
```

As a note for this particular example, the type annotation could also be applied to the variable binding which would eliminate the need to provide the type as an explicit argument (it would be inferred).

```rs
let parsed_int: Int = "12345".parse
```

### Type Parameter Constraints

> syntax is WIP

Constraints can be applied to the type parameters. Similar to how the type of a value parameter limits the values that can be passed into the function, the type constraints limits the types that can be used as the type parameter.

```rs
let sum_array = ('a: Add + Default, arr: ['a]): 'a -> {
    let mut total: 'a = default
    for item in arr {
        total += item
    }
    total
}
```

In this example, the types passed in for `'a` must implement the `Add` and `Default` traits.

See also:

- [Traits](TODO)

### Functions as Inputs for Other Functions

Functions may take other functions as inputs, also known as higher-order functions. A common example of this is the `map` function for `List`s.

```rs
let square = (num: Int): Int -> num * num

let nums = List [1, 2, 3, 4]

let squared_nums = nums.map square
print squared_nums  // List [1, 4, 9, 16]
```

For a list of type `List Int`, the `map` function is applied to a function whose type signature is `Int -> Int` (such as the example `square` function).

### Anonymous Functions

Functions may be defined anonymously, and is often used for smaller functions passed in as input to other functions.

```rs
let numbers = List [1, 2, 3, 4]

let doubled = numbers.map ( num -> num * 2 )

print doubled  // List [2, 4, 6, 8]
```

Note that in the example above, the type of the function is inferred as `Int -> Int` and didn't need to be specified explicitly.

### Functions as Outputs from Other Functions

Functions can also be the output value of functions.

In the contrived example below, the `adder_factory` function outputs other functions which can add an amount to an `Int`.

```rs
let adder_factory = (add_amount: Int): (Int -> Int) -> {
    let out_func: Int -> Int = num -> num + add_amount
    out_func
}

let add_two = adder_factory 2
let add_fourteen = adder_factory 14

print add_two 9       // 11
print add_fourteen 16 // 30
```

That example could also be slightly more terse by directly outputting the function rather than binding a temporary variable `out_func`.

```rs
let adder_factory = (add_amount: Int): (Int -> Int) -> {
    num -> num + add_amount
}
```

> TODO: Are the parentheses required in this case?

> TODO: show this example?
>
> ```rs
> type AdderFactory = Int -> Int -> Int
> let adder_factory: AdderFactory = add_amount -> {
>     num -> num + add_amount
> }
> ```

#### Additional Example: Composition and Type Parameters

> TODO: decide whether this is a good example to show or not. If there is a pipeline operator (`|>`) then composing functions like this is somewhat discouraged?
>
> Also feels like this should be in a separate "advanced examples" section.

In the example below, the function `compose2` takes two functions as inputs. The first input function is `'a -> 'b` and the second input function is `'b -> 'c`. The output is a function that is `'a -> 'c`.

```rs
let compose2 = ('a, 'b, 'c, f: 'a -> 'b, g: 'b -> 'c): ('a -> 'c) -> {
    // Outputting a new function
    input -> g f input
}
```

> WIP: syntax below is not finalized - is it ambiguous which ones are type parameters and which are value parameters? Could use `;` to separate them if needed?

To simplify the type definition, it can be defined separately.

```rs
type Compose2 = ('a, 'b, 'c, 'a -> 'b, 'b -> 'c) -> ('a -> 'c)
let compose2: Compose2 = (f, g) -> (input -> g f input)
```

## Associated Functions / Universal Function Call Syntax / Method Call Syntax

> WIP: Syntax not defined

**Objective**: Define a function `f` the same way as all other functions, and allow that function to be called with `x.f` (in addition to the normal `f x`).

```rs
// type Invoice (struct) with some fields
// Goal: be able to call the function with `my_invoice.days_until_due`

// No different syntax
// Must be defined in the same module as the type
let days_until_due = (invoice: Invoice): Int -> { ... }

// Use a "self" reserved identifier (not a keyword)
// Defined in the same module as the type
let days_until_due = (self: Invoice): Int -> { ... }

// Use a "self" modifier on the parameter
// Becomes too wordy with other modifiers like `mut`?
let days_until_due = (self invoice: Invoice): Int -> { ... }

// Other options?
```

Notes:

- `my_invoice.days_until_due` is a function application (it's an `Int`)
  - Just like how parentheses are not required in `f x` applications with one argument, parentheses not required here either because there's also one argument (the argument is to the left of the dot)
- Need to check and experiment with syntax for this vs. storing a function as a field in a union/struct and then calling that

> TODO: Research Nim and D languages for UFCS and method call syntax. Research Swift for named parameters, maybe `self` can be a reserved name parameter
