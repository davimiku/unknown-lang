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

Parameter types follow the same syntax as elsewhere, for example function that takes an `array` of `int` and outputs an `int` is `[int] -> int`.

```rs
let most_common_element = (arr: []Int) -> Int {
    // ...
}
```

## Function Application

Functions are applied to data by using the identifer of the function followed by a space ` ` and then the data.

```rs
let nine = square 3    // nine == 9
let squared_again = square nine  // squared_again == 81
```

Function application is right-associative, meaning that `double square 3` is executed as `double (square 3)`. `square 3` is applied first and then `double` on that result.

### Multiple Input Values with a Tuple

A function may only have a single input, but multiple input values can be approximated by using a tuple as in the example below:

> WIP: Going to change this. Functions can have multiple parameters though it will still be conceptually very similar to a (named) tuple.

```rs
fun parse_int = (input: string, radix: int) -> int {
    // ...
}

let user_input = "1234"
let parsed_user_input = parse_int (user_input, 10)
```

In this example, there is a single parameter which is a `(string, int)` tuple. The type signature of this function is `(string, int) -> int`.

Note that the space between the function identifier and the tuple input is the encouraged style.

### Multiple Output Values with a Tuple

A function may only have a single output, but that output can be a compound type such as a tuple.

```rs
let median_and_mode = (arr: []Int) -> (Int, Int) {
    // ...
}

let nums = [3, 3, 3, 4, 5, 5, 6, 8, 9]

// Get the result back as a tuple
let result = median_and_mode nums

// Or destructure the tuple into two separate variables
let (median, mode) = median_and_mode nums

print median  // 5
print mode // 3
```

### Early Output

The last expression of a function is its output value, however, the `return` keyword can also be used to stop the execution of the function and return the value early.

TODO: haven't described output type syntax yet?

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
    if x < y { x } else { y }
}
```

In the example above, the `if...else` is an _expression_ with an output value (in this case, `x` or `y`). The `if...else` _expression_ also is the final expression of the function `min`, so the `if...else` output value is also the `min` function output value.

Although the expression-oriented style is preferred, the `return` keyword can still be convenient as an "early return" escape hatch for longer sections of procedural code.

### Type Parameters

Functions may defined type parameters for input and/or output types. Just as functions can define parameters for values, they may also define parameters for types.

In the example below, the type parameter `'a` is first defined, then it is used in the type for the first value parameter.

```rs
let most_common_element = ('a, arr: []'a) -> 'a {
    // ...
}
```

The example can be described as "Given an input of an `array` of `'a`, the output will be a value of type `'a`".

The type signature of this function is `[]'a -> 'a` where `'a` is a generic type.

Type parameters must follow these syntax rules:

- Begins with a `'` character
- Defined before value parameters in the parameter list
- Defined before it is used

### Type Parameter Constraints

> WIP

Constraints can be applied to the type parameters. Similar to how the type of a value parameter limits the values that can be passed into the function, the type constraints limits the types that can be used as the type parameter.

```rs
let sum_array = ('a: Add + Default, arr: []'a) -> 'a {
    let mut total: 'a = default
    for item in arr {
        total += item
    }
    total
}
```

In this example, the types passed in for `'a` must implement the `Add` and `Default` traits.

### Functions as Inputs for Other Functions

Functions may take other functions as inputs, also known as higher-order functions. A common example of this is the `map` function for `List`s.

```rs
fun square = num: int -> int {
    num * num
}

let nums = List [1, 2, 3, 4]

let squared_nums = nums.map square
print squared_nums  // List [1, 4, 9, 16]
```

For a list of type `List[int]`, the `map` function is applied to a function whose type signature is `int -> int` (such as the example `square` function).

### Anonymous Functions

Functions may be defined anonymously, and is often used for smaller functions passed in as input to other functions.

```rs
let numbers = List [1, 2, 3, 4]

let doubled = numbers.map num -> { num * 2 }

print doubled  // List [2, 4, 6, 8]
```

Note that in the example above, the type of `num` is inferred to be `int` and did not need to be written out, due to `map` being generic taking a generic `'a` input which is inferred to be `int` because the `nums` value is a `List[int]`.

Similarly, the output type `int` was also inferred and did not need to be written out.

### Syntax Sugar

If the function body contains a single expression and the output type can be inferred, the curly brackets `{` and `}` may be omitted.

```rs
let numbers = List [1, 2, 3, 4]

let tripled = numbers.map num -> num * 3
```

Functions can also be explicitly typed on the left-hand side of the binding to remove the need for type annotations and ensure inference on the right-hand side.

```rs
fun cube: int -> int = num -> num ** 3
```

This `cube` function is explicitly type annotated as `int -> int`, so the criteria is met to be able to omit the curly brackets around the function body.

Omitting the curly brackets in these cases is optional, in some scenarios it may be better to keep them for readability.

### Functions as Outputs from Other Functions

Functions can also be the output value of functions.

In the contrived example below, the `adder_factory` function outputs other functions which can add an amount to an `int`.

```rs
fun adder_factory = add_amount: int -> int -> int {
    fun out_func: int -> int = num -> num + add_amount
    out_func
}

fun add_two = adder_factory 2
fun add_fourteen = adder_factory 14

print add_two 9  // 11
print add_fourteen 16 // 30
```

That example could also be slightly more terse by using an anonymous function rather than binding a temporary identifier `out_func`.

```rs
fun adder_factory = add_amount: int -> int -> int {
    num: int -> num + add_amount
}
```

Note that `->` which appear in succession are right-associative, meaning that this function signature is read as "a function adder_factory takes an `int` and outputs an `int -> int`".

#### Additional Example: Composition and Generics

In the example below, the function `compose` takes two functions as inputs. The first input function is `T -> U` and the second input function is `U -> V`. The output is a function that is `T -> V`.

```rs
fun[T, U, V] compose = (fun_a: T -> U, fun_b: U -> V) -> T -> V {
    // Outputting a new (anonymous) function
    input: T -> V { fun_b fun_a input }
}
```

## Associated Functions

Functions may be associated to types.

For example, given the type definition below:

```rs
type RGBColor = {
    red: int
    green: int
    blue: int
}
```

TODO: remember how impl blocks was confusing/weird when learning Rust. Is there an easier way?

`impl for RGBColor {`
`fun for RGBColor {`
`RGBColor fun {`
`fun RGBColor {`

How does the "dead zone" work in `impl` blocks? We don't have a dead zone at the base of the module. Or allow scoped variables within an `impl` block? Export the functions that would be associated to that type?

and what about traits?

`impl Add for RGBColor`
`trait Add for RGBColor` <-- think I like this one
`fun Add for RGBColor`

TODO: Dead zone inside trait block?

TODO: Tuple parameters become weird with a one-argument associated function. On the definition, should it be `(self, param)` or `self, param` ? On the application, should it be `type.func param` or `type.func (param)` ?

Probably:

- `fun rotate = (self, degrees: int) -> { self.alignment += degrees }`
- `dial.rotate 90`

Ideas (these may be overlapping):

- ALL functions input are actually tuples, even a single input. Syntax sugar for a 1-tuple that the parentheses can be dropped. <-- leaning towards this one
- Function definition don't need parentheses even for 2+ tuples.
- Implicit `self` ? We don't need `self` vs. `&self` or lifetimes like Rust, but what about `self` vs. `mut self` ? <-- after more thought I still prefer explicit and with `mut`

```rs
impl for RGBColor {

    // inferred output type is `string`
    fun to_hex = self -> {
        "#{self.red.to_hex}{self.green.to_hex}{self.blue.to_hex}"
    }
}
```

TODO: need an example with two associated functions
