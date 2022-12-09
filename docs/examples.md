# Code Examples

## Hello World

```
// `print` is a built-in function
// Text within quote marks are strings
print "Hello World"
```

The `print` function only accepts a String currently.

## Types

**Scalar/Primitive** data types:

- `Int`: integer (64-bit)
- `Float`: floating point number (64-bit)
- `Bool`: boolean value (true or false)
- `String`: UTF-8 encoded text

**Sum Types**:

Sum types allow the programmer to define some data must be one of a set of variants.

For example, if the variants are defined as `A`, `B`, `C` - then the data must be `A` OR `B` OR `C`.

There is one kind of sum type in this language which is known as a "tagged union" or "discriminated union". This is defined with the keyword `union`.

- Keyword `union` starts the definition
- Curly braces `{` and `}` surround the variants definition
- Each variant is a `variant_name: Type` pair
  - The name is how you'll refer to the variant, and the Type is the data associated with that variant
  - Pairs are separated by commas

Syntax example:

```txt
union {
    a: A,
    b: B,
    c: C,
}
```

Practical example:

```txt
type Event = union {
    login: LoginEvent,
    logout: LogoutEvent,
    cart_add: CartAddEvent,
    checkout: CheckoutEvent,
}
```

Any data of type `Event` is precisely one of these four variants. In this case, there are also different kinds of data associated with each variant.

`(...) -> { ... }`: functions (see the section below)

## Variables

```txt
// variables are declared with the `let` keyword
let x = 4
let message = "Hello World"

// types are inferred, and this is the preferred way
// in some cases you may want explicit type annotations

let message: string = "Hello World"
let count: string = 10 // <-- type error
```

## Functions

Function syntax is:

- Parameter list enclosed in parentheses `()`
  - Each value parameter is the form of `var_name: Type`
  - Example: `(a: Int, b: String)`
- Followed by an arrow `->`
- Followed by the return type
  - If the function returns `Unit` (doesn't return anything), that can be omitted
- Followed by the block expression that is the function body

```txt
// The "nothing" function, empty inputs and no output
let do_nothing = () -> { }

// no inputs, always returns `16` as Int
let returns_sixteen = () -> Int { 16 }

// one Int input, returns Int
let square = (input: Int) -> Int { input * input }

// one String input, no return
let shout = (input: String) -> {
    let shouted_input = input + "!!!!"
    print shouted_input
}
```

### Type Parameters

In addition to the value parameters described above, there can also be type parameters.

Type parameters are listed first in the parameter list, and the identifier starts with an apostrophe `'`

```txt
// The canonical "identity" function that just returns its input
let identity = ('a, input: 'a) -> 'a { input }


let output_two = identity 2
// output_two is inferred as Int (TODO: or `2` literal type?)
// TODO: describe type inference first?
```

1. The first `'a` defines a type parameter, called `'a`, which can be used later.
2. The second `'a`, used by the value parameter, constrains the type of that parameter.
3. The third `'a`, used as the return type, defines that the function returns the same type as its input.
