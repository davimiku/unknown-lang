# Code Examples

Examples that will be worked into other articles

## Hello World

```rs
// `print` is a built-in function
// Text within quote marks are strings
print 1
print "Hello World"
```

The `print` function is currently a hard-coded builtin that only accepts a String, Int, Float, or Bool.

## Types

**Scalar/Primitive** data types:

- `Int`: integer (64-bit)
- `Float`: floating point number (64-bit)
- `Bool`: boolean value (true or false)
- `String`: UTF-8 encoded text

**Sum Types**:

Sum types allow the programmer to define some data must be one of a set of variants.

For example, if the variants are defined as `A`, `B`, `C` - then the sum type of those variants represents `A` OR `B` OR `C`.

There is one kind of sum type in this language which is known as a "union". Specifically, this is a "tagged union" or "discriminated union".

Examples:

```rs
type Event =
  | login: LoginEvent
  | logout: LogoutEvent
  | cart_add: CartAddEvent
  | checkout: CheckoutEvent

type Status = pending | active | complete
```

- Variants are separated by the vertical bar `|`
  - A `|` may optionally be put at the beginning for stylistic preferences
- The variant name is followed by a `:` and then the associated type for that variant
  - In this example, `LoginEvent`, `LogoutEvent`, etc. are other types
  - If the variant has no associated type, the `:` may be omitted
- Union variants follow the same `snake_case` naming convention as variables

**Product Types**:

Product types allow the programmer to define data that is the combination of other data.

For example, if data is defined as `A`, `B`, `C` - then the product type of that data represents `A` AND `B` AND `C`.

There is one kind of product type in this language which is known as a "record".

Minimal example:

```rs
type Example = ( a: String, b: Int )
```

Practical example:

```rs
type CartAddEvent = (
    user: User,
    product: Product,
    quantity: Int,
    added_at: DateTime,
)
```

Unions and records can be freely composed with each other. A union may have a record as the data type for one or more of its variants, and a record may have a union as one of its fields.

## Variables

```rs
// variables are declared with the `let` keyword
let x = 4
let message = "Hello World"

// types are inferred, and this is the preferred way
// in some cases you may want explicit type annotations

let message: String = "Hello World"
let count: String = 10 // <-- type error
```

## Functions

Function syntax is:

- Parameter list enclosed in parentheses `()`
  - Each value parameter is the form of `var_name: Type`
  - Example: `(a: Int, b: String)`
- Followed by an arrow `->`
- Followed by the return type
  - If the function returns `()` (doesn't return anything), that can be omitted
- Followed by the block expression that is the function body

```rs
// The "nothing" function, empty inputs and no output
let do_nothing = () -> { }

// no inputs, always returns `16`
let returns_sixteen = () -> { 16 }

// one Int input, returns Int
let square = (input: Int) -> { input * input }

// one String input, no return
let shout = (input: String) -> {
    let shouted_input = input + "!!!!"
    print shouted_input
}
```

### Type Parameters

In addition to the value parameters described above, there can also be type parameters.

> WIP syntax: Type parameters are listed first in the parameter list, and the identifier starts with an apostrophe `'`

```rs
// The canonical "identity" function that just returns its input
let identity = ('A, input: 'A) -> { input }


let output_two = identity 2
// output_two is inferred as Int (TODO: or `2` literal type?)
// TODO: describe type inference first?
```

1. The first `'A` defines a type parameter, called `'A`, which can be used later.
2. The second `'A`, used by the value parameter, constrains the type of that parameter.
3. The third `'A`, used as the return type, defines that the function returns the same type as its input.

> WIP Interesting example, the type signature for a function that just puts its input into a list/array.

```ts
// typescript
type MakeArray = <T>(input: T) => T[];

function _makeArray<T>(input: T): T[] {
  return [input];
}

const makeArray: MakeArray = _makeArray;

const strResult = makeArray("hello");
const numResult = makeArray(123);
```

```rs
// our type signatures don't have parameter names
type MakeIntList = Int -> List Int

// how do we say "define 'A, then use 'A" ?
type MakeGenericList = ('A, 'A) -> List 'A
// this is confusing, why 'A twice?

// options

// require value names in type definitions (boo hiss...)
type MakeGenericList = ('A, a: 'A) -> List a

// Separate type parameters from value parameters with semicolon
type MakeGenericList = ('A; 'A) -> List 'A

// type parameters in a separate list
type MakeGenericList = ('A)('A) -> List 'A

// type parameters in a separate list, but with square brackets!
type MakeGenericList = ['A]('A) -> List 'A

// type parameters in a separate list, but with less than and greater than operators!
type MakeGenericList = <'A>('A) -> List 'A

// type parameters outside the function
type MakeGenericList = ('A)('A -> List 'A)

// let there be type parameters without defining them first? Is that ambiguous?
type MakeGenericList = 'A -> List 'A
```
