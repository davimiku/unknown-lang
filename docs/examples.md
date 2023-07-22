# Code Examples

## Hello World

```
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

There is one kind of sum type in this language which is known as a "tagged union" or "discriminated union". This is defined with the keyword `union`.

- Keyword `union` starts the definition
- Curly braces `{` and `}` surround the variant definitions
- Each variant is a `variant_name: Type` pair
  - The name is how you'll refer to the variant, and the Type is the data associated with that variant
  - Pairs are separated by commas
  - Line breaks are optionally used to separate pairs for aesthetics

Minimal example:

```txt
union { a: A, b: B, c: C }
```

Binding to a type variable:

```txt
type Example = union { a: A, b: B, c: C }
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

Any data of type `Event` is precisely one of these four variants.

**Product Types**:

Product types allow the programmer to define data that is the combination of other data.

For example, if data is defined as `A`, `B`, `C` - then the product type of that data represents `A` AND `B` AND `C`.

There is one kind of product type in this language which is known as a "structure". This is defined with the keyword `struct`.

- Keyword `struct` starts the definition
- Curly braces `{` and `}` surround the field definitions
- Each field is a `field_name: Type` pair
  - The name is how you'll refer to the field, and the Type is the data associated with that field
  - Pairs are separated by commas
  - Line breaks are optionally used to separate pairs for aesthetics

Minimal example:

```txt
struct { a: A, b: B, c: C }
```

Binding to a type variable:

```txt
type Example = struct { a: A, b: B, c: C }
```

Practical example:

```txt
type CartAddEvent = struct {
    user: User,
    product: Product,
    quantity: Int,
    added_at: DateTime,
}
```

As shown in this example, unions and structs can be freely composed with each other. A union may have a struct as the data type for one or more of its variants, and a struct may have a union as one of its fields.

## Variables

```txt
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
  - If the function returns `Unit` (doesn't return anything), that can be omitted
- Followed by the block expression that is the function body

```txt
// The "nothing" function, empty inputs and no output
let do_nothing = () -> { }

// no inputs, always returns `16`
let returns_sixteen = () ->  16

// one Int input, returns Int
let square = (input: Int) -> input * input

// one String input, no return
let shout = (input: String) -> {
    let shouted_input = input + "!!!!"
    print shouted_input
}
```

### Type Parameters

In addition to the value parameters described above, there can also be type parameters.

Type parameters are listed first in the parameter list, and the identifier starts with an apostrophe `'`

```
// The canonical "identity" function that just returns its input
let identity = ('a, input: 'a) -> { input }


let output_two = identity 2
// output_two is inferred as Int (TODO: or `2` literal type?)
// TODO: describe type inference first?
```

1. The first `'a` defines a type parameter, called `'a`, which can be used later.
2. The second `'a`, used by the value parameter, constrains the type of that parameter.
3. The third `'a`, used as the return type, defines that the function returns the same type as its input.

Interesting example, the type signature for a function that just puts its input into a list/array.

```ts
type MakeArray = <T>(input: T) => T[];

function _makeArray<T>(input: T) {
  return [input];
}

const makeArray: MakeArray = _makeArray;

const strResult = makeArray("hello");
const numResult = makeArray(123);
```

```rs
// our type signatures don't have parameter names
type MakeIntList = Int -> List Int

// how do we say "define 'a, then use 'a" ?
type MakeGenericList = ('a, 'a) -> List 'a
// this is confusing, why 'a twice?

// options

// require value parameter names in type definitions (boo hiss...)
type MakeGenericList = ('a, a: 'a) -> List a

// Separate type parameters from value parameters with semicolon
type MakeGenericList = ('a; 'a) -> List 'a

// type parameters in a separate list
type MakeGenericList = ('a)('a) -> List 'a

// type parameters outside the function
type MakeGenericList = ('a)('a -> List 'a)

// type parameters in a separate list, but with square brackets!
type MakeGenericList = ['a]('a) -> List 'a

// type parameters in a separate list, but with less than and greater than operators!
type MakeGenericList = <'a>('a) -> List 'a

// let there be type parameters without defining them first? Is that ambiguous?
type MakeGenericList = 'a -> List 'a
```
