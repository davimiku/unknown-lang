# Keywords

- [Assignment](#assignment)
- [Control Flow](#control-flow)

## Notation in Examples

In the examples below, a double underscore `__` is used to describe the general structure of something, not that precise thing.

For example, if you see `__expr__`, it's referring to an expression in general (any expression), not the literal characters "expr".

## Assignment

The following keywords relate to variable assignment. Please also see [assignment expressions](TODO).

### let

`let` creates an _immutable_ binding to a value.

_Structure_

```
let __ident__ = __expr__
```

_Example_

```
let greeting = "Hello, world!"
```

`greeting` cannot be bound to another value or mutated later.

### mut

`mut` modifies the binding to be a _mutable_ binding to a value.

_Structure_

```
let mut __ident__ = __value__
```

_Example_

```
let mut count = 0
```

`count` could be bound to another value later because it was declared as a mutable binding.

```rs
count = count + 1
```

## Control Flow

Below are the keywords used for control flow.

### if / else

`if` and `else` provide the same control flow constructs familiar from other languages.

_Structure_

```
if __condition_1__ {
    __expr1__
} else if __condition_2__ {
    __expr2__
} else {
    __expr3__
}
```

The types of the branch expressions must be compatible if the type of the if/else expression is used.

_Example_

In the example below, the output of the if/else expression is bound to a variable, so all of the branches must have compatible types.

```rs
// assume `user` already exists

// inferred type: String ✅
let greeting = if user.is_registered {
    "Hello, {user.full_name}!"
} else if user.visit_count > 1 {
    "Welcome back, visitor!"
} else {
    "Hello, newcomer!"
}
```

In the example below, the branches of the if/else expression do not have compatible types, and the program has a type error.

```rs
// assume `some_condition` already exists

// Type Error ❌: if/else branches are not compatible
let example = if some_condition {
    "I am a String"
} else {
    1234
}
```

The if/else expression can also be only used for side-effects, it's not mandatory to use the output value of the if/else expression.

```rs
// assume `some_condition` already exists

// Not using the output of the if/else expression is OK
if some_condition {
    print "The condition was true!"
} else {
    print "The condition was false..."
}
```

### for / in

The `for` and `in` keywords can be combined for iteration.

_Structure_

```rs
for __ident__ in __iterator__ {
    // do something
}
```

_Example_

```rs
// iterate through a collection
let usernames = ["john", "paul", "george", "ringo"]
for username in usernames {
    // do something with `username`
}

// iterate a number of times
for i in 0..10 {
    // do something with `i`
}
```

Learn more about [iterators](TODO) and [range expressions](TODO), and also reference the [standard library]() for types that are iterable.

The `for / in` style of iteration is generally used for imperative-style code that has **side-effects**.

### return

The `return` keyword stops the function and outputs the value after the keyword.

_Structure_

```
let __function_name__ = () -> {
    // ...
    return __value__
}
```

_Example_

```rs
let factorial = (n: Int): Int -> {
    if n <= 1 {
        return 1
    }
    n * factorial (n - 1)
}
```

The `return` keyword is often unnecessary because the last expression of the function is already used as the return value. Typically, the `return` keyword is only used for early returns.

For example, the previous example could have been written without the `return` keyword as:

```rs
let factorial = (n: Int): Int -> {
    if n <= 1 {
        1
    } else {
        n * factorial (n - 1)
    }
}
```

### while

> **Note**: This feature can be disabled by the execution context for executing untrusted user code to reduce the possibility of an infinite loop.

_Structure_

```rs
while __condition__ {
    // ...
}
```

```rs
let mut user_input = stdin.get()
while user_input !in allowed_values {
    print "Sorry, {user_input} is an invalid value"
    user_input = stdin.get()
}
```

The `while` blocks are also an expression that can output a value using the `break` keyword.

```rs
// need example
```

## Resource Lifetime Management

Resources are values that have a lifetime associated with them. Typically, this "lifetime" includes some action that must happen before the value is ready (initialization) and some action that must happen after the value is done being used (disposal). Common examples of resources with lifetimes are `File` handles (reading/writing files) and network requests (I/O streams, HTTP requests, etc.).

### with

The `with` keyword creates a block that automatically disposes resources at the end of the block. This defines the lifetime of that resources to be no longer than the lifetime of the block.

```rs
with let file = stdlib.fs.open "data.txt" {
    for line in file.read_lines {
        print line
    }

    // The file handle for "data.txt" is automatically closed at the end of the `with` block
}
```

See also the [Resource](TODO) trait from the standard library for other examples of resources that can be used in a `with` block.

## Modules and Visibility

> All of the syntax below is undecided.

### module

The `module` keyword defines a module. Multiple modules may exist within a single file of code, and modules may be defined inline using the `module` keyword and an identifier, such as:

```rs
module lexer {
    // ...
}

module parser {
    // ...
}
```

TODO: Create an image with colored sections showing conceptually multiple modules in a file

Files may also be defined as modules. The "parent" module defines the "child" module using the `module` keyword, the identifier for the module, and the relative filepath.

```rs
// in the parent module
module parser "./parser.gr"
```

The "parser.gr" file is now the "parser" module, and the same rules apply.

TODO: Create an image with two "files" (rectangles) side-by-side and show the module declaration in one file with an arrow to the other file.

### import

The `import` keyword brings an identifier from another module into the current scope.

The `/` syntax is a placeholder and is not finalized.

```rs
// example: `import` at the top of a module
import stdlib/json
```

If the `import` keyword appears at the top of a module, the identifiers may be used anywhere in that module. Similarly, the `import` keyword may appear in a [function](./functions.md) or in a [block](TODO), and the identifiers are brought into that scope only. See the [Scope](./scope.md) section for more detail on scopes.

```rs
// example: `import` within function block
let read_json_file = (path: String) -> {
    import stdlib/fs
    import stdlib/json

    // ...
}
```

```rs
// example: `import` within a block

let response = {
    import stdlib/http
    import stdlib/json


}
```

### as

The `as` keyword can change the local identifier for something that was brought into scope with the `import` statement.

```rs
import some_module/some_long_name as name
```

### export

> `export` as a keyword is not decided, it may be an attribute instead.

The `export` keyword marks an identifier as being "public", such that it can be `import`ed from other modules. By default, the [visibility](TODO) of the identifier is "one level up" from the current module, so either the parent of the current module, or if the current module is at the root of the package, is it visible outside of the package.

The visibility can be modified with the following [module path](TODO) keywords.

- group: The identifier can be `import`ed anywhere within the "module group", which is generally the "siblings" of the current module.
- package: The identifier can be `import`ed anywhere within the package.

## Future Use

The following keywords are reserved for future use. "Reserved" means that these keywords may not be used as variable identifiers, and these keywords may be given a meaning in the future.

- `of`
- `is`
- `typeof`
- `keyof`
- `effect`
- `default`
- `yield`
