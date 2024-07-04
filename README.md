# Unknown Language

This project is the beginning phase of the implementation of a new statically typed, imperative programming language. Currently, this language does not have a name. The primary output of the compiler is Cranelift Intermediate Format (CLIF), which is used by the [Cranelift](https://cranelift.dev/) compiler to generate machine code.

## Project Status

Although a fair amount of work has been put into this project, it is **not in a workable state**.

Currently, the main backend has just changed from a custom bytecode VM to generating native code using [Cranelift](https://cranelift.dev/), meaning that basic functionality does not currently work. A compile-to-javascript backend may also be added at some point in the future.

## Documentation

Please see the documentation files in the [docs folder](docs/README.md).

## Quick Start

Currently, there is no available executable file to download and run for this unnamed language. The compiler and runtime are implemented in Rust, and can be run with the tools in the standard Rust ecosystem. (currently there is no decided Minimum Supported Rust Version (MSRV), but this project will always be guaranteed to compile on the latest stable Rust version.)

To write code in this unnamed language and execute it, please follow these steps:

0. Install the Rust compiler following the steps on https://www.rust-lang.org/.

1. Clone this repository

```sh
git clone git@github.com:davimiku/unknown-lang.git
```

Currently, most work is happening on the `dev` branch.

```sh
cd unknown-lang
git switch dev
```

2. In the root of this repository, with `cargo` installed from the Rust installation in Step 0, run the following command:

```sh
cargo run
```

This runs the main binary. If installation was successful, this prints the help information for the main unnamed language compiler/runtime.

3. Use the `run` command of the binary to execute programs in this unnamed language. For example, to run the example Hello World program.

```sh
cargo run -- run examples/hello_world.prog
```

Note that the `.prog` extension of this example is temporary and will be change when this language has a name.

## Tests

Unit tests can be run with:

```sh
cargo test
```

End-to-End tests can be run with:

```sh
cargo run --bin e2e_tests
```

## Run with Miri

[miri](https://github.com/rust-lang/miri) is an interpreter for Rust's Mid-Level Representation (MIR) and is a useful tool to find some cases of memory unsafety or other issues.

Before testing with `miri`, the cargo cache needs to be cleared so that dependencies can compiled for miri.

```sh
cargo clean
```

To run the unit tests with miri

```sh
MIRIFLAGS="-Zmiri-disable-stacked-borrows" cargo miri test
```

To run a specific script file, for example named `test.prog` which exists in the same directory as this README:

```sh
MIRIFLAGS="-Zmiri-disable-stacked-borrows" cargo miri run -- run test.prog
```

Note that currently the stacked borrows check is disabled via this `miri-disable-stacked-borrows` flag. This is due to this issue in [rust-analyzer/rowan](https://github.com/rust-analyzer/rowan/issues/108) which is the library used for the syntax tree.

> Please don't comment on that issue asking it to be fixed, as Rowan is _not_ intended as a general-purpose public library, it is only developed for Rust Analyzer.

In order to be able to run Miri with the stacked borrows checks, we can either contribute the fix to the rowan library (preferred, if possible) or develop a new syntax tree library (doesn't seem like a good idea, would likely be reinventing the wheel but a worse wheel).

We should also keep an eye on [tree borrows](https://perso.crans.org/vanille/treebor/), the new aliasing model for Rust programs which may replace (or supplement) the stacked borrow model.
