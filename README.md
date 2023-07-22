# Unknown Language

This project is the beginning phase of the implementation of a new statically typed, imperative programming language. Currently, this language does not have a name. The language is implemented with a bytecode compiler and stack-based virtual machine to interpret the bytecode.

> In this early phase, I typically develop on the [dev branch](https://github.com/davimiku/unknown-lang/tree/dev). That branch will have the latest commits, but there is no guarantee that anything is working as expected.

## Project Status

Although a fair amount of work has been put into this project, it is **not in a workable state**.

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
