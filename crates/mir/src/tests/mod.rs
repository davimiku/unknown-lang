mod arithmetic;
mod calls;
mod comparison;
mod control_flow;
mod params;
mod scopes;

use crate::{display::MirWrite, Module};

use crate::{construct_function, construct_module, construct_script};

/// Lowers the provided input to MIR
///
/// Uses "normal mode" which represents a module
fn check_module(input: &str, expected: &str) {
    let (module, context) = hir::lower(input);
    let (module, context) = construct_module(&module, &context);

    check(module, context, expected)
}

/// Lowers the provided input to MIR as from "script mode"
fn check_script(input: &str, expected: &str) {
    let (module, context) = construct_script(input);

    check(module, context, expected)
}

/// Lowers the provided input to MIR from "function mode"
/// Input must be a single function expression
fn check_function(input: &str, expected: &str) {
    let (module, context) = construct_function(input);

    check(module, context, expected)
}

fn check(program: Module, context: &hir::Context, expected: &str) {
    let mut initial_indent = 0;
    let mut w = Vec::new();
    program
        .write(&mut w, context, &mut initial_indent)
        .expect("written successfully");

    let actual = String::from_utf8(w).expect("bytes to be valid UTF-8");

    let actual = actual.trim();
    let expected = expected.trim();
    if actual != expected {
        eprintln!("expected:\n{expected}");
        eprintln!("actual:\n{actual}");
        eprintln!("diff:");
        text_diff::print_diff(expected, actual, "");
        panic!("Expected did not match actual, see printed diff.");
    }
}
