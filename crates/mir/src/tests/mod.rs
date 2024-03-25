mod arithmetic;
mod calls;
mod comparison;
mod control_flow;
mod mutability;
mod params;
mod scopes;

use crate::display::write_module;
use crate::Module;

use crate::{construct_function, construct_module};

/// Lowers the provided input to MIR
///
/// Uses "normal mode" which represents a module
fn check_module(input: &str, expected: &str) {
    let (module, context) = hir::lower(input);
    let (module, context) = construct_module(&module, &context);

    check(module, context, expected)
}

/// Lowers the provided input to MIR from "function mode"
/// Input must be a single function expression
fn check_function(input: &str, expected: &str) {
    let (module, context) = construct_function(input);

    check(module, context, expected)
}

fn check(module: Module, context: &hir::Context, expected: &str) {
    let mut initial_indent = 0;
    let mut w = Vec::new();
    write_module(&module, &mut w, context, &mut initial_indent).expect("written successfully");

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
