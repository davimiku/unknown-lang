mod arithmetic;
mod comparison;
mod control_flow;
mod params;
mod scopes;

use crate::{display::MirWrite, Program};

use crate::construct;

/// Lowers the provided input to MIR as from "script mode"
fn check_script(input: &str, expected: &str) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Script);
    let (program, ..) = construct(root, &hir_context);

    check(program, hir_context, expected)
}

/// Lowers the provider input to MIR from "function mode"
/// Input must be a single function expression
fn check_function(input: &str, expected: &str) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Function);
    let (program, ..) = construct(root, &hir_context);

    check(program, hir_context, expected)
}

fn check(program: Program, hir_context: hir::Context, expected: &str) {
    let mut initial_indent = 0;
    let mut w = Vec::new();
    program
        .write(&mut w, &hir_context, &mut initial_indent)
        .expect("written successfully");

    let actual = String::from_utf8(w).expect("bytes to be valid UTF-8");

    let actual = actual.trim();
    let expected = expected.trim();
    if actual != expected {
        eprintln!("expected: {expected}");
        eprintln!("actual: {actual}");
        eprintln!("diff:");
        text_diff::print_diff(expected, actual, "");
        panic!("Expected did not match actual, see printed diff.");
    }
}

#[test]
#[ignore = "not implemented yet"]
fn assignment() {
    let input = "let a = 2";
    check_script(input, "");
}
