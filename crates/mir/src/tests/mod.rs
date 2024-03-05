mod arithmetic;
mod comparison;
mod control_flow;
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

#[test]
fn identity_int() {
    let input = "fun (i: Int) -> { i }";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0(_1):
        _0 = copy _1
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_variable_from_const() {
    let input = "
fun () -> { 
    let a = 16
    a
}";
    let expected = "
fun {anonymous}:
    params: {none}
    mut _0: 16
    _1: 16
    
    BB0(_1):
        _1 = const 16
        _0 = copy _1
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_variable_from_param() {
    let input = "
fun (a: Int) -> { 
    let b = a
    b
}";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _2 = copy _1
        _0 = copy _2
        Return ->
";

    check_function(input, expected);
}

#[test]
fn is_even() {
    let input = "fun (a: Int) -> { a % 2 == 0 }";

    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1):
        _2 = Rem(copy _1, const 2)
        _0 = Eq(copy _2, const 0)
        Return ->
";

    check_function(input, expected);
}
