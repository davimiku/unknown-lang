use crate::display::MirWrite;
use hir::ContextDisplay;

use crate::construct;

/// Lowers the provided input to MIR as from "script mode"
fn check_script(input: &str) -> String {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Script);
    println!("{}", root.display(&hir_context));
    let (program, ..) = construct(root, &hir_context);

    let mut initial_indent = 0;
    let mut w = Vec::new();
    program
        .write(&mut w, &hir_context, &mut initial_indent)
        .expect("written successfully");

    String::from_utf8(w).expect("bytes to be valid UTF-8")
}

/// Lowers the provider input to MIR from "function mode"
/// Input must be a single function expression
fn check_function(input: &str, expected: &str) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Function);
    println!("{}", root.display(&hir_context));
    let (program, ..) = construct(root, &hir_context);

    let mut initial_indent = 0;
    let mut w = Vec::new();
    program
        .write(&mut w, &hir_context, &mut initial_indent)
        .expect("written successfully");

    let actual = String::from_utf8(w).expect("bytes to be valid UTF-8");

    assert_eq!(actual.trim(), expected.trim());
}

#[test]
fn basic_arithmetic() {
    let input = "3 + 5";
    let expected = r#"
fun main:
    params: {none}
    mut _0: Int
    
    BB0:
        _0 = Add(const 3, const 5)
        return
"#;

    let actual = check_script(input);

    assert_eq!(actual.trim(), expected.trim());
}

#[test]
#[ignore = "not implemented yet"]
fn assignment() {
    let input = "let a = 2";
    check_script(input);
}

#[test]
fn identity_int() {
    let input = "fun (i: Int) -> { i }";
    let expected = r#"
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0:
        _0 = copy _1
        return
"#;

    check_function(input, expected);
}

#[test]
fn int_add_param_and_constant() {
    let input = "fun (i: Int) -> { i + 16 }";
    let expected = r#"
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0:
        _0 = Add(copy _1, const 16)
        return
"#;

    check_function(input, expected);
}

#[test]
fn int_sub_param_and_constant() {
    let input = "fun (i: Int) -> { i - 16 }";
    let expected = r#"
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0:
        _0 = Sub(copy _1, const 16)
        return
"#;

    check_function(input, expected);
}

#[test]
fn int_mul_param_and_constant() {
    let input = "fun (i: Int) -> { i * 16 }";
    let expected = r#"
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0:
        _0 = Mul(copy _1, const 16)
        return
"#;

    check_function(input, expected);
}

#[test]
fn int_equality() {
    let input = "fun (a: Int, b: Int) -> Bool { a == b }";
    let expected = r#"
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0:
        _0 = Eq(copy _1, copy _2)
        return
"#;

    check_function(input, expected);
}
