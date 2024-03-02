use super::to_fn;
use crate::builtins::{XFloat, XInt};
use crate::compile_function;

#[test]
fn int_add_param_and_constant() {
    let input = "fun (i: Int) -> { i + 6 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((10,)), 16);
    assert_eq!(code_fn((-22,)), -16);
}

#[test]
fn int_sub_param_and_constant() {
    let input = "fun (i: Int) -> { i - 10 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((26,)), 16);
    assert_eq!(code_fn((-16,)), -26);
}

#[test]
fn int_mul_param_and_constant() {
    let input = "fun (i: Int) -> { i * 8 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((2,)), 16);
    assert_eq!(code_fn((-2,)), -16);
}

#[test]
fn int_add_params() {
    let input = "fun (a: Int, b: Int) -> { a + b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((10, 6)), 16);
    assert_eq!(code_fn((-10, -16)), -26);
}

#[test]
fn int_sub_params() {
    let input = "fun (a: Int, b: Int) -> { a - b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((26, 10)), 16);
    assert_eq!(code_fn((-10, -16)), 6);
}

#[test]
fn int_mul_params() {
    let input = "fun (a: Int, b: Int) -> { a * b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((4, -4)), -16);
    assert_eq!(code_fn((-16, -4)), 64);
    assert_eq!(code_fn((2, 16)), 32);
}

#[test]
fn remainder_params() {
    let input = "fun (a: Int, b: Int) -> { a % b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((16, 4)), 0);
    assert_eq!(code_fn((16, 5)), 1);
    assert_eq!(code_fn((16, 6)), 4);

    assert_eq!(code_fn((-16, 4)), 0);
    assert_eq!(code_fn((-16, 5)), -1);
    assert_eq!(code_fn((-16, 6)), -4);

    assert_eq!(code_fn((16, -4)), 0);
    assert_eq!(code_fn((16, -5)), 1);
    assert_eq!(code_fn((16, -6)), 4);
}

#[test]
fn much_int_arithmetic() {
    let input = "fun (a: Int, b: Int) -> { a + 2 * b - 7 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((3, 10)), 16);
}

#[test]
fn float_add_param_and_constant() {
    let inputs = [
        "fun (i: Float) -> { i + 6.0 }",
        "fun (i: Float) -> { i + 6 }",
    ];

    for input in inputs {
        let code_ptr = compile_function(input).unwrap();

        let code_fn = unsafe { to_fn::<(XFloat,), XFloat>(code_ptr) };

        assert_eq!(code_fn((10.0,)), 16.0);
        assert_eq!(code_fn((-22.0,)), -16.0);
    }
}

#[test]
fn float_sub_param_and_constant() {
    let inputs = [
        "fun (i: Float) -> { i - 10.0 }",
        "fun (i: Float) -> { i - 10 }",
    ];

    for input in inputs {
        let code_ptr = compile_function(input).unwrap();

        let code_fn = unsafe { to_fn::<(XFloat,), XFloat>(code_ptr) };

        assert_eq!(code_fn((26.0,)), 16.0);
        assert_eq!(code_fn((-16.0,)), -26.0);
    }
}

#[test]
fn float_mul_param_and_constant() {
    let inputs = [
        "fun (i: Float) -> { i * 8.0 }",
        "fun (i: Float) -> { i * 8 }",
    ];

    for input in inputs {
        let code_ptr = compile_function(input).unwrap();

        let code_fn = unsafe { to_fn::<(XFloat,), XFloat>(code_ptr) };

        assert_eq!(code_fn((2.0,)), 16.0);
        assert_eq!(code_fn((-2.0,)), -16.0);
    }
}

#[test]
fn float_add_params() {
    let input = "fun (a: Float, b: Float) -> { a + b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat, XFloat), XFloat>(code_ptr) };

    assert_eq!(code_fn((10.0, 6.0)), 16.0);
    assert_eq!(code_fn((-10.0, -16.0)), -26.0);
}

#[test]
fn float_int_add_params() {
    let input = "fun (a: Float, b: Int) -> { a + b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat, XInt), XFloat>(code_ptr) };

    assert_eq!(code_fn((10.0, 6)), 16.0);
    assert_eq!(code_fn((-10.0, -16)), -26.0);
}

#[test]
fn int_float_add_params() {
    let input = "fun (a: Int, b: Float) -> { a + b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XFloat), XFloat>(code_ptr) };

    assert_eq!(code_fn((10, 6.0)), 16.0);
    assert_eq!(code_fn((-10, -16.0)), -26.0);
}

#[test]
fn float_sub_params() {
    let input = "fun (a: Float, b: Float) -> { a - b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat, XFloat), XFloat>(code_ptr) };

    assert_eq!(code_fn((26.0, 10.0)), 16.0);
    assert_eq!(code_fn((-10.0, -16.0)), 6.0);
}

#[test]
fn float_mul_params() {
    let input = "fun (a: Float, b: Float) -> { a * b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat, XFloat), XFloat>(code_ptr) };

    assert_eq!(code_fn((4.0, -4.0)), -16.0);
    assert_eq!(code_fn((-16.0, -4.0)), 64.0);
    assert_eq!(code_fn((2.0, 16.0)), 32.0);
}

#[test]
fn much_float_arithmetic() {
    let input = "fun (a: Float, b: Float) -> { a + 2 * b - 7 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat, XFloat), XFloat>(code_ptr) };

    assert_eq!(code_fn((3.0, 10.0)), 16.0);
}
