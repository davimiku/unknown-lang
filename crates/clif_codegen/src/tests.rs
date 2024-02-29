/// Converts from an opaque pointer to a function pointer
///
/// # Safety
///
/// The Input (I) and Output (O) types must be valid language types
/// and must be the *exact* Input and Output types of the function
/// that was compiled.
///
/// The Input (I) type must be a tuple. For a function with one parameter,
/// although a bare type parameter may technically work, by convention it
/// must be a one element tuple. For a zero parameter function, this is `()`.
///
///
/// This is *extremely* unsafe if used incorrectly.
unsafe fn to_fn<I, O>(code_ptr: *const u8) -> fn(I) -> O {
    std::mem::transmute::<_, fn(I) -> O>(code_ptr)
}

use crate::builtins::{XBool, XInt, FALSE, TRUE};
use crate::compile_function;

#[test]
fn constant_16() {
    let input = r#"fun () -> { 16 }"#;

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn constant_16_by_addition() {
    let input = r#"fun () -> { 10 + 6 }"#;

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn one_param_constant_16() {
    let input = "fun (i: Int) -> { 16 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn identity_int() {
    let input = "fun (i: Int) -> { i }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn identity_int_with_variable() {
    let input = "fun (i: Int) -> {
        let i2 = i
        i2
    }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn int_add_param_and_constant() {
    let input = "fun (i: Int) -> { i + 6 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((10,)), 16);
    assert_eq!(code_fn((-24,)), -16);
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
fn is_even() {
    let input = "fun (a: Int) -> { a % 2 == 0 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };

    assert_eq!(code_fn((16,)), TRUE);
    assert_eq!(code_fn((15,)), FALSE);

    assert_eq!(code_fn((-16,)), TRUE);
    assert_eq!(code_fn((-15,)), FALSE);

    assert_eq!(code_fn((0,)), TRUE);
}

#[test]
fn much_arithmetic() {
    let input = "fun (a: Int, b: Int) -> { a + 2 * b - 7 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XInt>(code_ptr) };

    assert_eq!(code_fn((3, 10)), 16);
}

#[test]
fn int_equality() {
    let input = "fun (a: Int, b: Int) -> { a == b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((16, -16)), FALSE);
}

#[test]
fn int_not_equality() {
    let input = "fun (a: Int, b: Int) -> { a != b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((16, -16)), TRUE);
}

#[test]
fn int_less_than() {
    let input = "fun (a: Int, b: Int) -> { a < b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_less_than_or_equal() {
    let input = "fun (a: Int, b: Int) -> { a <= b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_greater_than() {
    let input = "fun (a: Int, b: Int) -> { a > b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn int_greater_than_or_equal() {
    let input = "fun (a: Int, b: Int) -> { a >= b }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn variable_and_addition() {
    let input = "fun (i: Int) -> {
        let i2 = i + 10
        i2
    }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((6,)), 16);
    assert_eq!(code_fn((-4,)), 6);
}
