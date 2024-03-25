mod arithmetic;
mod calls;
mod control_flow;
mod mutability;
mod scopes;

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

use crate::{
    builtins::{XBool, XInt, FALSE, TRUE},
    compile_module,
};

fn compile_main(input: &str) -> *const u8 {
    let functions = compile_module(input).unwrap();
    *functions.get("main").unwrap()
}

#[allow(clippy::unit_cmp)]
#[test]
fn do_nothing() {
    let input = "let main = fun () -> { }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), ()>(code_ptr) };

    assert_eq!(code_fn(()), ());
}

#[test]
fn constant_16() {
    let input = "let main = fun () -> { 16 }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn constant_16_by_addition() {
    let input = "let main = fun () -> { 10 + 6 }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn one_param_constant_16() {
    let input = "let main = fun (i: Int) -> { 16 }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn identity_int() {
    let input = "let main = fun (i: Int) -> { i }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn identity_int_with_variable() {
    let input = "let main = fun (i: Int) -> {
        let i2 = i
        i2
    }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn is_even() {
    let input = "let main = fun (a: Int) -> { a % 2 == 0 }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };

    assert_eq!(code_fn((16,)), TRUE);
    assert_eq!(code_fn((15,)), FALSE);

    assert_eq!(code_fn((-16,)), TRUE);
    assert_eq!(code_fn((-15,)), FALSE);

    assert_eq!(code_fn((0,)), TRUE);
}

#[test]
fn int_equality() {
    let input = "let main = fun (a: Int, b: Int) -> { a == b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((16, -16)), FALSE);
}

#[test]
fn int_not_equality() {
    let input = "let main = fun (a: Int, b: Int) -> { a != b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((16, -16)), TRUE);
}

#[test]
fn int_less_than() {
    let input = "let main = fun (a: Int, b: Int) -> { a < b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_less_than_or_equal() {
    let input = "let main = fun (a: Int, b: Int) -> { a <= b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_greater_than() {
    let input = "let main = fun (a: Int, b: Int) -> { a > b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn int_greater_than_imm() {
    let input = "let main = fun (a: Int) -> { a > 16 }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };

    assert_eq!(code_fn((17,)), TRUE);
    assert_eq!(code_fn((16,)), FALSE);
    assert_eq!(code_fn((15,)), FALSE);
}

#[test]
fn int_greater_than_or_equal() {
    let input = "let main = fun (a: Int, b: Int) -> { a >= b }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn variable_and_addition() {
    let input = "let main = fun (i: Int) -> {
        let i2 = i + 10
        i2
    }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((6,)), 16);
    assert_eq!(code_fn((-4,)), 6);
}
