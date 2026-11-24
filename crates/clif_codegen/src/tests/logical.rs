use crate::builtins::{XBool, XFloat, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn is_even() {
    let code = "let main = fun (a: Int) -> { a % 2 == 0 }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };

    assert_eq!(code_fn((16,)), TRUE);
    assert_eq!(code_fn((15,)), FALSE);

    assert_eq!(code_fn((-16,)), TRUE);
    assert_eq!(code_fn((-15,)), FALSE);

    assert_eq!(code_fn((0,)), TRUE);
}

#[test]
fn int_equality() {
    let code = "let main = fun (a: Int, b: Int) -> { a == b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((16, -16)), FALSE);
}

#[test]
fn int_not_equality() {
    let code = "let main = fun (a: Int, b: Int) -> { a != b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((16, -16)), TRUE);
}

#[test]
fn int_less_than() {
    let code = "let main = fun (a: Int, b: Int) -> { a < b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_less_than_or_equal() {
    let code = "let main = fun (a: Int, b: Int) -> { a <= b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), FALSE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), TRUE);
}

#[test]
fn int_greater_than() {
    let code = "let main = fun (a: Int, b: Int) -> { a > b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), FALSE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn int_greater_than_imm() {
    let code = "let main = fun (a: Int) -> { a > 16 }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };

    assert_eq!(code_fn((17,)), TRUE);
    assert_eq!(code_fn((16,)), FALSE);
    assert_eq!(code_fn((15,)), FALSE);
}

#[test]
fn int_greater_than_or_equal() {
    let code = "let main = fun (a: Int, b: Int) -> { a >= b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt, XInt), XBool>(code_ptr) };

    assert_eq!(code_fn((17, 16)), TRUE);
    assert_eq!(code_fn((16, 16)), TRUE);
    assert_eq!(code_fn((15, 16)), FALSE);
}

#[test]
fn float_less_than() {
    let code = "let main = fun (a: Float, b: Float) -> { a < b }";

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(XFloat, XFloat), XBool>(code_ptr) };

    assert_eq!(code_fn((1.0, 2.0)), TRUE);
    assert_eq!(code_fn((2.0, 2.0)), FALSE);
    assert_eq!(code_fn((3.0, 2.0)), FALSE);
}
