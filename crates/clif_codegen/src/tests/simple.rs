use crate::builtins::XInt;
use crate::tests::{compile_main, to_fn};

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
    let input = "
let main = fun (i: Int) -> {
    let i2 = i
    i2
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn variable_and_addition() {
    let input = "
let main = fun (i: Int) -> {
    let i2 = i + 10
    i2
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((6,)), 16);
    assert_eq!(code_fn((-4,)), 6);
}
