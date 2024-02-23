/// Converts from an opaque pointer to a function pointer
///
/// # Safety
///
/// The Input (I) and Output (O) types must be valid language types
/// and must be the *exact* Input and Output types of the function
/// that was compiled.
///
/// This is *extremely* unsafe if used incorrectly.
unsafe fn to_fn<I, O>(code_ptr: *const u8) -> fn(I) -> O {
    std::mem::transmute::<_, fn(I) -> O>(code_ptr)
}

use crate::builtins::XInt;
use crate::compile_function;

#[test]
fn constant_42() {
    let input = r#"fun () -> { 42 }"#;

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 42)
}

#[test]
fn one_param_constant_42() {
    let input = "fun (i: Int) -> { 42 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 42)
}

#[test]
fn identity_int() {
    let input = "fun (i: Int) -> Int { i }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<XInt, XInt>(code_ptr) };

    assert_eq!(code_fn(123), 123)
}

#[test]
fn identity_int_addition() {
    let input = "fun (i: Int) -> Int { i + 16 }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<XInt, XInt>(code_ptr) };

    assert_eq!(code_fn(-20), -4)
}
