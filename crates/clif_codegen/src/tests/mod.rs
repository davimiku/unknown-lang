mod arithmetic;
mod branches;
mod calls;
mod logical;
mod loops;
mod mutability;
mod scopes;
mod simple;

/// Converts from an opaque pointer to a function pointer
///
/// # Safety
///
/// The Input (I) and Output (O) types must be valid language types
/// and must be the *exact* Input and Output types of the function
/// that was compiled.
///
/// The Input (I) type must be a tuple. For a function with one parameter,
/// it must be a one element tuple. For a zero parameter function, this is `()`.
///
/// This is *extremely* unsafe if used incorrectly.
///
/// Examples:
///
/// ```ignore
/// // compiled a function with no parameters and Unit return type
/// let code_fn = unsafe { to_fn::<(), ()>(code_ptr) };
///
/// // compiled a function with no parameters and Int return type
/// let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };
///
/// // compiled a function with an Int parameter and Unit return type
/// let code_fn = unsafe { to_fn::<(XInt,), ()>(code_ptr) };
///
/// // compiled a function with an Int parameter and Bool return type
/// let code_fn = unsafe { to_fn::<(XInt,), XBool>(code_ptr) };
///
/// // compiled a function with an Int and Bool parameters and Float return type
/// let code_fn = unsafe { to_fn::<(XInt, XBool), XFloat>(code_ptr) };
/// ```
unsafe fn to_fn<I, O>(code_ptr: *const u8) -> fn(I) -> O {
    std::mem::transmute::<_, fn(I) -> O>(code_ptr)
}

use cranelift::codegen::print_errors::{pretty_error, pretty_verifier_error};

use crate::builtins::XInt;
use crate::compile_module;

fn compile_main(input: &str) -> *const u8 {
    let functions = compile_module(input).unwrap();
    *functions.get("main").unwrap()
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
