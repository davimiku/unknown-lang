/// Converts from an opaque pointer to a function pointer
///
/// # Safety
///
/// The Input (I) and Output (O) types must be valid language types
/// and must be the actual Input and Output types of the function
/// that was compiled.
unsafe fn to_fn<I, O>(code_ptr: *const u8) -> fn(I) -> O {
    std::mem::transmute::<_, fn(I) -> O>(code_ptr)
}

use crate::builtins::XInt;
use crate::compile_function;

#[test]
fn test_int() {
    let input = r#"() -> 42"#;

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 42)
}
