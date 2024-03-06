use crate::builtins::{XBool, XFloat, FALSE, TRUE};
use crate::compile_function;
use crate::tests::to_fn;

#[test]
fn basic_if_else() {
    let input = "
    fun (condition: Bool) -> Float { 
        if condition {
            16.0
        } else {
            8.0
        }
    }";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XBool,), XFloat>(code_ptr) };
    assert_eq!(code_fn((TRUE,)), 16.0);
    assert_eq!(code_fn((FALSE,)), 8.0);
}
