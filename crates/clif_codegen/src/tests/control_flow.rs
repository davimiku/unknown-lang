use crate::builtins::{XBool, XFloat, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn basic_if_else() {
    let input = "
    let main = fun (condition: Bool) -> Float { 
        if condition {
            16.0
        } else {
            8.0
        }
    }";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XBool,), XFloat>(main) };
    assert_eq!(code_fn((TRUE,)), 16.0);
    assert_eq!(code_fn((FALSE,)), 8.0);
}
