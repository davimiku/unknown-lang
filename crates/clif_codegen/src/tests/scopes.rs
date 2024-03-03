use crate::builtins::{XFloat, XInt};
use crate::compile_function;
use crate::tests::to_fn;

#[test]
#[allow(clippy::unit_cmp)]
fn nested_scopes() {
    let input = "
fun (a: Float) -> {
    let b = 4.0
    {
        let c = a + a - b
        {
            let d = c * 2.5
        }
    }
}";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat,), ()>(code_ptr) };

    assert_eq!(code_fn((16.0,)), ());
}

#[test]
fn nested_scopes_with_return() {
    let input = "
fun (a: Float) -> {
    let b = 4.0
    {
        let c = a + a - b
        {
            let d = c * 2.5
            {
                d
            }
        }
    }
}";

    let code_ptr = compile_function(input).unwrap();

    let code_fn = unsafe { to_fn::<(XFloat,), XFloat>(code_ptr) };

    assert_eq!(code_fn((16.0,)), 70.0);
}
