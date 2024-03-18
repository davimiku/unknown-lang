use crate::builtins::XFloat;
use crate::tests::{compile_main, to_fn};

#[test]
#[allow(clippy::unit_cmp)]
fn nested_scopes() {
    let input = "
let main = fun (a: Float) -> {
    let b = 4.0
    {
        let c = a + a - b
        {
            let d = c * 2.5
        }
    }
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XFloat,), ()>(code_ptr) };

    assert_eq!(code_fn((16.0,)), ());
}

#[test]
fn nested_scopes_with_return() {
    let input = "
let main = fun (a: Float) -> {
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

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XFloat,), XFloat>(code_ptr) };

    assert_eq!(code_fn((16.0,)), 70.0);
}
