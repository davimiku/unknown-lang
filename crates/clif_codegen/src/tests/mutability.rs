use crate::{builtins::XInt, tests::to_fn};

use super::compile_main;

#[test]
fn integer_reassignment() {
    let code = "
let main = fun () -> {
    let mut i = 0
    i = i + 2
    i
}";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 2);
}

#[test]
fn integer_reassignment_from_param() {
    let code = "
let main = fun (a: Int) -> {
    let mut b = a + 2
    b = b + 6
    b
}";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((8,)), 16);
}
