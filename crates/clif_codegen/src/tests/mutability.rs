use crate::{builtins::XInt, tests::to_fn};

use super::compile_main;

#[test]
fn integer_reassignment() {
    let input = "
let main = fun () -> { 
    let mut i = 0
    i = i + 2
    i
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(main) };

    assert_eq!(code_fn(()), 2);
}

#[test]
fn integer_reassignment_from_param() {
    let input = "
let main = fun (a: Int) -> { 
    let mut b = a + 2
    b = b + 6
    b
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };

    assert_eq!(code_fn((8,)), 16);
}
