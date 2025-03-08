use crate::builtins::XInt;
use crate::tests::{compile_main, to_fn};

#[test]
fn define_and_pass_through_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((0,)), 0);
    assert_eq!(code_fn((1,)), 1);
    assert_eq!(code_fn((2,)), 2);
}

#[test]
fn define_and_use_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun () -> { Color.green }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 1);
}
