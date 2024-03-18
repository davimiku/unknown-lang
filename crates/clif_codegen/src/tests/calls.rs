use crate::builtins::{XBool, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn is_even_call() {
    let input = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Bool {
    is_even a
}
";
    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(main) };

    assert_eq!(code_fn((-1,)), FALSE);
    assert_eq!(code_fn((0,)), TRUE);
    assert_eq!(code_fn((1,)), FALSE);
    assert_eq!(code_fn((2,)), TRUE);
}

#[test]
fn is_even_call_conditional() {
    let input = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Int {
    if is_even a {
        16
    } else {
        7
    }
}
";
    let main = compile_main(input);

    // let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };

    // assert_eq!(code_fn((-1,)), 7);
    // assert_eq!(code_fn((0,)), 16);
    // assert_eq!(code_fn((1,)), 7);
    // assert_eq!(code_fn((2,)), 16);
}
