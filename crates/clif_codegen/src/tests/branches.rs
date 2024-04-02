use crate::builtins::{XBool, XFloat, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn basic_if_else() {
    let input = "
    let main = fun (condition: Bool) -> Int { 
        if condition {
            16
        } else {
            8
        }
    }";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XBool,), XFloat>(main) };
    assert_eq!(code_fn((TRUE,)), 16.0);
    assert_eq!(code_fn((FALSE,)), 8.0);
}

#[test]
fn empty_then() {
    // the `mut` makes the type inferred as `Int` instead of `0` to guard
    // against future possible optimizations (could be optimized to "return 0")
    let input = "
let main = fun (condition: Bool, i: Int) -> Int {
    if condition {}
    i
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XBool, XInt), XInt>(main) };
    assert_eq!(code_fn((TRUE, 16)), 16);
    assert_eq!(code_fn((FALSE, 16)), 16);
}

#[test]
fn no_else_block() {
    let input = "
let main = fun (condition: Bool, i: Int) -> Int {
    if condition {
        let j = 1
    }
    i
}";

    let main = compile_main(input);
}

#[test]
fn if_else_with_addition_after() {
    let input = "
let main = fun (condition: Bool, b: Int) -> Int { 
    let a = if condition {
        16
    } else {
        8
    }
    a + b
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XBool, XInt), XInt>(main) };
    assert_eq!(code_fn((TRUE, 2)), 18);
    assert_eq!(code_fn((FALSE, 2)), 10);
}
