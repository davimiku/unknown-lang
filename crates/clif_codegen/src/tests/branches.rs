use crate::builtins::{XBool, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn match_with_basic_union() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        .green -> { 16 }
        .blue -> { 24 }
    }
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };
    assert_eq!(code_fn((0,)), 8); // .red
    assert_eq!(code_fn((1,)), 16); // .green
    assert_eq!(code_fn((2,)), 24); // .blue
}

#[test]
fn match_with_one_branch_and_otherwise() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        otherwise -> { 16 }
    }
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };
    assert_eq!(code_fn((0,)), 8); // .red
    assert_eq!(code_fn((1,)), 16); // .green
    assert_eq!(code_fn((2,)), 16); // .blue
}

#[test]
fn match_with_two_branches_and_otherwise() {
    let input = "
type Color = (red | green | blue | purple)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        .purple -> { 12 }
        otherwise -> { 16 }
    }
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };
    assert_eq!(code_fn((0,)), 8); // .red
    assert_eq!(code_fn((1,)), 16); // .green
    assert_eq!(code_fn((2,)), 16); // .blue
    assert_eq!(code_fn((3,)), 12); // .purple
}

#[test]
#[ignore = "FIXME"]
fn match_with_otherwise_using_bound_otherwise() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Color {
    match condition {
        .green -> { 8 }
        otherwise -> { otherwise }
    }
}";

    let main = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };
    assert_eq!(code_fn((0,)), 0); // .red
    assert_eq!(code_fn((1,)), 8); // .green
    assert_eq!(code_fn((2,)), 2); // .blue
}

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

    let code_fn = unsafe { to_fn::<(XBool,), XInt>(main) };
    assert_eq!(code_fn((TRUE,)), 16);
    assert_eq!(code_fn((FALSE,)), 8);
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

    let code_fn = unsafe { to_fn::<(XBool, XInt), XInt>(main) };
    assert_eq!(code_fn((TRUE, 16)), 16);
    assert_eq!(code_fn((FALSE, 16)), 16);
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
