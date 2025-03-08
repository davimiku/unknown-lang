use crate::builtins::XInt;
use crate::tests::{compile_main, to_fn};

#[test]
fn loop_break_condition() {
    let input = "
let main = fun () -> {
    let mut i = 0
    loop {
        if i > 5 { break }
        i = i + 1
    }
    i
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 6);
}

#[test]
fn loop_break_condition_from_param() {
    let input = "
let main = fun (i: Int) -> {
    let mut j = 0
    loop {
        if j > i { break }
        j = j + 1
    }
    j
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 17);
    assert_eq!(code_fn((8,)), 9);
}

#[test]
fn fibonacci_loop() {
    let input = "
let main = fun (n: Int) -> Int {
    let mut a = 0
    let mut b = 1
    loop {
        if n < 1 { break }
        let c = a + b
        a = b
        b = c
        n = n - 1
    }
    a
}";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((0,)), 0);
    assert_eq!(code_fn((1,)), 1);
    assert_eq!(code_fn((2,)), 1);
    assert_eq!(code_fn((3,)), 2);
    assert_eq!(code_fn((4,)), 3);
    assert_eq!(code_fn((5,)), 5);
    assert_eq!(code_fn((6,)), 8);
    assert_eq!(code_fn((7,)), 13);
    assert_eq!(code_fn((8,)), 21);
    assert_eq!(code_fn((9,)), 34);
}
