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
    //     let input = "let main = fun () -> {
    //     let mut i = 0
    //     loop {
    //         if i > 5 { break }
    //         i = i + 1
    //     }
    // }";
    let code_ptr = compile_main(input);

    // let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    // assert_eq!(code_fn(()), 6);
}

/*



*/
