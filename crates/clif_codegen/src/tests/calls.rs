use crate::builtins::{XBool, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[test]
fn is_even_call() {
    let code = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Bool {
    is_even a
}
";
    let main = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XBool>(main) };

    assert_eq!(code_fn((-1,)), FALSE);
    assert_eq!(code_fn((0,)), TRUE);
    assert_eq!(code_fn((1,)), FALSE);
    assert_eq!(code_fn((2,)), TRUE);
}

#[test]
fn is_even_call_conditional() {
    let code = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Int {
    if is_even a {
        16
    } else {
        7
    }
}
";
    let main = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(main) };

    assert_eq!(code_fn((-1,)), 7);
    assert_eq!(code_fn((0,)), 16);
    assert_eq!(code_fn((1,)), 7);
    assert_eq!(code_fn((2,)), 16);
}

#[test]
fn non_capturing_function_can_be_passed_and_called_indirectly() {
    let code = r#"
let map_int = fun (input: Int, mapper: (Int) -> Int) -> Int {
    mapper input
}

let double_mapper = fun (int: Int) -> Int { int * 2 }

let main = fun () -> Int {
    map_int (21, double_mapper)
}
"#;
    let main = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };

    assert_eq!(code_fn(()), 42);
}

#[test]
fn generic_box_map_infers_mapper_type_and_calls_indirectly() {
    let code = r#"
type Box 'A = [ value: 'A ]

let box_map = fun ['A](mapper: ('A) -> 'A, box: Box 'A) -> Box 'A {
    [ value = mapper box.value ]
}

let double_mapper = fun (value: Int) -> Int { value * 2 }

let main = fun () -> Int {
    let input: Box Int = [ value = 21 ]
    let output: Box Int = box_map (double_mapper, input)
    output.value
}
"#;
    let main = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };

    assert_eq!(code_fn(()), 42);
}

#[test]
fn stdlib_option_map_infers_mapper_type_and_calls_indirectly() {
    let code = r#"
// Option from stdlib
// Option.map from stdlib

let double_mapper = fun (value: Int) -> Int { value * 2 }

let main = fun () -> Int {
    let opt: Option Int = Option.some 21
    let mapped: Option Int = Option.map (opt, double_mapper)
    match mapped {
        .none -> 0,
        .some value -> value,
    }
}
"#;
    let main = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };

    assert_eq!(code_fn(()), 42);
}
