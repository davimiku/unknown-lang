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

#[test]
#[ignore = "need to remove Bool builtin"]
fn stdlib_option_calls() {
    let is_none_code = r#"
let main = fun () -> Int {
    let opt_none: Option Int = Option.none
    let its_none = Option.is_none opt_none

    match is_none {
        .false -> 0,
        .true -> 42,
    }
}
"#;
    let main = compile_main(is_none_code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };
    assert_eq!(code_fn(()), 42);

    let is_some_code = r#"
let main = fun () -> Int {
    let opt_some: Option Int = Option.some 123
    let its_some = Option.is_some opt_some

    match is_some {
        .false -> 0,
        .true -> 42,
    }
}
"#;
    let main = compile_main(is_some_code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };
    assert_eq!(code_fn(()), 42);
}

#[test]
fn user_defined_operator_uses_ufcs_function_call() {
    let code = r#"
type Point = [x: Int]

#export
let distance_from = fun (a: Point, b: Point) -> Int { a.x + b.x }

#export
let `+` = fun (a: Point, b: Point) -> Int { a.x + b.x }

let main = fun () -> Int {
    let first: Point = [x = 20]
    let second: Point = [x = 22]
    let via_method = first.distance_from second
    let via_operator = first + second
    let via_direct_call = Int.+ (20, 22)
    via_method + via_operator + via_direct_call
}
"#;
    let main = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(main) };

    assert_eq!(code_fn(()), 126);
}
