use crate::builtins::{XBool, XInt, FALSE, TRUE};
use crate::tests::{compile_main, to_fn};

#[allow(clippy::unit_cmp)]
#[test]
fn do_nothing() {
    let code = "let main = fun () -> { }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), ()>(code_ptr) };

    assert_eq!(code_fn(()), ());
}

#[test]
fn constant_16() {
    let code = "let main = fun () -> { 16 }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn constant_16_by_addition() {
    let code = "let main = fun () -> { 10 + 6 }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn one_param_constant_16() {
    let code = "let main = fun (i: Int) -> { 16 }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 16);
}

#[test]
fn identity_int() {
    let code = "let main = fun (i: Int) -> { i }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn identity_int_with_variable() {
    let code = "
let main = fun (i: Int) -> {
    let i2 = i
    i2
}";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((16,)), 16);
}

#[test]
fn variable_and_addition() {
    let code = "
let main = fun (i: Int) -> {
    let i2 = i + 10
    i2
}";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((6,)), 16);
    assert_eq!(code_fn((-4,)), 6);
}

#[test]
#[allow(clippy::unit_cmp)]
fn bool_param() {
    let code = "let main = fun (a: Bool) -> { }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XBool,), ()>(code_ptr) };

    assert_eq!(code_fn((TRUE,)), ());
    assert_eq!(code_fn((FALSE,)), ());
}

#[test]
fn bool_return_true() {
    let code = "let main = fun () -> { true }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XBool>(code_ptr) };

    assert_eq!(code_fn(()), TRUE);
}

#[test]
fn bool_return_false() {
    let code = "let main = fun () -> { false }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(), XBool>(code_ptr) };

    assert_eq!(code_fn(()), FALSE);
}

#[test]
fn bool_return_identity() {
    let code = "let main = fun (b: Bool) -> { b }";

    let code_ptr = compile_main(code);

    let code_fn = unsafe { to_fn::<(XBool,), XBool>(code_ptr) };

    assert_eq!(code_fn((TRUE,)), TRUE);
    assert_eq!(code_fn((FALSE,)), FALSE);
}

#[test]
fn list_push_and_len() {
    let code = r#"
let main = fun () -> Int {
    let values: List Int = List.new ()
    List.push (values, 10)
    List.push (values, 20)
    List.len values
}
"#;
    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };
    assert_eq!(code_fn(()), 2);
}

#[test]
fn list_aliases_observe_pushes_across_growth() {
    let code = r#"
let main = fun () -> Int {
    let values: List Int = List.new ()
    let alias = values
    List.push (values, 10)
    List.push (alias, 20)
    List.push (values, 30)
    List.len values
}
"#;
    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };
    assert_eq!(code_fn(()), 3);
}

#[test]
fn list_push_inlines_record_elements() {
    let code = r#"
let main = fun () -> Int {
    let points: List [ x: Int, y: Int ] = List.new ()
    let first = [ x = 1, y = 2 ]
    let second = [ x = 3, y = 4 ]
    let third = [ x = 5, y = 6 ]
    List.push (points, first)
    List.push (points, second)
    List.push (points, third)
    42
}
"#;
    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };
    assert_eq!(code_fn(()), 42);
}
