#[test]
fn generic_function_is_monomorphized_before_clif_codegen() {
    let entries = clif_codegen::compile_module(
        r#"
type Box 'A = [ value: 'A ]

let make_box = fun ['A](a: 'A) -> Box 'A {
    [ value = a ]
}

let main = fun () -> Int {
    let boxed: Box Int = make_box 123
    boxed.value
}
"#,
    )
    .expect("generic function should lower to concrete MIR before CLIF");

    // SAFETY: main has no parameters and returns an Int.
    let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(entries["main"]) };
    assert_eq!(main(), 123);
}
