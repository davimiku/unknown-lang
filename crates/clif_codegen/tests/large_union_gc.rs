#![cfg(debug_assertions)]

#[test]
fn large_gc_union_uses_tagged_heap_descriptor() {
    let entries = clif_codegen::compile_module(
        r#"
type Choice = (text: String | large: [ one: Int, two: Int, three: Int, four: Int, five: Int, six: Int, seven: Int, eight: Int, text: String ])
let allocate = fun () -> Int {
    let child = "child"
    0
}
let make = fun () -> Choice { Choice.text "first" }
let main = fun () -> String {
    let choice = make ()
    allocate ()
    match choice {
        .text value -> { value }
        otherwise -> { "unexpected" }
    }
}
"#,
    )
    .expect("large GC union to compile");

    // SAFETY: main takes no arguments and returns one GC String word.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(entries["main"]) };
    let result = main();
    assert!(runtime::heap_contains_address(result));
}
