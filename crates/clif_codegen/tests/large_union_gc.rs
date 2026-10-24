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
    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    let result = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection during object allocation");
    assert!(collection.bytes_after <= collection.bytes_before);
    assert!(runtime::heap_contains_address(result));
}
