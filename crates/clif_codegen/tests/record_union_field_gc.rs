// The forced-collection hook is global, so this is an isolated test binary.
#![cfg(debug_assertions)]

#[test]
fn nested_record_union_field_is_rooted_across_older_frames() {
    let entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
let make = fun () -> Choice { Choice.text "kept" }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let choice = make ()
    let outer = [ inner = [ choice = choice, later = "later" ], count = 42 ]
    allocate ()
    let inner = outer.inner
    let result = inner.choice
    match result {
        .text text -> { text }
        otherwise -> { "unexpected variant" }
    }
}
"#,
    )
    .expect("nested record union field to compile")["main"];

    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    // SAFETY: main takes no arguments and returns a String pointer. No
    // collection or other JIT execution occurs before inspecting the result.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(entry) };
    let kept = main();
    runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("roots across frames");
    assert!(runtime::heap_contains_address(kept));
    assert!(
        observed
            .roots
            .iter()
            .any(|root| root.word == kept && root.return_address != observed.return_address),
        "nested record union root missing: {observed:?}"
    );
}
