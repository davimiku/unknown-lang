// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[test]
fn switching_to_integer_variant_clears_shadow_root() {
    let entries = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
let make_text = fun () -> Choice { Choice.text "dead" }
let make_number = fun () -> Choice { Choice.number 42 }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let mut choice = make_text ()
    let keep = "keep"
    choice = make_number ()
    allocate ()
    keep
}
"#,
    )
    .expect("union reassignment to compile");
    // SAFETY: main takes no parameters and returns one String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(entries["main"]) };
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let keep = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("roots in caller");
    assert!(
        observed
            .roots
            .iter()
            .any(|root| root.word == 0 && root.return_address != observed.return_address),
        "inactive union root should be null: {observed:?}"
    );
    assert!(
        !observed.roots.iter().any(|root| root.word == 42),
        "integer payload was scanned: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "dead variant was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(keep));
}
