// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[test]
fn inactive_string_variant_is_never_scanned_as_pointer() {
    let entries = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
let make = fun () -> Choice { Choice.number 42 }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let keep = "keep"
    let choice = make ()
    let orphan = "orphan"
    allocate ()
    keep
}
"#,
    )
    .expect("integer union variant to compile");
    // SAFETY: main takes no parameters and returns a one-word String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(entries["main"]) };
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let keep = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("roots in caller");
    assert!(observed
        .roots
        .iter()
        .any(|root| root.word == keep && root.return_address != observed.return_address));
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
        "orphan not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(keep));
}
