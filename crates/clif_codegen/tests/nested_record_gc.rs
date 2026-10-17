// An isolated test process: forced GC does not scan other threads' Hopper stacks.
#![cfg(debug_assertions)]

#[test]
fn nested_record_in_older_frame_survives_collection() {
    let entry_points = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let record = [ count = 42, inner = [ text = "first" ] ]
    let orphan = "orphan"
    allocate ()
    record.inner.text
}
"#,
    )
    .expect("nested record with one String leaf to compile");
    let main_ptr = entry_points["main"];
    // SAFETY: Hopper main takes no arguments and returns a one-word String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };

    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let first = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("nested record root in older caller");
    assert!(
        observed
            .roots
            .iter()
            .any(|root| { root.word == first && root.return_address != observed.return_address }),
        "expected nested field in older Hopper frame: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(first));
}
