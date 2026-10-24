// An isolated test process: forced GC does not scan other threads' Hopper stacks.
#![cfg(debug_assertions)]

#[test]
fn record_field_in_older_frame_survives_collection() {
    let entry_points = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let record = [
        first_number = 1,
        second_number = 2,
        third_number = 3,
        fourth_number = 4,
        fifth_number = 5,
        sixth_number = 6,
        seventh_number = 7,
        eighth_number = 8,
        text = "first"
    ]
    let orphan = "orphan"
    allocate ()
    record.text
}
"#,
    )
    .expect("record with a direct String field to compile");
    let main_ptr = entry_points["main"];
    // SAFETY: Hopper main takes no arguments and returns a one-word String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };

    runtime::ffi::probe_next_live_allocation_roots();
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let first = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("record field root in older caller");
    assert!(
        observed
            .roots
            .iter()
            .any(|root| {
                root.return_address != observed.return_address
                    && runtime::heap_contains_address(root.word)
            }),
        "expected heap record root in older Hopper frame: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(first));
}
