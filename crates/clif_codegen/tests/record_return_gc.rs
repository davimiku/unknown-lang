// An isolated test process: forced GC does not scan other threads' Hopper stacks.
#![cfg(debug_assertions)]

#[test]
fn returned_record_is_rooted_in_caller() {
    let entry_points = clif_codegen::compile_module(
        r#"
type Value = [ number: Int, text: String ]
let make = fun () -> Value { [ number = 42, text = "first" ] }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let record = make ()
    let orphan = "orphan"
    allocate ()
    record.text
}
"#,
    )
    .expect("record returned by Hopper function to compile");
    let main_ptr = entry_points["main"];
    // SAFETY: Hopper main takes no arguments and returns a one-word String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };

    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let first = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("returned record root in caller");
    assert!(
        observed
            .roots
            .iter()
            .any(|root| { root.word == first && root.return_address != observed.return_address }),
        "expected returned field in older Hopper frame: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(first));
}
