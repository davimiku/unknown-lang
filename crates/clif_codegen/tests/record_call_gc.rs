// An isolated test process: forced GC does not scan other threads' Hopper stacks.
#![cfg(debug_assertions)]

#[test]
fn copied_record_parameter_is_rooted_in_callee() {
    let entry_points = clif_codegen::compile_module(
        r#"
type Value = [ number: Int, text: String ]
let use_value = fun (value: Value) -> String {
    let child = "child"
    value.text
}
let main = fun () -> {
    let record = [ number = 42, text = "first" ]
    let copy = record
    let orphan = "orphan"
    use_value copy
}
"#,
    )
    .expect("record copy and parameter to compile");
    let main_ptr = entry_points["main"];
    // SAFETY: Hopper main takes no arguments and returns a one-word String handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };

    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let first = main();
    let collection = runtime::ffi::take_forced_collection().expect("collection in callee");
    let observed = runtime::ffi::take_probed_roots().expect("record parameter root in callee");
    assert!(
        observed
            .roots
            .iter()
            .any(|root| { root.word == first && root.return_address == observed.return_address }),
        "expected parameter root in allocating Hopper frame: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(first));
}
