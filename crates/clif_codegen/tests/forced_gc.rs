// Run in its own test binary so no other JIT test can have live values in the
// global heap during the deliberately forced collection.
#![cfg(debug_assertions)]

#[test]
fn collection_keeps_older_frame_root_and_reclaims_dead_string() {
    let entry_points = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let orphan = "orphan"
    let first = "first"
    allocate ()
    first
}
"#,
    )
    .expect("test program to compile");
    let main_ptr = entry_points["main"];
    // SAFETY: Hopper main takes no arguments and returns a one-word string handle.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };

    runtime::ffi::probe_next_live_allocation_roots();
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let first = main();
    let collection = runtime::ffi::take_forced_collection().expect("forced collection to run");
    let observed = runtime::ffi::take_probed_roots().expect("older caller root to be observed");
    assert!(observed
        .roots
        .iter()
        .any(|root| { root.word == first && root.return_address != observed.return_address }));
    assert!(collection.bytes_after < collection.bytes_before);
    assert!(runtime::heap_contains_address(first));
}
