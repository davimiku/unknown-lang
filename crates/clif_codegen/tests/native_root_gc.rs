// Isolated binary: forced collection uses the global heap and must not collect
// values held by unrelated tests.
#![cfg(debug_assertions)]

#[test]
fn native_handle_keeps_returned_string_alive_across_hopper_collection() {
    let returned_entries =
        clif_codegen::compile_module(r#"let main = fun () -> { "retained by native" }"#)
            .expect("returning program to compile");
    let trigger_entries = clif_codegen::compile_module(
        r#"
let main = fun () -> {
    let live = "live"
    let orphan = "orphan"
    live
}
"#,
    )
    .expect("allocating program to compile");
    // SAFETY: both main functions take no arguments and return one String
    // pointer word; the compiled JIT entries and maps remain registered.
    let handle = unsafe { runtime::NativeRoot::call_no_arg_string(returned_entries["main"]) };
    let raw = handle.as_raw();

    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    // SAFETY: this entry has the same no-argument String-returning ABI.
    let _trigger = unsafe { runtime::NativeRoot::call_no_arg_string(trigger_entries["main"]) };
    assert!(runtime::ffi::take_forced_collection().is_some());
    assert!(runtime::heap_contains_address(raw as usize));

    runtime::collect_idle();
    assert!(runtime::heap_contains_address(raw as usize));
    drop(handle);
    drop(_trigger);
    runtime::collect_idle();
    assert!(!runtime::heap_contains_address(raw as usize));
}
