// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[test]
fn string_argument_survives_allocation_of_following_argument() {
    let entries = clif_codegen::compile_module(
        r#"
let first_arg = fun (first: String, second: String) -> String { first }
let main = fun () -> String {
    let orphan = "orphan"
    first_arg ("first", "second")
}
"#,
    )
    .expect("two String arguments to compile");

    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    // SAFETY: main takes no arguments and returns a one-word String handle.
    let root = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    let collection = runtime::ffi::take_forced_collection()
        .expect("allocating the second argument to see the first live root");
    let observed = runtime::ffi::take_probed_roots().expect("argument root at safepoint");
    assert!(
        observed
            .roots
            .iter()
            .any(|seen| seen.word == root.as_raw() as usize),
        "missing first argument root: {observed:?}"
    );
    assert!(runtime::heap_contains_address(root.as_raw() as usize));
    assert!(
        collection.bytes_after < collection.bytes_before,
        "unreachable string was not reclaimed: {collection:?}"
    );
}
