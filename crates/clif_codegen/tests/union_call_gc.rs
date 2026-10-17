// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[repr(C)]
#[derive(Default)]
struct Choice {
    tag: i64,
    payload: usize,
}

#[test]
fn gc_union_parameter_is_rooted_in_callee() {
    let entries = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
let make = fun () -> Choice { Choice.text "first" }
let use_choice = fun (choice: Choice) -> Choice {
    let child = "child"
    choice
}
let main = fun () -> Choice {
    let choice = make ()
    let orphan = "orphan"
    use_choice choice
}
"#,
    )
    .expect("union with a GC-bearing parameter to compile");
    // SAFETY: main takes no parameters and uses the caller-provided sret pointer.
    let main: extern "C" fn(*mut Choice) = unsafe { std::mem::transmute(entries["main"]) };
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let mut result = Choice::default();
    main(&mut result);
    let collection = runtime::ffi::take_forced_collection().expect("collection in callee");
    let observed = runtime::ffi::take_probed_roots().expect("union parameter root in callee");
    assert_eq!(result.tag, 0);
    assert!(
        observed
            .roots
            .iter()
            .any(|root| root.word == result.payload
                && root.return_address == observed.return_address),
        "callee failed to root active variant: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(result.payload));
}
