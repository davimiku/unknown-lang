// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[repr(C)]
#[derive(Default)]
struct Pair {
    a: usize,
    b: usize,
}

#[test]
fn first_record_field_survives_allocation_of_second_field() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () -> {
    let orphan = "orphan"
    [ a = "first", b = "second" ]
}
"#,
    )
    .expect("two reference fields to compile");
    // SAFETY: main takes no parameters and writes its stack-record result to sret.
    let main: extern "C" fn(*mut Pair) = unsafe { std::mem::transmute(entries["main"]) };
    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    let mut result = Pair::default();
    main(&mut result);
    let collection =
        runtime::ffi::take_forced_collection().expect("collection during second field allocation");
    let observed = runtime::ffi::take_probed_roots().expect("temporary record root at allocation");
    assert_ne!(result.a, result.b);
    assert!(
        observed
            .roots
            .iter()
            .any(|root| root.word == result.a && root.return_address == observed.return_address),
        "first field not rooted during second allocation: {observed:?}"
    );
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
    assert!(runtime::heap_contains_address(result.a));
    assert!(runtime::heap_contains_address(result.b));
}
