// Forced collection must run in an isolated test process.
#![cfg(debug_assertions)]

#[repr(C)]
#[derive(Default)]
struct Pair {
    a: usize,
    b: usize,
}

#[test]
fn both_record_fields_survive_collection_in_child() {
    let entries = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let record = [ a = "first", b = "second" ]
    let orphan = "orphan"
    allocate ()
    record
}
"#,
    )
    .expect("two reference fields to compile");
    // SAFETY: main takes no parameters and writes its record to sret.
    let main: extern "C" fn(*mut Pair) = unsafe { std::mem::transmute(entries["main"]) };
    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    let mut result = Pair::default();
    main(&mut result);
    let collection = runtime::ffi::take_forced_collection().expect("collection in child");
    let observed = runtime::ffi::take_probed_roots().expect("record roots in older caller");
    for address in [result.a, result.b] {
        assert!(
            observed
                .roots
                .iter()
                .any(|root| root.word == address && root.return_address != observed.return_address),
            "missing record field: {observed:?}"
        );
        assert!(runtime::heap_contains_address(address));
    }
    assert!(
        collection.bytes_after < collection.bytes_before,
        "orphan was not reclaimed: {collection:?}"
    );
}
