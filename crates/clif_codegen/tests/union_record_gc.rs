// Forced collection probes are available only in debug builds. Keep the
// collector's global test hook isolated from the rest of the test suite.
#![cfg(debug_assertions)]

#[test]
fn record_variant_keeps_both_references_in_older_frame() {
    let entry = clif_codegen::compile_module(
        r#"
type Choice = (record: [ first: String, second: String ] | number: Int)
let make = fun () -> Choice {
    let value = [ first = "first", second = "second" ]
    Choice.record value
}
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let choice = make ()
    allocate ()
    match choice {
        .record value -> { value.first }
        otherwise -> { "unexpected variant" }
    }
}
"#,
    )
    .expect("record variant to compile")["main"];

    runtime::ffi::force_collection_at_next_allocation_with_older_root();
    // SAFETY: main is a no-argument function returning a String pointer. The
    // raw pointer is inspected without another collection or native callback.
    let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(entry) };
    let first = main();
    let observed = runtime::ffi::take_probed_roots().expect("collection in callee");
    runtime::ffi::take_forced_collection().expect("collection occurred");
    let mut older: Vec<_> = observed
        .roots
        .iter()
        .filter(|root| root.return_address != observed.return_address && root.word != 0)
        .map(|root| root.word)
        .collect();
    older.sort_unstable();
    older.dedup();
    assert!(older.contains(&first), "missing first field: {observed:?}");
    assert!(
        older.len() >= 2
            && older
                .iter()
                .all(|&word| runtime::heap_contains_address(word)),
        "missing second live record field: {observed:?}"
    );
}
