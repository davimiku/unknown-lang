#![cfg(feature = "gc-stress")]

#[test]
fn gc_bearing_large_record_elements_survive_list_growth_and_collection() {
    let entry = clif_codegen::compile_module(
        r#"
let main = fun () {
    let values: List [ text: String, note: String, one: Int, two: Int, three: Int ] = List.new ()
    let first = [ text = "first", note = "first note", one = 1, two = 2, three = 3 ]
    let second = [ text = "second", note = "second note", one = 4, two = 5, three = 6 ]
    List.push (values, first)
    List.push (values, second)
    values.storage
}
"#,
    )
    .expect("List of large inline records to compile")["main"];

    // The threshold forces collections while creating string fields and while
    // the Array's backing buffer grows.
    unsafe { runtime::configure_automatic_collection_for_test(1) };
    let array_root = unsafe { runtime::NativeRoot::call_no_arg_string(entry) };
    let owner = unsafe { &*(array_root.as_raw().cast::<runtime::array::HopperArray>()) };
    let buffer = unsafe { &*owner.buffer.cast::<runtime::array::HopperArrayBuffer>() };
    assert_eq!(buffer.length, 2);
    assert!(buffer.capacity >= 2);

    let string_refs: Vec<_> = (0..10)
        .filter_map(|word| {
            let value = unsafe {
                buffer
                    .elements
                    .as_ptr()
                    .cast::<usize>()
                    .add(word)
                    .read_unaligned()
            };
            runtime::heap_contains_address(value).then_some(value)
        })
        .collect();
    assert_eq!(string_refs.len(), 4);

    runtime::collect_idle();
    assert!(string_refs
        .iter()
        .all(|reference| runtime::heap_contains_address(*reference)));
    drop(array_root);
    runtime::collect_idle();
    assert!(string_refs
        .iter()
        .all(|reference| !runtime::heap_contains_address(*reference)));
}
