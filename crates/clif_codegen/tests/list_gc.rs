#![cfg(debug_assertions)]

#[test]
fn growing_list_survives_collection_during_reallocation() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () -> Int {
    let values: List Int = List.new ()
    List.push (values, 1)
    List.push (values, 2)
    2
}
"#,
    )
    .expect("List Int program to compile");

    // The list is a live GC reference in the current Hopper frame while a
    // growth allocation occurs.
    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(entries["main"]) };
    assert_eq!(main(), 2);
    assert!(runtime::ffi::take_forced_collection().is_some());
}

#[test]
fn list_with_capacity_allocates_the_requested_buffer_size() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () {
    let values: List Int = List.with_capacity 7
    values.storage
}
"#,
    )
    .expect("List with initial capacity to compile");

    let array_root = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    let owner = unsafe { &*(array_root.as_raw().cast::<runtime::array::HopperArray>()) };
    let buffer = unsafe { &*owner.buffer.cast::<runtime::array::HopperArrayBuffer>() };
    assert_eq!(buffer.length, 0);
    assert_eq!(buffer.capacity, 7);
    drop(array_root);
}

#[test]
fn string_list_growth_survives_collection() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () -> String {
    let values: List String = List.new ()
    let first = "first"
    List.push (values, first)
    List.push (values, "second")
    "ok"
}
"#,
    )
    .expect("List String program to compile");

    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    let result = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    assert!(runtime::heap_contains_address(result.as_raw() as usize));
    assert!(runtime::ffi::take_forced_collection().is_some());
    drop(result);
}

#[test]
fn record_list_growth_copies_multiword_elements_inline() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () {
    let points: List [ x: Int, y: Int ] = List.new ()
    let first = [ x = 1, y = 2 ]
    let second = [ x = 3, y = 4 ]
    List.push (points, first)
    List.push (points, second)
    points.storage
}
"#,
    )
    .expect("Array of Point program to compile");

    let array = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    let owner = unsafe { &*(array.as_raw().cast::<runtime::array::HopperArray>()) };
    let buffer = unsafe { &*owner.buffer.cast::<runtime::array::HopperArrayBuffer>() };
    assert_eq!(buffer.length, 2);
    assert!(buffer.capacity >= 2);
    let fields = unsafe { std::slice::from_raw_parts(buffer.elements.as_ptr().cast::<i64>(), 4) };
    assert_eq!(fields, [1, 2, 3, 4]);
    drop(array);
}
