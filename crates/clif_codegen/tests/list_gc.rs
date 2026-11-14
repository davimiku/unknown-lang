#![cfg(debug_assertions)]

#[test]
fn growing_list_survives_collection_during_reallocation() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () -> Int {
    let values: List Int = list_new ()
    list_push (values, 1)
    list_push (values, 2)
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
fn string_list_growth_survives_collection() {
    let entries = clif_codegen::compile_module(
        r#"
let main = fun () -> String {
    let values: List String = list_new ()
    let first = "first"
    list_push (values, first)
    list_push (values, "second")
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
let main = fun () -> List [ x: Int, y: Int ] {
    let points: List [ x: Int, y: Int ] = list_new ()
    let first = [ x = 1, y = 2 ]
    let second = [ x = 3, y = 4 ]
    list_push (points, first)
    list_push (points, second)
    points
}
"#,
    )
    .expect("List of Point program to compile");

    let list = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    let list_ref = unsafe { &*(list.as_raw().cast::<runtime::list::HopperList>()) };
    assert_eq!(list_ref.length, 2);
    assert!(list_ref.capacity >= 2);
    let fields = unsafe { std::slice::from_raw_parts(list_ref.elements.as_ptr().cast::<i64>(), 4) };
    assert_eq!(fields, [1, 2, 3, 4]);
    drop(list);
}
