#![cfg(feature = "gc-stress")]

#[test]
fn gc_bearing_record_elements_survive_list_growth_and_collection() {
    let entry = clif_codegen::compile_module(
        r#"
let main = fun () -> List [ text: String, number: Int ] {
    let values: List [ text: String, number: Int ] = list_new ()
    let first = [ text = "first", number = 1 ]
    let second = [ text = "second", number = 2 ]
    list_push (values, first)
    list_push (values, second)
    values
}
"#,
    )
    .expect("List of inline records to compile")["main"];

    // The threshold ensures collections occur while creating String fields
    // and while the List grows from its initial capacity of one.
    unsafe { runtime::configure_automatic_collection_for_test(1) };
    let list_root = unsafe { runtime::NativeRoot::call_no_arg_string(entry) };
    let list = unsafe { &*(list_root.as_raw().cast::<runtime::list::HopperList>()) };
    assert_eq!(list.length, 2);
    assert!(list.capacity >= 2);

    let element0 = unsafe {
        list.elements
            .as_ptr()
            .cast::<usize>()
            .add(1)
            .read_unaligned()
    };
    let element1 = unsafe {
        list.elements
            .as_ptr()
            .cast::<usize>()
            .add(3)
            .read_unaligned()
    };
    assert!(runtime::heap_contains_address(element0));
    assert!(runtime::heap_contains_address(element1));

    runtime::collect_idle();
    assert!(runtime::heap_contains_address(element0));
    assert!(runtime::heap_contains_address(element1));
    drop(list_root);
    runtime::collect_idle();
    assert!(!runtime::heap_contains_address(element0));
    assert!(!runtime::heap_contains_address(element1));
}
