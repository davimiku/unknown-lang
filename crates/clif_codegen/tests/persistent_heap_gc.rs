#![cfg(feature = "gc-stress")]

#[test]
fn persistent_large_records_and_unions_survive_automatic_collection() {
    let record_entry = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let value = [ one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, text = "record" ]
    allocate ()
    value.text
}
"#,
    )
    .expect("large record program to compile")["main"];
    let union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | large: [ one: Int, two: Int, three: Int, four: Int, five: Int, six: Int, seven: Int, eight: Int, text: String ])
let make = fun () -> Choice { Choice.text "union" }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let choice = make ()
    allocate ()
    match choice {
        .text value -> { value }
        otherwise -> { "unexpected" }
    }
}
"#,
    )
    .expect("large union program to compile")["main"];

    unsafe { runtime::configure_automatic_collection_for_test(1) };
    let record = unsafe { runtime::NativeRoot::call_no_arg_string(record_entry) };
    assert!(runtime::heap_contains_address(record.as_raw() as usize));
    drop(record);
    runtime::collect_idle();

    unsafe { runtime::configure_automatic_collection_for_test(1) };
    let union = unsafe { runtime::NativeRoot::call_no_arg_string(union_entry) };
    assert!(runtime::heap_contains_address(union.as_raw() as usize));
    drop(union);
    runtime::collect_idle();
}
