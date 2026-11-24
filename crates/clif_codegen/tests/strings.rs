#![cfg(debug_assertions)]

#[test]
fn string_byte_length_concat_and_equality_work_through_public_api() {
    let make_string = |text: &str| {
        clif_codegen::compile_module(&format!("let main = fun () -> String {{ \"{text}\" }}"))
            .expect("String-producing program to compile")
    };
    let left_entries = make_string("hello ");
    let right_entries = make_string("world");
    let left = unsafe { runtime::NativeRoot::call_no_arg_string(left_entries["main"]) };
    let right = unsafe { runtime::NativeRoot::call_no_arg_string(right_entries["main"]) };

    let entries = clif_codegen::compile_module(
        r#"
let main = fun (left: String, right: String) -> Int {
    let joined = String.concat (left, right)
    let operator_joined = left ++ right
    let empty = String.is_empty ("")
    let length = String.byte_len (joined)
    let same = joined == "hello world"
    let operator_same = joined == operator_joined
    let different = joined != "different"
    if length == 11 {
        if same {
            if operator_same {
                if different { if empty { 1 } else { 0 } } else { 0 }
            } else { 0 }
        } else { 0 }
    } else { 0 }
}
"#,
    )
    .expect("String byte length, concat, and equality to compile");

    // The first collecting String operation is concat. Its inputs are native
    // arguments held alive by NativeRoot while the Hopper frame is suspended.
    runtime::ffi::force_collection_at_next_allocation_with_live_root();
    let main: extern "C" fn(runtime::ffi::RawGcRef, runtime::ffi::RawGcRef) -> i64 =
        unsafe { std::mem::transmute(entries["main"]) };
    assert_eq!(main(left.as_raw(), right.as_raw()), 1);
    assert!(runtime::ffi::take_forced_collection().is_some());
}
