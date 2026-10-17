// One test process: collection operates on the global runtime heap.
#![cfg(debug_assertions)]

#[test]
fn empty_string_literal_survives_a_later_collection() {
    let entries = clif_codegen::compile_module(r#"let main = fun () -> String { "" }"#)
        .expect("empty String literal to compile");
    // SAFETY: main takes no arguments and returns one GC String pointer word.
    let root = unsafe { runtime::NativeRoot::call_no_arg_string(entries["main"]) };
    let address = root.as_raw() as usize;
    assert!(runtime::heap_contains_address(address));
    runtime::collect_idle();
    assert!(runtime::heap_contains_address(address));
    drop(root);
    runtime::collect_idle();
    assert!(!runtime::heap_contains_address(address));
}
