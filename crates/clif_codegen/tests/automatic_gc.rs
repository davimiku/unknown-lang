// Stress mode uses the global heap: keep all phases in one isolated test binary.
#![cfg(feature = "gc-stress")]

#[test]
fn threshold_collection_keeps_hopper_and_native_roots_across_threads() {
    let baseline_entry =
        clif_codegen::compile_module(r#"let main = fun () -> String { "held by native" }"#)
            .expect("baseline program to compile")["main"];
    let older_frame_entry = clif_codegen::compile_module(
        r#"
let allocate = fun () -> Int {
    let transient = "transient"
    0
}
let main = fun () -> String {
    let orphan = "orphan"
    let kept = "kept in caller"
    allocate ()
    kept
}
"#,
    )
    .expect("older-frame program to compile")["main"];
    let worker_entry =
        clif_codegen::compile_module(r#"let main = fun () -> String { "returned on worker" }"#)
            .expect("worker program to compile")["main"] as usize;
    let call_arguments_entry = clif_codegen::compile_module(
        r#"
let first_arg = fun (first: String, second: String) -> String { first }
let main = fun () -> String {
    let orphan = "orphan"
    first_arg ("first", "second")
}
"#,
    )
    .expect("call-argument program to compile")["main"];
    let record_entry = clif_codegen::compile_module(
        r#"
let main = fun () -> String {
    let orphan = "orphan"
    let record = [ first = "first", second = "second" ]
    record.first
}
"#,
    )
    .expect("record program to compile")["main"];

    let active_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int | decimal: Float)
let make = fun () -> Choice { Choice.text "active" }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let choice = make ()
    let orphan = "orphan"
    allocate ()
    match choice {
        .text value -> { value }
        otherwise -> { "unexpected variant" }
    }
}
"#,
    )
    .expect("active union program to compile")["main"];
    let inactive_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int | decimal: Float)
let make = fun () -> Choice { Choice.decimal 3.5 }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let keep = "keep"
    let choice = make ()
    let orphan = "orphan"
    allocate ()
    keep
}
"#,
    )
    .expect("inactive union program to compile")["main"];
    let reassigned_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int | decimal: Float)
let make_text = fun () -> Choice { Choice.text "dead" }
let make_number = fun () -> Choice { Choice.number 42 }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let mut choice = make_text ()
    let keep = "keep"
    choice = make_number ()
    let orphan = "orphan"
    allocate ()
    keep
}
"#,
    )
    .expect("reassigned union program to compile")["main"];

    let record_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (record: [ first: String, second: String ] | other: [ count: Int, text: String ] | number: Int)
let make = fun () -> Choice {
    let value = [ first = "first", second = "second" ]
    Choice.record value
}

let consume = fun (choice: Choice) -> String {
    let child = "child"
    match choice {
        .record value -> { value.second }
        .other value -> { value.text }
        otherwise -> { "unexpected variant" }
    }
}
let main = fun () -> String {
    let choice = make ()
    let orphan = "orphan"
    consume choice
}

"#,
    )
    .expect("record-payload union program to compile")["main"];
    let other_record_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (record: [ first: String, second: String ] | other: [ count: Int, text: String ] | number: Int)
let make = fun () -> Choice {
    let value = [ count = 42, text = "other" ]
    Choice.other value
}
let main = fun () -> String {
    let choice = make ()
    let orphan = "orphan"
    let child = "child"
    match choice {
        .other value -> { value.text }
        otherwise -> { "unexpected variant" }
    }
}
"#,
    )
    .expect("alternate record-payload union program to compile")["main"];

    let record_with_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
type Wrapped = [ choice: Choice, later: String ]
let make = fun () -> Choice { Choice.text "first" }
let use_wrapped = fun (value: Wrapped) -> String {
    let child = "child"
    let choice = value.choice
    match choice {
        .text text -> { text }
        otherwise -> { value.later }
    }
}
let main = fun () -> String {
    let choice = make ()
    let wrapped = [ choice = choice, later = "later" ]
    let copy = wrapped
    use_wrapped copy
}
"#,
    )
    .expect("record containing active union to compile")["main"];
    let inactive_record_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
type Wrapped = [ choice: Choice, later: String ]
let make = fun () -> Choice { Choice.number 42 }
let use_wrapped = fun (value: Wrapped) -> String {
    let child = "child"
    value.later
}
let main = fun () -> String {
    let choice = make ()
    let wrapped = [ choice = choice, later = "later" ]
    use_wrapped wrapped
}
"#,
    )
    .expect("record containing inactive union to compile")["main"];

    let reassigned_record_union_entry = clif_codegen::compile_module(
        r#"
type Choice = (text: String | number: Int)
type Wrapped = [ choice: Choice, later: String ]
let make_text = fun () -> Choice { Choice.text "old" }
let make_number = fun () -> Choice { Choice.number 42 }
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> String {
    let initial = make_text ()
    let mut wrapped: Wrapped = [ choice = initial, later = "previous" ]
    let replacement = make_number ()
    wrapped = [ choice = replacement, later = "current" ]
    allocate ()
    wrapped.later
}
"#,
    )
    .expect("reassigned record union to compile")["main"];

    // SAFETY: each entry is a registered, no-argument String-returning JIT
    // function. NativeRoot registers each result before releasing the gate.
    let baseline = unsafe { runtime::NativeRoot::call_no_arg_string(baseline_entry) };
    let object_size = runtime::allocated_bytes_for_test();
    assert!(object_size > 0);

    // SAFETY: this binary has no unregistered native-held GC references. The
    // helper takes an idle gate reservation before changing the threshold.
    unsafe { runtime::configure_automatic_collection_for_test(3 * object_size) };
    let kept = unsafe { runtime::NativeRoot::call_no_arg_string(older_frame_entry) };
    // One orphan died at the threshold; the older Hopper frame and native
    // baseline remained rooted, and the new child object was allocated.
    assert_eq!(runtime::allocated_bytes_for_test(), 3 * object_size);
    assert!(runtime::heap_contains_address(baseline.as_raw() as usize));
    assert!(runtime::heap_contains_address(kept.as_raw() as usize));

    // Collection raises its next threshold; reset it for a second automatic
    // cycle, this time from another thread with two live native handles.
    unsafe { runtime::configure_automatic_collection_for_test(3 * object_size) };
    let worker = std::thread::spawn(move || {
        // SAFETY: the registered worker entry has the same String-returning ABI.
        unsafe { runtime::NativeRoot::call_no_arg_string(worker_entry as *const u8) }
    })
    .join()
    .expect("worker to finish");
    assert_eq!(runtime::allocated_bytes_for_test(), 3 * object_size);
    assert!(runtime::heap_contains_address(baseline.as_raw() as usize));
    assert!(runtime::heap_contains_address(kept.as_raw() as usize));
    assert!(runtime::heap_contains_address(worker.as_raw() as usize));

    drop(baseline);
    drop(kept);
    runtime::collect_idle();
    assert_eq!(runtime::allocated_bytes_for_test(), object_size);
    drop(worker);
    runtime::collect_idle();
    assert_eq!(runtime::allocated_bytes_for_test(), 0);

    // Both paths previously relied on debug-only forced collections: the
    // first GC-valued call argument and a partially built stack record must
    // remain mapped while evaluating their second string literals.
    for entry in [call_arguments_entry, record_entry] {
        unsafe { runtime::configure_automatic_collection_for_test(2 * object_size) };
        let result = unsafe { runtime::NativeRoot::call_no_arg_string(entry) };
        // Record construction now also creates a managed record object. The
        // exact total therefore depends on the record layout, but both String
        // objects must still be present before the idle collection.
        assert!(runtime::allocated_bytes_for_test() >= 2 * object_size);
        assert!(runtime::heap_contains_address(result.as_raw() as usize));
        drop(result);
        runtime::collect_idle();
        assert_eq!(runtime::allocated_bytes_for_test(), 0);
    }

    // A low threshold collects during union construction and in a younger
    // Hopper frame, including when the active variant is not a GC reference.
    for (case, entry) in [
        active_union_entry,
        inactive_union_entry,
        reassigned_union_entry,
        record_union_entry,
        other_record_union_entry,
        record_with_union_entry,
        inactive_record_union_entry,
        reassigned_record_union_entry,
    ]
    .into_iter()
    .enumerate()
    {
        unsafe { runtime::configure_automatic_collection_for_test(object_size) };
        let result = unsafe { runtime::NativeRoot::call_no_arg_string(entry) };
        assert!(
            runtime::heap_contains_address(result.as_raw() as usize),
            "union case {case} returned a collected object"
        );
        runtime::collect_idle();
        assert_eq!(runtime::allocated_bytes_for_test(), object_size);
        drop(result);
        runtime::collect_idle();
        assert_eq!(runtime::allocated_bytes_for_test(), 0);
    }
}
