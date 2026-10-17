use crate::{compile_module, CompileErrors};

fn assert_rejects_gc_aggregate(input: &str) {
    match compile_module(input) {
        Err(CompileErrors::UnsupportedGcAggregate { function, ty }) => {
            assert_eq!(function, "main");
            assert!(!ty.is_empty());
        }
        other => panic!("expected an unsupported GC aggregate error, got {other:?}"),
    }
}

#[test]
fn accepts_record_with_one_string_field() {
    compile_module(
        r#"
let main = fun () -> { [ text = "live", number = 42 ] }
"#,
    )
    .expect("one direct String field has a precise root");
}

#[test]
fn accepts_record_with_multiple_string_fields() {
    compile_module(
        r#"
let main = fun () -> { [ first = "one", second = "two" ] }
"#,
    )
    .expect("all record fields have precise roots during construction");
}

#[test]
fn accepts_nested_record_with_one_string_field() {
    compile_module(
        r#"
let main = fun () -> { [ outer = [ text = "live" ], number = 42 ] }
"#,
    )
    .expect("nested record with one String leaf has a precise root");
}

#[test]
fn accepts_nested_record_with_two_string_fields() {
    compile_module(
        r#"
let main = fun () -> { [ inner = [ text = "one" ], other = "two" ] }
"#,
    )
    .expect("nested record fields have precise roots during construction");
}

#[test]
fn accepts_union_with_string_or_int_payload() {
    compile_module(
        r#"
type Choice = (text: String | number: Int)
let main = fun () -> { Choice.text "live" }
"#,
    )
    .expect("tagged union has a nullable shadow root");
}

#[test]
fn rejects_record_containing_gc_union() {
    assert_rejects_gc_aggregate(
        r#"
type Choice = (text: String | number: Int)
let main = fun (choice: Choice) -> { [ choice = choice, number = 42 ] }
"#,
    );
}

#[test]
fn rejects_union_with_nested_record_string_payload() {
    assert_rejects_gc_aggregate(
        r#"
type Choice = (record: [ text: String ] | number: Int)
let main = fun (choice: Choice) -> { 0 }
"#,
    );
}

#[test]
fn accepts_record_parameter_with_one_string_field() {
    compile_module(
        r#"
type Value = [ text: String, number: Int ]
let main = fun (value: Value) -> { value.number }
"#,
    )
    .expect("record parameter with one direct String field has a precise root");
}

#[test]
fn record_with_string_can_be_copied_and_passed_to_hopper_function() {
    compile_module(
        r#"
type Value = [ text: String, number: Int ]
let use_value = fun (value: Value) -> String {
    let child = "child"
    value.text
}
let main = fun () -> {
    let first = [ text = "first", number = 42 ]
    let copy = first
    use_value copy
}
"#,
    )
    .expect("record copy and call to compile");
}

#[test]
fn pointer_free_aggregates_still_compile() {
    compile_module(
        r#"
type Point = [ x: Int, y: Int ]
let main = fun () -> { [ x = 3, y = 42 ] }
"#,
    )
    .expect("pointer-free aggregates should still compile");
}
