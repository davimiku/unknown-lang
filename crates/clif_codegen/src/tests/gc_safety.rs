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
fn accepts_record_containing_gc_union() {
    compile_module(
        r#"
type Choice = (text: String | number: Int)
let main = fun (choice: Choice) -> { [ choice = choice, number = 42 ] }
"#,
    )
    .expect("union field has a separate nullable shadow root");
}

#[test]
fn accepts_union_with_nested_record_string_payload() {
    compile_module(
        r#"
type Choice = (record: [ first: String, inner: [ second: String ] ] | number: Int)
let main = fun (choice: Choice) -> { 0 }
"#,
    )
    .expect("record variant references have per-variant shadow roots");
}

#[test]
fn rejects_nested_gc_union_variant() {
    assert_rejects_gc_aggregate(
        r#"
type Inner = (text: String | number: Int)
type Outer = (nested: Inner | number: Int)
let main = fun (choice: Outer) -> { 0 }
"#,
    );
}

#[test]
fn rejects_gc_union_nested_in_union_record_payload() {
    assert_rejects_gc_aggregate(
        r#"
type Inner = (text: String | number: Int)
type Outer = (record: [ choice: Inner ] | number: Int)
let main = fun (choice: Outer) -> { 0 }
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

#[test]
fn large_heap_records_support_parameters_returns_and_nested_values() {
    compile_module(
        r#"
type Value = [ one: Int, two: Int, three: Int, four: Int, five: Int, six: Int, seven: Int, eight: Int, text: String ]
let make = fun (text: String) -> Value {
    [ one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, text = text ]
}
let use_value = fun (value: Value) -> Value {
    let child = "child"
    value
}
let main = fun () -> String {
    let value = make "first"
    let nested = [ one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, text = value.text ]
    let result = use_value nested
    result.text
}
"#,
    )
    .expect("large heap records to support aggregate ABI paths");
}

#[test]
fn large_heap_records_support_reassignment() {
    compile_module(
        r#"
type Value = [ one: Int, two: Int, three: Int, four: Int, five: Int, six: Int, seven: Int, eight: Int, text: String ]
let make = fun (text: String) -> Value {
    [ one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, text = text ]
}
let main = fun () -> String {
    let mut value = make "old"
    value = make "new"
    value.text
}
"#,
    )
    .expect("large heap record reassignment to compile");
}
