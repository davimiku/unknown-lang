use super::check;

#[test]
fn int_literal() {
    let input = "1";

    check(input, "1;", &[]);
}

#[test]
fn string_literal() {
    let input = r#""Hello""#;

    check(input, "\"Hello\";", &[]);
}

#[test]
fn int_literal_function_inferred_return() {
    let input = "fun (i: Int) -> { 42 }";

    check(
        input,
        "fun (i~1.0 : Int) -> 42 { 42; };",
        &[("i~1.0", "Int")],
    );
}

#[test]
fn int_literal_function_explicit_return() {
    let input = "fun (i: Int) -> Int { 42 }";

    check(
        input,
        "fun (i~1.0 : Int) -> 42 { 42; };",
        &[("i~1.0", "Int")],
    );
}

#[test]
fn multiple_string_literals() {
    let input = r#""a1"
"b2"
"c3"
"d4""#;

    let expected = r#""a1";
"b2";
"c3";
"d4";"#;

    check(input, expected, &[]);
}
