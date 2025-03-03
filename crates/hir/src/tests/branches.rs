use expect_test::expect;
use indoc::indoc;

use super::check;

#[test]
fn match_with_basic_union() {
    let input = "
type Color = (red | green | blue)
let main = fun (scrutinee: Color) -> Int {
    match scrutinee {
        .red -> { 8 }
        .green -> { 16 }
        .blue -> { 24 }
    }
}";
    let expected = indoc! {r#"
    Color~1.0 := red: () | green: () | blue: ()
main~1.1 : ((red | green | blue)) -> Int = fun "main"(scrutinee~1.2 : (red | green | blue)) -> Int { match scrutinee~1.2{
    
    
    
}; };"#};

    let expected_vars = &[
        ("main~1.1", "((red | green | blue)) -> Int"),
        ("scrutinee~1.2", "(red | green | blue)"),
    ];

    check(input, expected, expected_vars);
}

#[test]
fn match_with_variant_payloads() {
    let input = "
type Union = (a: Int | b: Float)
let main = fun (scrutinee: Union) -> Int {
    match scrutinee {
        .a a_int -> { 2 },
        .b b_float -> { 3 },
    }
}";
    let expected = indoc! {r#"
    Union~1.0 := a: Int~0.1 | b: Float~0.2
main~1.1 : ((a: Int | b: Float)) -> Int = fun "main"(scrutinee~1.2 : (a: Int | b: Float)) -> Int { match scrutinee~1.2{
    
    
}; };"#};

    let expected_vars = &[
        ("main~1.1", "((a: Int | b: Float)) -> Int"),
        ("scrutinee~1.2", "(a: Int | b: Float)"),
        ("a_int~1.3", "Int"),
        ("b_float~1.4", "Float"),
    ];

    check(input, expected, expected_vars);
}

#[test]
fn match_with_variant_ident_binding() {
    let input = "
type Union = (a: Int | b: Float)
let main = fun (scrutinee: Union) -> Int {
    match scrutinee {
        .a a_int -> { a_int },
        .b b_float -> { 0 },
    }
}";
    let expected = indoc! {r#"
    Union~1.0 := a: Int~0.1 | b: Float~0.2
main~1.1 : ((a: Int | b: Float)) -> Int = fun "main"(scrutinee~1.2 : (a: Int | b: Float)) -> Int { match scrutinee~1.2{
    
    
}; };"#};

    let expected_vars = &[
        ("main~1.1", "((a: Int | b: Float)) -> Int"),
        ("scrutinee~1.2", "(a: Int | b: Float)"),
        ("a_int~1.3", "Int"),
        ("b_float~1.4", "Float"),
    ];

    check(input, expected, expected_vars);
}
