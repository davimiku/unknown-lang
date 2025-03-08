use super::check;

#[test]
fn addition() {
    let input = "let a = 1 + 2";
    let expected = "let a = 1 + 2;\n";

    check(input, expected);
}

#[test]
fn variable_addition() {
    let input = r#"
let a = 5
let b = 2
let result = a + b"#;

    let expected = r#"let a = 5;
let b = 2;
let result = a + b;
"#;

    check(input, expected);
}
