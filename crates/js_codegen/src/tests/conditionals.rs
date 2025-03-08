use super::check;

#[test]
fn empty_if_else() {
    let input = "if true {} else {}";
    let expected = "if (true) {
} else {
};
";

    check(input, expected);
}

#[test]
fn basic_if_else() {
    let input = "if true { 1 } else { 2 }";
    let expected = "if (true) {
    1;
} else {
    2;
};
";

    check(input, expected);
}

#[test]
#[ignore = "it works but the indentation is off"]
fn if_else_assignment() {
    let input = "let x = if true { 1 } else { 2 }";
    let expected = r#"let x;
if (true) {
    x = 1;
} else {
    x = 2;
};
"#;

    check(input, expected);
}
