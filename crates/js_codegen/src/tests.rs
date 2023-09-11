use super::codegen;

fn check(input: &str, expected: &str) {
    let (program, context) = hir::lower(input, hir::LowerTarget::Module);

    let actual = codegen(context.expr(program), &context);

    assert_eq!(actual, expected);
}

#[test]
fn empty() {
    let input = "";
    let expected = "";

    check(input, expected);
}

#[test]
fn integer() {
    let input = "123";
    let expected = "123;\n";

    check(input, expected);
}

#[test]
fn variable_addition() {
    let input = r#"
let a = 5
let b = 2
let result = a + b"#;
    // TODO: fix the extra space around the `+` operator
    let expected = r#"let a = 5;
let b = 2;
let result =  a  +  b ;
"#;

    check(input, expected);
}

#[test]
fn string_concat() {
    let input = r#""#;
}
