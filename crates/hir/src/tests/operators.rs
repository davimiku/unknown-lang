use super::check;
use indoc::indoc;

#[test]
fn int_addition() {
    let input = "1 + 2";

    check(input, "`+`~0.3$0 (1,2,);", &[]);
}

#[test]
fn not_false() {
    let input = "!false";

    check(input, "!false~0.1;", &[]);
}

#[test]
fn not_true() {
    let input = "!true";

    check(input, "!true~0.2;", &[]);
}

#[test]
fn not_variable_ref() {
    let input = r#"
    let a = true
    let b = !a
"#;
    let expected_expr = indoc! {"
        a~1.0 : (false | true) = true~0.2;
        b~1.1 : (false | true) = !a~1.0;"};

    let expected_vars = &[("a~1.0", "(false | true)"), ("b~1.1", "(false | true)")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn int_to_string() {
    let input = "~123";

    check(input, "~123;", &[]);
}

#[test]
fn print_int_to_string() {
    let input = "print ~123";

    check(input, "print~0.0$0 (~123,);", &[]);
}

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;

    check(input, "`++`~0.8$0 (\"Hello \",\"World!\",);", &[]);
}
