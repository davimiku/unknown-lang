use ast::Root;
use expect_test::expect;

use super::*;

/// Checks that the input is successfully lowered.
///
/// Compares the fmt version of the lowered expression to the expected value.
fn check(input: &str, expected: expect_test::Expect) {
    let root: Root = parser::parse(input).into();

    let mut interner = Interner::default();

    let (root_expr, context) = lower(&root, &mut interner);

    let actual = fmt_expr::fmt_root(root_expr, &context);

    expected.assert_eq(&actual);
}

#[test]
fn int_literal() {
    let input = "1";
    let expected = expect![["
{
    1
}"]];

    check(input, expected);
}

#[test]
fn string_literal() {
    let input = r#""Hello""#;
    let expected = expect![[r#"
{
    "Hello"
}"#]];

    check(input, expected);
}

#[test]
fn int_addition() {
    let input = "1 + 2";
    let expected = expect![[r#"
{
    1 + 2
}"#]];

    check(input, expected)
}

#[test]
fn not_false() {
    let input = "!false";
    let expected = expect![[r#"
{
    !false
}"#]];

    check(input, expected)
}

#[test]
fn not_true() {
    let input = "!true";
    let expected = expect![[r#"
{
    !true
}"#]];

    check(input, expected)
}

#[test]
fn not_variable_ref() {
    let input = r#"
    let a = true
    let b = !a
"#;
    let expected = expect![[r#"
{
    a~0 : true = true
    b~0 : false = !a~0
}
a~0 : true
b~0 : false
"#]];

    check(input, expected)
}

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;
    let expected = expect![[r#"
{
    "Hello " ++ "World!"
}"#]];

    check(input, expected)
}

#[test]
fn local_def() {
    let input = "let a = 10";
    let expected = expect![[r#"
{
    a~0 : 10 = 10
}
a~0 : 10
"#]];

    check(input, expected);
}

#[test]
fn local_def_annotation() {
    let input = "let a: Int = 10";
    let expected = expect![[r#"
{
    a~0 : Int~0 = 10
}
a~0 : Int
"#]];

    check(input, expected)
}

#[test]
fn local_ref() {
    let input = r#"
        let a = 10
        a
"#;
    let expected = expect![[r#"
{
    a~0 : 10 = 10
    a~0
}
a~0 : 10
"#]];

    check(input, expected);
}

#[test]
fn multiple_local_def() {
    let input = r#"
        let a = 1
        let b = 2
"#;
    let expected = expect![[r#"
{
    a~0 : 1 = 1
    b~0 : 2 = 2
}
a~0 : 1
b~0 : 2
"#]];

    check(input, expected);
}

#[test]
fn multiple_local_ref() {
    let input = r#"
        let a = 1
        let b = 2
        a
        b
"#;
    let expected = expect![[r#"
{
    a~0 : 1 = 1
    b~0 : 2 = 2
    a~0
    b~0
}
a~0 : 1
b~0 : 2
"#]];

    check(input, expected);
}

#[test]
fn one_level_nested_scope() {
    let input = r#"
        let a = 0
        {
            let a = 10
            a
        }
        a
"#;
    let expected = expect![[r#"
{
    a~0 : 0 = 0
    {
        a~1 : 10 = 10
        a~1
    }
    a~0
}
a~0 : 0
a~1 : 10
"#]];

    check(input, expected);
}

#[test]
fn two_level_nested_scope() {
    let input = r#"
        let a = 0
        {
            a
            let a = 10
            {
                a
                let a = 20
                a
            }
            a
        }
        a
"#;
    let expected = expect![[r#"
{
    a~0 : 0 = 0
    {
        a~0
        a~1 : 10 = 10
        {
            a~1
            a~2 : 20 = 20
            a~2
        }
        a~1
    }
    a~0
}
a~0 : 0
a~1 : 10
a~2 : 20
"#]];

    check(input, expected);
}

#[test]
fn local_wrong_type() {
    let input = "let a: String = 1";
    let expected = expect![[r#""#]];

    check(input, expected);
}

#[test]
fn nullary_function() {
    let input = "() -> {}";
    let expected = expect![[r#"
{
    fun () -> {
    }
}"#]];

    check(input, expected);
}

#[test]
fn unary_function_no_param_type() {
    let input = "a -> {}";
    let expected = expect![[r#"
{
    fun (a~0 : {{empty}}) -> {
    }
}"#]];

    check(input, expected);
}

#[test]
fn unary_function() {
    let input = "(a: Int) -> {}";
    let expected = expect![[r#"
{
    fun (a~0 : Int) -> {
    }
}"#]];

    check(input, expected);
}

#[test]
fn print_param() {
    let input = "(a: String) -> print a";
    let expected = expect![[r#""#]];

    check(input, expected);
}
