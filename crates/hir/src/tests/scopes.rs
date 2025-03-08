use super::check;
use indoc::indoc;

#[test]
fn multiple_variable_ref() {
    let input = r#"
        let a = 1
        let b = 2
        a
        b
"#;

    let expected_expr = indoc! {"
        a~1.0 : 1 = 1;
        b~1.1 : 2 = 2;
        a~1.0;
        b~1.1;"};
    let expected_vars = &[("a~1.0", "1"), ("b~1.1", "2")];

    check(input, expected_expr, expected_vars);
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

    let expected_expr = indoc! {"
        a~1.0 : 0 = 0;
        {
            a~1.1 : 10 = 10;
            a~1.1;
        };
        a~1.0;"};
    let expected_vars = &[("a~1.0", "0"), ("a~1.1", "10")];

    check(input, expected_expr, expected_vars);
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

    let expected_expr = indoc! {"
        a~1.0 : 0 = 0;
        {
            a~1.0;
            a~1.1 : 10 = 10;
            {
                a~1.1;
                a~1.2 : 20 = 20;
                a~1.2;
            };
            a~1.1;
        };
        a~1.0;"};

    let expected_vars = &[("a~1.0", "0"), ("a~1.1", "10"), ("a~1.2", "20")];

    check(input, expected_expr, expected_vars);
}
