use ast::Root;
use itertools::Itertools;
use text_size::TextRange;

use indoc::indoc;

use crate::typecheck::{TypeDiagnostic, TypeDiagnosticVariant};

use super::*;

macro_rules! cast {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            a
        } else {
            panic!("mismatch variant when cast to {}", stringify!($pat));
        }
    }};
}

fn check(input: &str, expected_expr: &str, expected_vars: &[(&str, &str)]) {
    let mut interner = Interner::default();

    let root: Root = parser::parse(input).into();
    let (root_expr, context) = lower(&root, &mut interner);

    assert_eq!(context.diagnostics, vec![]);

    let expected_expr = expected_expr
        .split('\n')
        .map(|s| format!("    {s}"))
        .join("\n");

    let mut expected_vars = expected_vars
        .iter()
        .chain(&[("print~0", "(String) -> Unit")])
        .map(|(name, ty)| format!("{name} : {ty}"))
        .join("\n");
    if !expected_vars.is_empty() {
        expected_vars.push('\n');
    }

    let expected = format!(
        "{{
{expected_expr}
}}
{expected_vars}"
    );
    let actual = fmt_expr::fmt_root(root_expr, &context);

    assert_eq!(actual, expected);
}

/// Checks that the input lowered with an error
fn check_error(input: &str, expected: Vec<Diagnostic>, interner: Option<Interner>) {
    let mut interner = interner.unwrap_or(Interner::default());
    let root: Root = parser::parse(input).into();
    let (root, context) = lower(&root, &mut interner);

    let main_block = context.expr(root);
    let main_block = cast!(main_block, Expr::Block);

    assert_eq!(context.diagnostics, expected);
}

#[test]
fn int_literal() {
    let input = "1";

    check(input, "1", &[]);
}

#[test]
fn string_literal() {
    let input = r#""Hello""#;

    check(input, "\"Hello\"", &[]);
}

#[test]
fn int_addition() {
    let input = "1 + 2";

    check(input, "1 + 2", &[]);
}

#[test]
fn not_false() {
    let input = "!false";

    check(input, "!false", &[]);
}

#[test]
fn not_true() {
    let input = "!true";

    check(input, "!true", &[]);
}

#[test]
fn not_variable_ref() {
    let input = r#"
    let a = true
    let b = !a
"#;
    let expected_expr = indoc! {"
        a~0 : true = true
        b~0 : false = !a~0"};

    let expected_vars = &[("a~0", "true"), ("b~0", "false")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;

    check(input, "\"Hello \" ++ \"World!\"", &[]);
}

#[test]
fn local_def() {
    let input = "let a = 10";

    check(input, "a~0 : 10 = 10", &[("a~0", "10")]);
}

#[test]
fn local_def_annotation() {
    let input = "let a: Int = 10";

    check(input, "a~0 : Int~0 = 10", &[("a~0", "Int")]);
}

#[test]
fn local_ref() {
    let input = r#"
        let a = 10
        a
"#;

    let expected_expr = indoc! {"
        a~0 : 10 = 10
        a~0"};

    check(input, expected_expr, &[("a~0", "10")]);
}

#[test]
fn multiple_local_def() {
    let input = r#"
        let a = 1
        let b = 2
"#;

    let expected_expr = indoc! {"
        a~0 : 1 = 1
        b~0 : 2 = 2"};
    let expected_vars = &[("a~0", "1"), ("b~0", "2")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn multiple_local_ref() {
    let input = r#"
        let a = 1
        let b = 2
        a
        b
"#;

    let expected_expr = indoc! {"
        a~0 : 1 = 1
        b~0 : 2 = 2
        a~0
        b~0"};
    let expected_vars = &[("a~0", "1"), ("b~0", "2")];

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
        a~0 : 0 = 0
        {
            a~1 : 10 = 10
            a~1
        }
        a~0"};
    let expected_vars = &[("a~0", "0"), ("a~1", "10")];

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
        a~0"};

    let expected_vars = &[("a~0", "0"), ("a~1", "10"), ("a~2", "20")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn local_wrong_type_int_literal() {
    let input = "let a: String = 100";
    //                               16^ ^19
    let expected = vec![TypeDiagnostic {
        range: TextRange::new(16.into(), 19.into()),
        variant: TypeDiagnosticVariant::TypeMismatch {
            expected: Type::String,
            actual: Type::IntLiteral(100),
        },
    }
    .into()];

    check_error(input, expected, None);
}

#[test]
fn local_wrong_type_string_literal() {
    let mut interner = Interner::default();
    let key = interner.intern("Hello World!");

    let input = r#"let a: Int = "Hello World!""#;
    //                              13^            ^27
    let expected = vec![TypeDiagnostic {
        range: TextRange::new(13.into(), 27.into()),
        variant: TypeDiagnosticVariant::TypeMismatch {
            expected: Type::Int,
            actual: Type::StringLiteral(key),
        },
    }
    .into()];

    check_error(input, expected, Some(interner));
}

#[test]
fn nullary_function() {
    let input = "() -> {}";
    let expected_expr = indoc! {"
        fun () -> {
        }"};

    check(input, expected_expr, &[]);
}

#[test]
fn nullary_function_assignment() {
    let input = "let f = () -> {}";
    let expected_expr = indoc! {"
        f~0 : () -> Unit = fun () -> {
        }"};

    check(input, expected_expr, &[("f~0", "() -> Unit")]);
}

#[test]
fn unary_function_no_param_type() {
    let mut interner = Interner::default();
    let key = interner.intern("a");
    let input = "a -> {}";
    let expected = vec![TypeDiagnostic {
        variant: TypeDiagnosticVariant::Undefined {
            name: (key, 0).into(),
        },
        range: Default::default(),
    }
    .into()];

    check_error(input, expected, Some(interner));
}

#[test]
fn unary_function() {
    let input = "(a: Int) -> {}";
    let expected_expr = indoc! {"
        fun (a~0 : Int) -> {
        }"};

    check(input, expected_expr, &[("a~0", "Int")]);
}

#[test]
fn unary_function_assignment() {
    let input = "let f = (a: Int) -> {}";

    let expected_expr = indoc! {"
        f~0 : (Int) -> Unit = fun (a~0 : Int) -> {
        }"};
    let expected_vars = &[("a~0", "Int"), ("f~0", "(Int) -> Unit")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_function() {
    let input = "(a: String) -> print a";

    let expected_expr = "fun (a~0 : String) -> print~0 (a~0,)";
    let expected_vars = &[("a~0", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_function_assignment() {
    let input = "let f = (a: String) -> print a";

    let expected_expr = "f~0 : (String) -> Unit = fun (a~0 : String) -> print~0 (a~0,)";
    let expected_vars = &[("a~0", "String"), ("f~0", "(String) -> Unit")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_with_call() {
    let input = r#"
let print_param = (a: String) -> print a
print_param "Hello!"
"#;

    let expected_expr = indoc! {"
        print_param~0 : (String) -> Unit = fun (a~0 : String) -> print~0 (a~0,)
        print_param~0 (\"Hello!\",)"};

    let expected_vars = &[("a~0", "String"), ("print_param~0", "(String) -> Unit")];

    check(input, expected_expr, expected_vars);
}
