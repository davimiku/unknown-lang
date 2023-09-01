use itertools::Itertools;
use lsp_diagnostic::{LSPDiagnostic, LSPDiagnosticSeverity};

use indoc::indoc;

use crate::{display_root, lower, BlockExpr, Expr, LowerTarget};

macro_rules! cast {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            a
        } else {
            panic!("mismatch variant when cast to {}", stringify!($pat));
        }
    }};
}

fn _print(input: &str) {
    let (root_expr, context) = lower(input, LowerTarget::Module);

    let root_expr = cast!(context.expr(root_expr), Expr::Block);
    if let BlockExpr::NonEmpty { exprs } = root_expr {
        for expr in exprs {
            let expr = context.expr(*expr);
            println!("{expr:?}");
        }
    }
}

fn check(input: &str, expected: &str, expected_vars: &[(&str, &str)]) {
    let (root_expr, context) = lower(input, LowerTarget::Module);

    assert_eq!(context.diagnostics, vec![]);

    let expected_expr = expected.split('\n').map(|s| format!("    {s}")).join("\n");

    let expected_vars = &mut [("args~0.1", "[]String"), ("print~0.0", "(String) -> ()")]
        .iter()
        .chain(expected_vars)
        .sorted_by(|(a, ..), (b, ..)| a.cmp(b))
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
    let actual = display_root(root_expr, &context);

    if actual != expected {
        println!("expected: {expected}");
        println!("actual: {actual}");
        println!("diff:");
        text_diff::print_diff(&expected, &actual, "");
        panic!("Expected did not match actual, see printed diff.");
    }
}

/// Checks that the input lowered with error(s)
fn check_error(input: &str, expected: Vec<LSPDiagnostic>) {
    let (root, context) = lower(input, LowerTarget::Module);

    let main_block = context.expr(root);
    let _ = cast!(main_block, Expr::Block);

    let diagnostics: Vec<LSPDiagnostic> = context.diagnostics.iter().map(|d| d.into()).collect();

    assert_eq!(diagnostics, expected);
}

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

#[test]
fn int_addition() {
    let input = "1 + 2";

    check(input, "1 + 2;", &[]);
}

#[test]
fn not_false() {
    let input = "!false";

    check(input, "!false;", &[]);
}

#[test]
fn not_true() {
    let input = "!true";

    check(input, "!true;", &[]);
}

#[test]
fn not_variable_ref() {
    let input = r#"
    let a = true
    let b = !a
"#;
    let expected_expr = indoc! {"
        a~0.2 : true = true;
        b~0.3 : false = !a~0.2;"};

    let expected_vars = &[("a~0.2", "true"), ("b~0.3", "false")];

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

    check(input, "print~0.0 (~123,);", &[]);
}

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;

    check(input, "\"Hello \" ++ \"World!\";", &[]);
}

#[test]
fn let_binding() {
    let input = "let a = 10";

    check(input, "a~0.2 : 10 = 10;", &[("a~0.2", "10")]);
}

#[test]
fn let_binding_annotation() {
    let input = "let a: Int = 10";
    let expected = "a~0.2 : Int~0.1 = 10;";

    check(input, expected, &[("a~0.2", "Int")]);
}

#[test]
fn variable_ref() {
    let input = r#"
        let a = 10
        a
"#;

    let expected_expr = indoc! {"
        a~0.2 : 10 = 10;
        a~0.2;"};

    check(input, expected_expr, &[("a~0.2", "10")]);
}

#[test]
fn multiple_let_bindings() {
    let input = r#"
        let a = 1
        let b = 2
"#;

    let expected_expr = indoc! {"
        a~0.2 : 1 = 1;
        b~0.3 : 2 = 2;"};
    let expected_vars = &[("a~0.2", "1"), ("b~0.3", "2")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn multiple_variable_ref() {
    let input = r#"
        let a = 1
        let b = 2
        a
        b
"#;

    let expected_expr = indoc! {"
        a~0.2 : 1 = 1;
        b~0.3 : 2 = 2;
        a~0.2;
        b~0.3;"};
    let expected_vars = &[("a~0.2", "1"), ("b~0.3", "2")];

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
        a~0.2 : 0 = 0;
        {
            a~0.3 : 10 = 10;
            a~0.3;
        };
        a~0.2;"};
    let expected_vars = &[("a~0.2", "0"), ("a~0.3", "10")];

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
        a~0.2 : 0 = 0;
        {
            a~0.2;
            a~0.3 : 10 = 10;
            {
                a~0.3;
                a~0.4 : 20 = 20;
                a~0.4;
            };
            a~0.3;
        };
        a~0.2;"};

    let expected_vars = &[("a~0.2", "0"), ("a~0.3", "10"), ("a~0.4", "20")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn local_wrong_type_int_literal() {
    let input: &str = "let a: String = 100";
    //                               16^ ^19
    let expected: LSPDiagnostic = LSPDiagnostic {
        range: (16, 19),
        severity: LSPDiagnosticSeverity::Error,
        message: "TODO".to_owned(),
        code: None,
        code_description: None,
        source: Some("type_checker".to_owned()),
        tags: None,
    };

    check_error(input, vec![expected]);
}

#[test]
fn local_wrong_type_string_literal() {
    let input: &str = r#"let a: Int = "Hello World!""#;
    //                              13^            ^27
    let expected: LSPDiagnostic = LSPDiagnostic {
        range: (13, 27),
        severity: LSPDiagnosticSeverity::Error,
        message: "TODO".to_owned(),
        code: None,
        code_description: None,
        source: Some("type_checker".to_owned()),
        tags: None,
    };

    check_error(input, vec![expected]);
}

#[test]
fn nullary_function() {
    let input = "() -> {}";
    let expected_expr = indoc! {"
    fun () -> {};"};

    check(input, expected_expr, &[]);
}

#[test]
fn nullary_function_assignment() {
    let input = "let f = () -> {}";
    let expected_expr = indoc! {"
    f~0.2 : () -> () = fun<f> () -> {};"};

    check(input, expected_expr, &[("f~0.2", "() -> ()")]);
}

// #[test]
// fn unary_function_no_param_type() {
//     let mut interner = Interner::default();
//     let key = interner.intern("a");
//     let input = "a -> {}";
//     let expected = vec![TypeDiagnostic {
//         variant: TypeDiagnosticVariant::UndefinedSymbol {
//             name: (key, 0).into(),
//         },
//         range: Default::default(),
//     }
//     .into()];

//     check_error(input, expected, Some(interner));
// }

#[test]
fn unary_function() {
    let input = "(a: Int) -> {}";
    let expected_expr = indoc! {"
    fun (a~0.2 : Int) -> {};"};

    check(input, expected_expr, &[("a~0.2", "Int")]);
}

#[test]
fn unary_function_assignment() {
    let input = "let f = (a: Int) -> {}";

    let expected_expr = indoc! {"
    f~0.2 : (Int) -> () = fun<f> (a~0.3 : Int) -> {};"};
    let expected_vars = &[("a~0.3", "Int"), ("f~0.2", "(Int) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_string() {
    let input = "print \"Hello\"";

    let expected_expr = "print~0.0 (\"Hello\",);";

    check(input, expected_expr, &[])
}

#[test]
fn int() {
    let input = "1";

    let expected_expr = "1;";

    check(input, expected_expr, &[])
}

#[test]
fn print_param_function() {
    let input = "(a: String) -> print a";

    let expected_expr = "fun (a~0.2 : String) -> print~0.0 (a~0.2,);";
    let expected_vars = &[("a~0.2", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_function_assignment() {
    let input = "let f = (a: String) -> print a";

    let expected_expr = "f~0.2 : (String) -> () = fun<f> (a~0.3 : String) -> print~0.0 (a~0.3,);";
    let expected_vars = &[("a~0.3", "String"), ("f~0.2", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_with_call() {
    let input = r#"
let print_param = (a: String) -> print a
print_param "Hello!"
"#;

    let expected_expr = indoc! {"
        print_param~0.2 : (String) -> () = fun<print_param> (a~0.3 : String) -> print~0.0 (a~0.3,);
        print_param~0.2 (\"Hello!\",);"};

    let expected_vars = &[("a~0.3", "String"), ("print_param~0.2", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn let_binding_and_print() {
    let input = r#"
let a = "Hello"
print a"#;

    let expected_expr = indoc! {"
        a~0.2 : \"Hello\" = \"Hello\";
        print~0.0 (a~0.2,);"};

    let expected_vars = &[("a~0.2", "\"Hello\"")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn multiple_local_defs_and_print() {
    let input = r#"
let a = "Hello"
let b = " World"
print b
print a"#;

    let expected_expr = indoc! {"
        a~0.2 : \"Hello\" = \"Hello\";
        b~0.3 : \" World\" = \" World\";
        print~0.0 (b~0.3,);
        print~0.0 (a~0.2,);"};

    let expected_vars = &[("a~0.2", "\"Hello\""), ("b~0.3", "\" World\"")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_function() {
    let input = r#"
let repeat = (s: String) -> s ++ s
"#;

    let expected_expr = indoc! {"
    repeat~0.2 : (String) -> String = fun<repeat> (s~0.3 : String) -> s~0.3 ++ s~0.3;"};

    let expected_vars = &[("repeat~0.2", "(String) -> String"), ("s~0.3", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_function_call() {
    let input = r#"
let repeat = (s: String) -> { s ++ s }
let hello_hello = repeat "Hello "
print hello_hello"#;

    let expected_expr = indoc! {"
        repeat~0.2 : (String) -> String = fun<repeat> (s~0.3 : String) -> {
            s~0.3 ++ s~0.3;
        };
        hello_hello~0.4 : String = repeat~0.2 (\"Hello \",);
        print~0.0 (hello_hello~0.4,);"};

    let expected_vars = &[
        ("hello_hello~0.4", "String"),
        ("repeat~0.2", "(String) -> String"),
        ("s~0.3", "String"),
    ];

    check(input, expected_expr, expected_vars);
}

#[test]
fn plain_return_statement() {
    let input = r#"return 1"#;

    let expected_expr = indoc! {"
    return 1;"};

    let expected_vars = &[];

    check(input, expected_expr, expected_vars);
}

#[test]
fn conditional_return() {
    let input = r#"let res = 2 + 3
if res != 5 {
    return 1
}"#;

    let expected = indoc! {"
res~0.2 : 5 = 2 + 3;
if (res~0.2 != 5) {
    return 1;
};"};

    let expected_vars = &[("res~0.2", "5")];

    check(input, expected, expected_vars);
}

#[test]
fn array_literal_int() {
    let input = r#"let a = [1, 2, 3]"#;

    let expected = indoc! {"
    a~0.2 : []Int = [1,2,3,];"};

    let expected_vars = &[("a~0.2", "[]Int")];

    check(input, expected, expected_vars);
}

#[test]
fn array_literal_string() {
    let input = r#"let a = ["x", "y", "z"]"#;

    let expected = indoc! {"
    a~0.2 : []String = [\"x\",\"y\",\"z\",];"};

    let expected_vars = &[("a~0.2", "[]String")];

    check(input, expected, expected_vars);
}

#[test]
fn array_literal_index() {
    let input = "[0, 1, 2].1";

    let expected = indoc! {"
    [0,1,2,].1;"};

    let expected_vars = &[];

    check(input, expected, expected_vars);
}
