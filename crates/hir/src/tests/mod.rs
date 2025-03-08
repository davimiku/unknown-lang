mod branches;
mod functions;
mod literals;
mod logic;
mod loops;
mod operators;
mod scopes;
mod typecheck;
mod unions;

use itertools::Itertools;
use la_arena::Idx;
use lsp_diagnostic::{LSPDiagnostic, LSPDiagnosticSeverity};

use indoc::indoc;
use text_size::TextRange;
use util_macros::assert_matches;

use crate::diagnostic::TypeDiagnosticVariant;
use crate::display::display_module;
use crate::{lower, ContextDisplay, Diagnostic};

fn check(input: &str, expected_content: &str, expected_vars: &[(&str, &str)]) {
    let (module, context) = lower(input);

    if !context.diagnostics.is_empty() {
        for diag in context.diagnostics.iter() {
            eprintln!("{}", diag.display(&context));
        }
    }
    assert_eq!(context.diagnostics, vec![]);

    let expected_vars = expected_vars
        .iter()
        .sorted_by(|(a, ..), (b, ..)| a.cmp(b))
        .map(|(name, ty)| format!(".    {name} : {ty}"))
        .join("\n");

    let (actual_content, actual_vars) = display_module(&module, &context);
    let expected_content = expected_content.trim();
    let actual_content = actual_content.trim();

    if actual_content != expected_content {
        eprintln!("expected: {expected_content}");
        eprintln!("actual  : {actual_content}");
        eprintln!("diff:");
        text_diff::print_diff(expected_content, actual_content, "");
        panic!("Expected Content did not match actual, see printed diff.");
    }

    let expected_vars = expected_vars.trim();
    let actual_vars = actual_vars.trim();
    if actual_vars != expected_vars {
        eprintln!("expected vars:\n{expected_vars}");
        eprintln!("actual vars:\n{actual_vars}");
        eprintln!("diff:");
        text_diff::print_diff(expected_vars, actual_vars, "");
        panic!("Expected Vars did not match actual, see printed diff.");
    }
}

/// Checks that the input lowered with type error(s)
fn check_type_error(input: &str, expected: Vec<(TypeDiagnosticVariant, TextRange)>) {
    let (_, context) = lower(input);

    for (actual, (expected_variant, expected_range)) in
        context.diagnostics.into_iter().zip(expected.into_iter())
    {
        let actual_range = actual.range();
        let actual = assert_matches!(actual, Diagnostic::Type).variant;
        let actual = std::mem::discriminant(&actual);
        let expected = std::mem::discriminant(&expected_variant);
        assert_eq!(actual, expected);
    }
}

#[test]
fn let_binding() {
    let input = "let a = 10";

    check(input, "a~1.0 : 10 = 10;", &[("a~1.0", "10")]);
}

#[test]
fn let_binding_mut() {
    let input = "let mut a = 10";

    check(input, "a~1.0 : mut Int = 10;", &[("a~1.0", "Int")]);
}

#[test]
fn let_binding_annotation() {
    let input = "let a: Int = 10";
    let expected = "a~1.0 : Int~0.0 = 10;";

    check(input, expected, &[("a~1.0", "Int")]);
}

#[test]
fn let_binding_mut_annotation() {
    let input = "let mut a: Int = 10";

    check(input, "a~1.0 : mut Int~0.0 = 10;", &[("a~1.0", "Int")]);
}

#[test]
fn variable_ref() {
    let input = r#"
        let a = 10
        a
"#;

    let expected_expr = indoc! {"
        a~1.0 : 10 = 10;
        a~1.0;"};

    check(input, expected_expr, &[("a~1.0", "10")]);
}

#[test]
fn variable_reassign() {
    let input = "
        let mut a = 10
        a = 20
";

    let expected_expr = indoc! {"
        a~1.0 : mut Int = 10;
        a~1.0 <- 20;"};

    check(input, expected_expr, &[("a~1.0", "Int")]);
}

#[test]
fn variable_reassign_not_mut() {
    let input = "let a = 10
a = 10
";

    let bogus_idx = Idx::from_raw(u32::MAX.into());

    check_type_error(
        input,
        vec![(
            TypeDiagnosticVariant::Immutable { expr: bogus_idx },
            TextRange::new(11.into(), 12.into()),
        )],
    );
}

#[test]
fn variable_reassign_with_add() {
    let input = "
        let mut a = 10
        a = a + 5
";

    let expected_expr = indoc! {"
        a~1.0 : mut Int = 10;
        a~1.0 <- `+`~0.3$0 (a~1.0,5,);"};

    check(input, expected_expr, &[("a~1.0", "Int")]);
}

#[test]
#[ignore = "not implemented yet"]
fn variable_add_reassign() {
    let input = r#"
        let mut a = 10
        a += 5
"#;

    let expected_expr = indoc! {"
        a~1.0 : mut Int = 10;
        a~1.0 <- `+`~0.3$0 (a~1.0,5,);"};

    check(input, expected_expr, &[("a~1.0", "Int")]);
}

#[test]
fn multiple_let_bindings() {
    let input = r#"
        let a = 1
        let b = 2
"#;

    let expected_expr = indoc! {"
        a~1.0 : 1 = 1;
        b~1.1 : 2 = 2;"};
    let expected_vars = &[("a~1.0", "1"), ("b~1.1", "2")];

    check(input, expected_expr, expected_vars);
}

#[test]
#[ignore = "FIXME"]
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

    // check_type_error(input, vec![expected]);
}

#[test]
#[ignore = "FIXME"]
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

    // check_type_error(input, vec![expected]);
}

// #[test]
// fn unary_function_no_param_type() {
//     let mut interner = Interner::default();
//     let key = interner.intern("a");
//     let input = "fun a -> {}";
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
fn let_binding_and_print() {
    let input = r#"
let a = "Hello"
print a"#;

    let expected_expr = indoc! {"
        a~1.0 : \"Hello\" = \"Hello\";
        print~0.0$0 (a~1.0,);"};

    let expected_vars = &[("a~1.0", "\"Hello\"")];

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
        a~1.0 : \"Hello\" = \"Hello\";
        b~1.1 : \" World\" = \" World\";
        print~0.0$0 (b~1.1,);
        print~0.0$0 (a~1.0,);"};

    let expected_vars = &[("a~1.0", "\"Hello\""), ("b~1.1", "\" World\"")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_strings() {
    let input = r#""a" ++ "a""#;

    let expected_expr = indoc! {r#"`++`~0.8$0 ("a","a",);"#};

    let expected_vars = &[];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_in_function() {
    let input = r#"
let repeat = fun (s: String) -> { s ++ s }
"#;

    let expected_expr = indoc! {"
    repeat~1.0 : (String) -> String = fun \"repeat\"(s~1.1 : String) -> String { `++`~0.8$0 (s~1.1,s~1.1,); };"};

    let expected_vars = &[("repeat~1.0", "(String) -> String"), ("s~1.1", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_function_call() {
    let input = r#"
let repeat = fun (s: String) -> String { s ++ s }
let hello_hello = repeat "Hello "
print hello_hello"#;

    let expected_expr = indoc! {"
        repeat~1.0 : (String) -> String = fun \"repeat\"(s~1.1 : String) -> String { `++`~0.8$0 (s~1.1,s~1.1,); };
        hello_hello~1.2 : String = repeat~1.0$0 (\"Hello \",);
        print~0.0$0 (hello_hello~1.2,);"};

    let expected_vars = &[
        ("hello_hello~1.2", "String"),
        ("repeat~1.0", "(String) -> String"),
        ("s~1.1", "String"),
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
    let input = r#"let res = true
if res {
    return 1
}"#;

    let expected = indoc! {"
res~1.0 : (false | true) = true~0.2;
if (res~1.0) { return 1; };"};

    let expected_vars = &[("res~1.0", "(false | true)")];

    check(input, expected, expected_vars);
}

#[test]
fn array_literal_int() {
    let input = r#"let a = [1, 2, 3]"#;

    let expected = indoc! {"
    a~1.0 : []Int = [1,2,3,];"};

    let expected_vars = &[("a~1.0", "[]Int")];

    check(input, expected, expected_vars);
}

#[test]
fn list_literal_string() {
    let input = r#"let a = ["x", "y", "z"]"#;

    let expected = indoc! {"
    a~1.0 : []String = [\"x\",\"y\",\"z\",];"};

    let expected_vars = &[("a~1.0", "[]String")];

    check(input, expected, expected_vars);
}

#[test]
fn always_returns_true() {
    let input = "let main = fun () -> { true }";

    let expected =
        "main~1.0 : () -> (false | true) = fun \"main\"() -> (false | true) { true~0.2; };";

    let expected_vars = &[("main~1.0", "() -> (false | true)")];

    check(input, expected, expected_vars);
}
