use itertools::Itertools;
use la_arena::Idx;
use lsp_diagnostic::{LSPDiagnostic, LSPDiagnosticSeverity};

use indoc::indoc;
use text_size::TextRange;
use util_macros::assert_matches;

use crate::{
    diagnostic::TypeDiagnosticVariant, display::display_module, lower, ContextDisplay, Diagnostic,
};

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
        .map(|(name, ty)| format!("{name} : {ty}"))
        .join("\n");

    let (actual_content, actual_vars) = display_module(&module, &context);
    let expected_content = expected_content.trim();
    let actual_content = actual_content.trim();

    if actual_content != expected_content {
        eprintln!("expected: {expected_content}");
        eprintln!("actual  : {actual_content}");
        eprintln!("diff:");
        text_diff::print_diff(expected_content, actual_content, "");
        panic!("Expected did not match actual, see printed diff.");
    }

    let expected_vars = expected_vars.trim();
    let actual_vars = actual_vars.trim();
    if actual_vars != expected_vars {
        eprintln!("expected: {expected_vars}");
        eprintln!("actual  : {actual_vars}");
        eprintln!("diff:");
        text_diff::print_diff(expected_vars, actual_vars, "");
        panic!("Expected did not match actual, see printed diff.");
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
fn int_literal_function_inferred_return() {
    let input = "fun (i: Int) -> { 42 }";

    check(
        input,
        "fun (i~1.0 : Int) -> 42 { 42; };",
        &[("i~1.0", "Int")],
    );
}

#[test]
fn int_literal_function_explicit_return() {
    let input = "fun (i: Int) -> Int { 42 }";

    check(
        input,
        "fun (i~1.0 : Int) -> 42 { 42; };",
        &[("i~1.0", "Int")],
    );
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

    check(input, "`+`~0.1$0 (1,2,);", &[]);
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
        a~1.0 : true = true;
        b~1.1 : false = !a~1.0;"};

    let expected_vars = &[("a~1.0", "true"), ("b~1.1", "false")];

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

    check(input, "`++`~0.6$0 (\"Hello \",\"World!\",);", &[]);
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
    let expected = "a~1.0 : Int~0.1 = 10;";

    check(input, expected, &[("a~1.0", "Int")]);
}

#[test]
fn let_binding_mut_annotation() {
    let input = "let mut a: Int = 10";

    check(input, "a~1.0 : mut Int~0.1 = 10;", &[("a~1.0", "Int")]);
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
        a~1.0 <- `+`~0.1$0 (a~1.0,5,);"};

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
        a~1.0 <- `+`~0.1$0 (a~1.0,5,);"};

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

    // check_type_error(input, vec![expected]);
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

    // check_type_error(input, vec![expected]);
}

#[test]
fn nullary_function() {
    let input = "fun () -> {}";
    let expected_expr = indoc! {"
    fun () -> {};"};

    check(input, expected_expr, &[]);
}

#[test]
fn nullary_function_assignment() {
    let input = "let f = fun () -> {}";
    let expected_expr = indoc! {"
    f~1.0 : () -> () = fun \"f\"() -> {};"};

    check(input, expected_expr, &[("f~1.0", "() -> ()")]);
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
fn unary_function() {
    let input = "fun (a: Int) -> {}";
    let expected_expr = indoc! {"
    fun (a~1.0 : Int) -> {};"};

    check(input, expected_expr, &[("a~1.0", "Int")]);
}

#[test]
fn unary_function_assignment() {
    let input = "let f = fun (a: Int) -> {}";

    let expected_expr = indoc! {"
    f~1.0 : (Int) -> () = fun \"f\"(a~1.1 : Int) -> {};"};
    let expected_vars = &[("a~1.1", "Int"), ("f~1.0", "(Int) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_string() {
    let input = "print \"Hello\"";

    let expected_expr = "print~0.0$0 (\"Hello\",);";

    check(input, expected_expr, &[])
}

#[test]
fn print_int() {
    let input = "print 16";

    let expected_expr = "print~0.0$1 (16,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_float() {
    let input = "print 16.0";

    let expected_expr = "print~0.0$2 (16.0,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_bool() {
    let input = "print true";

    let expected_expr = "print~0.0$3 (true,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_param_function() {
    let input = "fun (a: String) -> { print a }";

    let expected_expr = "fun (a~1.0 : String) -> { print~0.0$0 (a~1.0,); };";
    let expected_vars = &[("a~1.0", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_function_assignment() {
    let input = "let f = fun (a: String) -> { print a }";

    let expected_expr =
        "f~1.0 : (String) -> () = fun \"f\"(a~1.1 : String) -> { print~0.0$0 (a~1.1,); };";
    let expected_vars = &[("a~1.1", "String"), ("f~1.0", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_with_call() {
    let input = r#"
let print_param = fun (a: String) -> { print a }
print_param "Hello!"
"#;

    let expected_expr = indoc! {"
        print_param~1.0 : (String) -> () = fun \"print_param\"(a~1.1 : String) -> { print~0.0$0 (a~1.1,); };
        print_param~1.0$0 (\"Hello!\",);"};

    let expected_vars = &[("a~1.1", "String"), ("print_param~1.0", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn function_call_function() {
    let input = "
let is_even = fun (a: Int) -> { a % 2 == 0 }

let main = fun (a: Int) -> {
    is_even a
}
";

    let expected_expr = indoc! {"
        is_even~1.0 : (Int) -> Bool = fun \"is_even\"(a~1.1 : Int) -> Bool { `==`~0.7$0 (%~0.5$0 (a~1.1,2,),0,); };
        main~1.2 : (Int) -> Bool = fun \"main\"(a~1.3 : Int) -> Bool { is_even~1.0$0 (a~1.3,); };"};

    let expected_vars = &[
        ("a~1.1", "Int"),
        ("a~1.3", "Int"),
        ("is_even~1.0", "(Int) -> Bool"),
        ("main~1.2", "(Int) -> Bool"),
    ];

    check(input, expected_expr, expected_vars);
}

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

    let expected_expr = indoc! {r#"`++`~0.6$0 ("a","a",);"#};

    let expected_vars = &[];

    check(input, expected_expr, expected_vars);
}

#[test]
fn concat_in_function() {
    let input = r#"
let repeat = fun (s: String) -> { s ++ s }
"#;

    let expected_expr = indoc! {"
    repeat~1.0 : (String) -> String = fun \"repeat\"(s~1.1 : String) -> String { `++`~0.6$0 (s~1.1,s~1.1,); };"};

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
        repeat~1.0 : (String) -> String = fun \"repeat\"(s~1.1 : String) -> String { `++`~0.6$0 (s~1.1,s~1.1,); };
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
fn basic_if_else() {
    let input = "
fun (condition: Bool) -> Float { 
    if condition {
        16.0
    } else {
        8.0
    }
}";

    let expected_expr = indoc! {"
    fun (condition~1.0 : Bool) -> Float { if (condition~1.0) { 16.0; } else { 8.0; }; };"};

    let expected_vars = &[("condition~1.0", "Bool")];

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
res~1.0 : true = true;
if (res~1.0) { return 1; };"};

    let expected_vars = &[("res~1.0", "true")];

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
fn array_literal_string() {
    let input = r#"let a = ["x", "y", "z"]"#;

    let expected = indoc! {"
    a~1.0 : []String = [\"x\",\"y\",\"z\",];"};

    let expected_vars = &[("a~1.0", "[]String")];

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

#[test]
fn empty_loop() {
    let input = "loop {}";

    let expected = "loop {};";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_immediate_break() {
    let input = "loop { break }";

    let expected = "loop { break; };";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_break_later() {
    let input = "loop { 
if true { break }
}";

    let expected = "loop { if (true) { break; }; };";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_full_example() {
    let input = "
let main = fun () -> {
    let mut i = 0
    loop {
        if i > 5 { break }
        i = i + 1
    }
    i
}";

    let expected = "main~1.0 : () -> Int = fun \"main\"() -> Int {
    i~1.1 : mut Int = 0;
    loop {
        if (`>`~0.11$0 (i~1.1,5,)) { break; };
        i~1.1 <- `+`~0.1$0 (i~1.1,1,);
    };
    i~1.1;
};";

    let expected_vars = &[("i~1.1", "Int"), ("main~1.0", "() -> Int")];

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

mod typecheck_tests {
    use util_macros::assert_matches;

    use crate::{lower_script, ContextDisplay, Expr, Type};

    fn check(input: &str, expected_return_type: &Type) {
        let (root_expr, context) = lower_script(input);

        if !context.diagnostics.is_empty() {
            for diag in context.diagnostics.iter() {
                eprintln!("{}", diag.display(&context));
            }
        }
        assert_eq!(context.diagnostics, vec![]);

        println!("{}", root_expr.display(&context));
        let root_expr = context.expr(root_expr);
        let root_func = assert_matches!(root_expr, Expr::Function);

        let return_type = context.expr_type(root_func.overloads[0].body);
        assert_eq!(return_type, expected_return_type);
    }

    #[test]
    fn int_literal() {
        let input = "1";

        let expected_return_type = Type::IntLiteral(1);

        check(input, &expected_return_type);
    }

    #[test]
    fn int_addition() {
        let input = "1 + 2";

        let expected_return_type = Type::Int;

        check(input, &expected_return_type);
    }

    #[test]
    fn float_addition() {
        let expected_return_type = Type::Float;

        check("1.0 + 2.0", &expected_return_type);
        check("1 + 2.0", &expected_return_type);
        check("1.0 + 2", &expected_return_type);
    }
}
