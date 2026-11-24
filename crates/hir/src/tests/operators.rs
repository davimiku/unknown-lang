use super::check;
use crate::diagnostic::{Diagnostic, TypeDiagnosticVariant};
use crate::{Context, Interner, ModuleId, SourceKind};
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
        a~1.0 : Bool~0.3 = true~0.2;
        b~1.1 : Bool~0.3 = !a~1.0;"};

    let expected_vars = &[("a~1.0", "Bool~0.3"), ("b~1.1", "Bool~0.3")];

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

#[test]
fn ufcs_calls_exported_function_from_named_type_module() {
    let input = r#"
        #export
        type Point = [x: Int]

        #export
        let distance_from = fun (a: Point, b: Point) -> { a.x - b.x }

        let first: Point = [x = 10]
        let second: Point = [x = 3]
        first.distance_from second
    "#;

    let (_, context) = crate::lower(input);
    assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);
}

#[test]
fn user_defined_operator_and_explicit_ufcs_use_same_function() {
    let input = r#"
        #export
        type Point = [x: Int]

        #export
        let `+` = fun (a: Point, b: Point) -> { a.x + b.x }

        let first: Point = [x = 10]
        let second: Point = [x = 3]
        let via_dot = first.+ second
        first + second
    "#;

    let (_, context) = crate::lower(input);
    assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);
}

#[test]
fn ufcs_finds_exported_function_for_imported_type() {
    let mut context = Context::new(Interner::default());
    let point_module_name = context.interner.intern("point");
    context.load_module_in(
        ModuleId::BOOTSTRAP,
        point_module_name,
        r#"
            #export
            type Point = [x: Int]

            #export
            let distance_from = fun (a: Point, b: Point) -> { a.x - b.x }
        "#,
        SourceKind::User,
    );

    context.load_module(
        r#"
            type Point = @point/Point
            let first: Point = [x = 10]
            let second: Point = [x = 3]
            first.distance_from second
        "#,
        SourceKind::User,
    );

    assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);
}

#[test]
fn anonymous_record_does_not_get_ufcs_candidates() {
    let input = r#"
        #export
        let foo = fun (point: [x: Int], other: Int) -> { point.x + other }

        let point = [x = 1]
        point.foo 2
    "#;

    let (_, context) = crate::lower(input);
    assert!(matches!(
        context.diagnostics.as_slice(),
        [Diagnostic::Type(diagnostic)]
            if matches!(
                diagnostic.variant,
                TypeDiagnosticVariant::UfcsFunctionNotFound { .. }
            )
    ));
}

#[test]
fn private_function_is_not_a_ufcs_candidate() {
    let input = r#"
        type Point = [x: Int]
        let foo = fun (point: Point, other: Int) -> { point.x + other }

        let point: Point = [x = 1]
        point.foo 2
    "#;

    let (_, context) = crate::lower(input);
    assert!(matches!(
        context.diagnostics.as_slice(),
        [Diagnostic::Type(diagnostic)]
            if matches!(
                diagnostic.variant,
                TypeDiagnosticVariant::UfcsFunctionNotFound { .. }
            )
    ));
}

#[test]
fn private_operator_is_not_a_ufcs_candidate() {
    let input = r#"
        #export
        type Point = [x: Int]
        let `+` = fun (a: Point, b: Point) -> Point { [x = a.x + b.x] }

        let first: Point = [x = 10]
        let second: Point = [x = 3]
        first + second
    "#;

    let (_, context) = crate::lower(input);
    assert!(matches!(
        context.diagnostics.as_slice(),
        [Diagnostic::Type(diagnostic)]
            if matches!(
                diagnostic.variant,
                TypeDiagnosticVariant::UfcsFunctionNotFound { .. }
            )
    ));
}

#[test]
fn record_field_takes_priority_over_same_named_ufcs_function() {
    let input = r#"
        #export
        type Point = [foo: Int]

        #export
        let foo = fun (point: Point, other: Int) -> { point.foo + other }

        let point: Point = [foo = 1]
        point.foo 2
    "#;

    let (_, context) = crate::lower(input);
    assert!(matches!(
        context.diagnostics.as_slice(),
        [Diagnostic::Type(diagnostic)]
            if matches!(
                diagnostic.variant,
                TypeDiagnosticVariant::CalleeNotFunction { .. }
            )
    ));
}

#[test]
fn primitive_modules_export_typed_homogeneous_operators() {
    use crate::{BlockExpr, Expr, Type};

    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();

    for (module_path, operator, operand_ty, result_ty) in [
        (
            ["core", "num", "int"],
            "+",
            context.core_types().int,
            context.core_types().int,
        ),
        (
            ["core", "num", "float"],
            "+",
            context.core_types().float,
            context.core_types().float,
        ),
    ] {
        let module_path = module_path.map(|name| context.interner.intern(name));
        let module = context
            .resolve_module_path(ModuleId::BOOTSTRAP, &module_path)
            .unwrap();
        let operator = context.interner.intern(operator);
        let symbol = context.exported_value(module, operator).unwrap();
        let Type::Function(function) = context.type_of_value(&symbol) else {
            panic!("primitive operator must be a function");
        };
        let signature = &function.signatures[0];
        assert_eq!(signature.params.as_ref(), &[operand_ty, operand_ty]);
        assert_eq!(signature.return_ty, result_ty);
    }

    let int_module_path = ["core", "num", "int"].map(|name| context.interner.intern(name));
    let int_module = context
        .resolve_module_path(ModuleId::BOOTSTRAP, &int_module_path)
        .unwrap();
    let plus_key = context.interner.intern("+");
    let int_plus = context.exported_value(int_module, plus_key).unwrap();
    assert_eq!(
        context.lookup_operator(int_plus),
        Some(crate::IntrinsicExpr::Add)
    );

    let user_module = context.load_module(
        r#"
            let main = fun (a: Int, b: Int) -> Int { a + b }
            let direct_call: Int = Int.+ (1, 2)
        "#,
        SourceKind::User,
    );
    assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);
    let Expr::VarDef(main) = context.expr(user_module.exprs[0]) else {
        panic!("main must be a function definition");
    };
    let Expr::Function(main_function) = context.expr(main.value) else {
        panic!("main must be a function");
    };
    let body = main_function.overloads[0].body;
    let Expr::Block(BlockExpr::NonEmpty { exprs }) = context.expr(body) else {
        panic!("main body must be a non-empty block");
    };
    let Expr::Statement(call) = context.expr(*exprs.last().unwrap()) else {
        panic!("main body must end with a call statement");
    };
    let Expr::Call(call) = context.expr(*call) else {
        panic!("addition must lower to a call");
    };
    assert_eq!(call.symbol, Some(int_plus));
}

#[test]
fn core_operator_fallback_still_supports_stdlib_bool() {
    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    context.load_module("true == false", SourceKind::User);

    assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);
}

#[test]
fn mixed_int_float_arithmetic_requires_explicit_conversion() {
    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    context.load_module(
        indoc::indoc! {"
            let add_int_float = fun (a: Int, b: Float) -> Float { a + b }
            let add_float_int = fun (a: Float, b: Int) -> Float { a + b }
            let sub_int_float = fun (a: Int, b: Float) -> Float { a - b }
            let sub_float_int = fun (a: Float, b: Int) -> Float { a - b }
            let mul_int_float = fun (a: Int, b: Float) -> Float { a * b }
            let mul_float_int = fun (a: Float, b: Int) -> Float { a * b }
            let div_int_float = fun (a: Int, b: Float) -> Float { a / b }
            let div_float_int = fun (a: Float, b: Int) -> Float { a / b }"
        },
        SourceKind::User,
    );

    assert_eq!(context.diagnostics.len(), 8, "{:#?}", context.diagnostics);
    assert!(context
        .diagnostics
        .iter()
        .all(|diagnostic| { matches!(diagnostic, Diagnostic::Type(_)) }));
}
