// TODO - tests that return a Context, instead of asserting for an expected
// some way to query the generated Context
//
// right now you can assert the overall type inferred, i.e. the last value of the block,
// but it could be good to have tests that can query based on a string or something like that

use la_arena::Idx;
use util_macros::assert_matches;

use crate::diagnostic::{TypeDiagnostic, TypeDiagnosticVariant};
use crate::typecheck::check::is_subtype;
use crate::typecheck::TypeResult;
use crate::{lower, BlockExpr, Context, Diagnostic, Expr, FunctionType, Interner, Type};

use super::infer_expr;

fn check(input: &str, context: &mut Context) -> TypeResult {
    let parse = parser::parse(input);
    if !parse.errors().is_empty() {
        dbg!(parse.errors());
        assert!(parse.errors().is_empty());
    }

    let syntax = parse.syntax();
    let root = ast::Root::cast(syntax).expect("valid Root node");

    let exprs: Vec<Idx<Expr>> = root
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    // wrap everything in a block
    let root = Expr::Block(if exprs.is_empty() {
        BlockExpr::Empty
    } else {
        BlockExpr::NonEmpty { exprs }
    });
    let root = context.alloc_expr(root, None);

    let result = infer_expr(root, context);
    assert!(result.is_ok());
    result
}

fn lower_with_stdlib(input: &str) -> (crate::Module, Context) {
    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    let module = context.load_module(input, crate::SourceKind::User);
    (module, context)
}

fn check_infer_type(input: &str, expected: &Type) {
    let mut context = Context::new(Interner::default());

    let result = check(input, &mut context);

    let actual = context.type_(result.ty);

    assert_eq!(actual, expected);
}

#[test]
fn infer_int_literal() {
    let input = "1";
    let expected = Type::IntLiteral(1);

    check_infer_type(input, &expected);
}

#[test]
fn infer_int_addition() {
    let input = "2 + 3";
    let expected = Type::Int;

    check_infer_type(input, &expected);
}

#[test]
fn infer_unit() {
    let input = "()";
    let expected = Type::Unit;

    check_infer_type(input, &expected);
}

#[test]
fn infer_nominal_list_and_builtin_operations() {
    let input = "{
        let values: List Int = List.new ()
        List.push (values, 42)
        List.len values
    }";

    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    let result = check(input, &mut context);
    assert_eq!(context.type_(result.ty), &Type::Int);
}

#[test]
fn infer_list_type_application() {
    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    let result = check("let values: List String = List.new ()", &mut context);

    let Type::Unit = context.type_(result.ty) else {
        panic!("list declaration should have unit type")
    };

    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    let result = check("let values: List (String) = List.new ()", &mut context);
    assert_eq!(context.type_(result.ty), &Type::Unit);
}

#[test]
fn generic_call_requires_enough_type_context() {
    let (_, context) = lower_with_stdlib("let values = List.new ()");
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::CannotInferTypeArguments,
                ..
            })
        )
    }));
}

#[test]
fn generic_call_infers_type_arguments_from_expected_return_type() {
    let input = r#"
        let make_empty = fun ['A]() -> List 'A {
            List.new ()
        }
        let values: List Int = make_empty ()

        let return_empty = fun ['A]() -> List 'A {
            return List.new ()
        }
        let returned: List Int = return_empty ()

        let identity = fun ['A](value: 'A) -> 'A {
            value
        }
        let nested: List String = identity (List.new ())
        let selected: List Int = if true { List.new () } else { List.new () }
        let record: [ values: List Float ] = [ values = List.new () ]
    "#;
    let (_, context) = lower_with_stdlib(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );
}

#[test]
fn intrinsic_builtin_signature_is_checked_against_hopper_annotation() {
    let mut context = Context::new(Interner::default());
    context.load_stdlib().unwrap();
    context.load_module(
        "type Array = @core/collections/array/Array\n\
         #builtin(\"Array.len\")\n\
         let len: (Array 'A) -> String = intrinsic",
        crate::SourceKind::StandardLibrary,
    );

    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::TypeMismatch { .. },
                ..
            })
        )
    }));
}

#[test]
fn polymorphic_list_push_checks_the_element_type() {
    let (_, context) = lower_with_stdlib(
        r#"
            let values: List Int = List.new ()
            List.push (values, "not an Int")
        "#,
    );
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::TypeMismatch { .. },
                ..
            })
        )
    }));
}

#[test]
fn generic_box_alias_is_substituted_through_function_annotations() {
    let input = r#"
        type Box 'A = [ value: 'A ]

        let set_box_value = fun (box: Box 'A, new_value: 'A) {
            box.value = new_value
            box
        }

        let get_box_value = fun (box: Box 'A) -> 'A {
            box.value
        }
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );

    fn function_type<'a>(context: &'a Context, name: &str) -> &'a FunctionType {
        let symbol = context
            .database
            .value_names
            .iter()
            .find_map(|(symbol, key)| (context.lookup(*key) == name).then_some(*symbol))
            .expect("function symbol exists");
        let ty = context.type_database.value_symbols[&symbol];
        let Type::Function(function) = context.type_(ty) else {
            panic!("expected function type")
        };
        function
    }

    fn box_element_type(context: &Context, ty: Idx<Type>) -> Idx<Type> {
        let Type::Product(product) = context.type_(ty) else {
            panic!("expected Box to expand to a product type")
        };
        product
            .fields
            .iter()
            .find_map(|(key, (_, ty))| (context.lookup(*key) == "value").then_some(*ty))
            .expect("Box has a value field")
    }

    let set = &function_type(&context, "set_box_value").signatures[0];
    assert_eq!(set.params.len(), 2);
    assert_eq!(
        box_element_type(&context, set.params[0]),
        set.params[1],
        "the Box element and new_value must share one type parameter"
    );
    assert_eq!(
        context.type_(set.params[0]),
        context.type_(set.return_ty),
        "set_box_value should return the same Box type it receives"
    );

    let get = &function_type(&context, "get_box_value").signatures[0];
    assert_eq!(
        box_element_type(&context, get.params[0]),
        get.return_ty,
        "get_box_value should return the Box element type"
    );
}

#[test]
fn explicit_function_type_parameters_are_available_to_annotations() {
    let input = r#"
        type Box 'A = [ value: 'A ]
        let get_box_value = fun ['A](box: Box 'A) -> 'A {
            box.value
        }
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );

    let symbol = context
        .database
        .value_names
        .iter()
        .find_map(|(symbol, key)| (context.lookup(*key) == "get_box_value").then_some(*symbol))
        .expect("function symbol exists");
    let ty = context.type_database.value_symbols[&symbol];
    let Type::Function(function) = context.type_(ty) else {
        panic!("expected function type")
    };
    let signature = &function.signatures[0];
    let Type::Product(box_type) = context.type_(signature.params[0]) else {
        panic!("expected Box to expand to a product type")
    };
    let element_type = box_type
        .fields
        .iter()
        .find_map(|(key, (_, ty))| (context.lookup(*key) == "value").then_some(*ty))
        .expect("Box has a value field");
    assert_eq!(element_type, signature.return_ty);

    let (_, context) = lower("let invalid = fun ['A](value: 'B) -> 'B { value }");
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::UnresolvedVarRef { .. },
                ..
            })
        )
    }));
}

#[test]
fn generic_alias_application_substitutes_concrete_arguments() {
    let input = r#"
        type Box 'A = [ value: 'A ]
        let boxed: Box Int = [ value = 42 ]
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );
}

#[test]
fn generic_alias_supports_multiple_parameters_and_checks_arity() {
    let input = r#"
        type Pair ('A, 'B) = [ first: 'A, second: 'B ]
        let pair: Pair (Int, String) = [ first = 42, second = "answer" ]
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );

    let (_, context) = lower("type Box 'A = [ value: 'A ]\nlet invalid: Box = [ value = 1 ]");
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::ArgsMismatch {
                    expected: 1,
                    actual: 0,
                },
                ..
            })
        )
    }));
}

#[test]
fn generic_tagged_union_constructors_instantiate_payload_types() {
    let input = r#"
        type Option 'A = (none | some: 'A)

        let some = Option.some 123
        let some_int: Option Int = some
        let none: Option Int = Option.none

        match some {
            .none -> { 0 }
            .some value -> { value }
        }
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );
}

#[test]
fn generic_tagged_union_constructor_preserves_payload_type() {
    let input = r#"
        type Option 'A = (none | some: 'A)
        let invalid: Option String = Option.some 123
    "#;
    let (_, context) = lower(input);
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::TypeMismatch { .. },
                ..
            })
        )
    }));
}

#[test]
fn generic_function_calls_instantiate_box_and_pair_types() {
    let input = r#"
        type Box 'A = [ value: 'A ]
        type Pair ('A, 'B) = [ first: 'A, second: 'B ]

        let make_box = fun ['A](a: 'A) -> Box 'A {
            [ value = a ]
        }
        let make_pair = fun ['A, 'B](a: 'A, b: 'B) -> Pair ('A, 'B) {
            [ first = a, second = b ]
        }

        let int_box: Box Int = make_box 123
        let string_box: Box String = make_box "hello"
        let pair: Pair (Int, String) = make_pair (42, "answer")
    "#;
    let (_, context) = lower(input);
    assert!(
        context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        context.diagnostics
    );
}

#[test]
fn generic_function_call_rejects_inconsistent_type_arguments() {
    let input = r#"
        let choose = fun ['A](first: 'A, second: 'A) -> 'A {
            first
        }
        let invalid = choose (1, "two")
    "#;
    let (_, context) = lower(input);
    assert!(context.diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic,
            Diagnostic::Type(TypeDiagnostic {
                variant: TypeDiagnosticVariant::TypeMismatch { .. },
                ..
            })
        )
    }));
}

#[test]
fn infer_let_binding() {
    let input = "let a = 1";
    let expected = Type::Unit; // the let binding itself is Unit

    check_infer_type(input, &expected);
}

#[test]
fn infer_block() {
    let input = "{
    let a = 1
    a
}";
    let expected = Type::IntLiteral(1);

    check_infer_type(input, &expected);
}

#[test]
fn infer_union_implicit_unit() {
    let mut context = Context::new(Interner::default());
    let red = context.interner.intern("red");
    let green = context.interner.intern("green");
    let blue = context.interner.intern("blue");

    let input = "{
        type Color = (red | green | blue)

        Color.green
}";

    let expected = Type::sum(
        Box::new([
            (red, context.core_types().unit),
            (green, context.core_types().unit),
            (blue, context.core_types().unit),
        ]),
        None,
        None,
    );
    let expected_sum = assert_matches!(expected, Type::Sum);

    let result = check(input, &mut context);

    let actual = context.type_(result.ty);
    let actual = assert_matches!(actual, Type::Sum);
    for (actual, expected) in actual.variants.iter().zip(expected_sum.variants) {
        assert_eq!(actual.0, expected.0);
        assert!(is_subtype(actual.1, expected.1, &context));
        assert!(is_subtype(expected.1, actual.1, &context));
    }
}

#[test]
fn infer_union_explicit_unit() {
    let mut context = Context::new(Interner::default());
    let red = context.interner.intern("red");
    let green = context.interner.intern("green");
    let blue = context.interner.intern("blue");

    let input = "{
        type Color = (red: () | green: () | blue: ())

        Color.green
}";

    let expected = Type::sum(
        Box::new([
            (red, context.core_types().unit),
            (green, context.core_types().unit),
            (blue, context.core_types().unit),
        ]),
        None,
        None,
    );
    let expected_sum = assert_matches!(expected, Type::Sum);

    let result = check(input, &mut context);

    let actual = context.type_(result.ty);
    let actual = assert_matches!(actual, Type::Sum);
    for (actual, expected) in actual.variants.iter().zip(expected_sum.variants) {
        assert_eq!(actual.0, expected.0);
        assert!(is_subtype(actual.1, expected.1, &context));
        assert!(is_subtype(expected.1, actual.1, &context));
    }
}

#[test]
fn infer_union_with_payload_types() {
    let mut context = Context::new(Interner::default());
    let red = context.interner.intern("red");
    let green = context.interner.intern("green");
    let blue = context.interner.intern("blue");

    let input = "{
        type Color = (red: Int | green: () | blue: Bool)

        Color.green
}";

    let expected = Type::sum(
        Box::new([
            (red, context.core_types().int),
            (green, context.core_types().unit),
            (blue, context.core_types().bool),
        ]),
        None,
        None,
    );
    let expected_sum = assert_matches!(expected, Type::Sum);

    let result = check(input, &mut context);

    let actual = context.type_(result.ty);
    let actual = assert_matches!(actual, Type::Sum);
    for (actual, expected) in actual.variants.iter().zip(expected_sum.variants) {
        assert_eq!(actual.0, expected.0);
        assert!(is_subtype(actual.1, expected.1, &context));
        assert!(is_subtype(expected.1, actual.1, &context));
    }
}

#[test]
fn infer_match_arms() {
    let input = "{
    type Color = (red | green | blue)

    match Color.green {
        .red -> { 8 }
        .green -> { 16 }
        .blue -> { 24 }
    }
}";

    let expected = Type::Int;

    check_infer_type(input, &expected);
}

#[test]
fn infer_match_arms_with_data() {
    let input = "{
    type Number = (int: Int | float: Float)

    let n = Number.int 16

    match n {
        .int i -> { 0.0 }
        .float f -> { f }
    }
}";

    let expected = Type::Float;

    check_infer_type(input, &expected);
}
