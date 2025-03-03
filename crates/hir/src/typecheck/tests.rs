// TODO - tests that return a Context, instead of asserting for an expected
// some way to query the generated Context
//
// right now you can assert the overall type inferred, i.e. the last value of the block,
// but it could be good to have tests that can query based on a string or something like that

use la_arena::Idx;

use crate::typecheck::TypeResult;
use crate::{BlockExpr, Context, Expr, Interner, Type};

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

fn check_infer_type(input: &str, expected: &Type) {
    let mut context = Context::new(Interner::default());

    let result = check(input, &mut context);

    let actual = context.type_(result.ty);

    assert_eq!(actual, expected);
}

/// Use to check types with a context already provided, for example, intern some strings
/// first so they can be asserted later.
fn check_with_context(input: &str, expected: &Type, context: &mut Context) {
    let result = check(input, context);

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

    let expected = Type::sum(vec![
        (red, context.core_types().unit),
        (green, context.core_types().unit),
        (blue, context.core_types().unit),
    ]);

    check_with_context(input, &expected, &mut context);
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

    let expected = Type::sum(vec![
        (red, context.core_types().unit),
        (green, context.core_types().unit),
        (blue, context.core_types().unit),
    ]);

    check_with_context(input, &expected, &mut context);
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

    let expected = Type::sum(vec![
        (red, context.core_types().int),
        (green, context.core_types().unit),
        (blue, context.core_types().bool),
    ]);

    check_with_context(input, &expected, &mut context);
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
