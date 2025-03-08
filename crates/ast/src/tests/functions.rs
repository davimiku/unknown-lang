use crate::{Expr, Function, TypeExpr};
use util_macros::{assert_matches, assert_some};

use super::parse_expr;

fn check_function(
    parsed: Expr,
    expected_idents: &[&str],
    expected_type_idents: &[&str],
    expected_return_type: Option<&str>,
) -> Function {
    let function = assert_matches!(parsed, Expr::Function);
    let param_list = function.param_list();
    let params = param_list.params();

    for (i, (expected_ident, param)) in expected_idents.iter().zip(params).enumerate() {
        let expected_type_ident = expected_type_idents.get(i);
        let ident = assert_some!(param.ident());
        assert_eq!(ident.as_string(), *expected_ident);
        // TODO: only works for simple idents
        if let Some(expected_type_ident) = expected_type_ident {
            let type_ident = assert_matches!(assert_some!(param.type_expr()), TypeExpr::Ident);
            assert_eq!(type_ident.as_string(), *expected_type_ident);
        }
    }

    // TODO: only works for simple idents
    if let Some(expected_return_type) = expected_return_type {
        let return_type = function.return_type();
        if let Some(TypeExpr::Ident(ident)) = return_type {
            assert_eq!(ident.as_string(), expected_return_type);
        } else {
            panic!("expected a return type");
        }
    }

    function
}

#[test]
fn empty_function() {
    let input = "fun () -> { }";
    let expected_idents = [];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents, None);
}

#[test]
fn unary_function() {
    let input = "fun a -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents, None);
}

#[test]
fn unary_function_with_parens() {
    let input = "fun (a) -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents, None);
}

#[test]
fn unary_function_with_param_type() {
    let input = "fun (a: A) -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = ["A"];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents, None);
}

#[test]
fn binary_function_with_explicit_param_type() {
    let input = "fun (a: A, b: B) -> { }";
    let expected_idents = ["a", "b"];
    let expected_type_idents = ["A", "B"];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents, None);
}

#[test]
fn binary_function_with_return_type() {
    let input = "fun (a: A, b: B) -> C { }";
    let expected_idents = ["a", "b"];
    let expected_type_idents = ["A", "B"];
    let expected_return_type = Some("C");

    let parsed = parse_expr(input);

    check_function(
        parsed,
        &expected_idents,
        &expected_type_idents,
        expected_return_type,
    );
}

#[test]
fn unary_function_with_body() {
    let input = "fun (a: Int) -> Int { a }";
    let expected_idents = ["a"];
    let expected_type_idents = ["Int"];
    let expected_return_type = Some("Int");

    let parsed = parse_expr(input);

    let function = check_function(
        parsed,
        &expected_idents,
        &expected_type_idents,
        expected_return_type,
    );
    let body = function.body().and_then(|body| body.expr());
    let body = assert_some!(body);
    let body = assert_matches!(body, Expr::Block);
    let body_expr = body.exprs().take(1).next().unwrap();
    assert_matches!(body_expr, Expr::Path);
}
