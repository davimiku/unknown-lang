use crate::Expr;
use util_macros::{assert_matches, assert_some};

mod bindings;
mod branches;
mod calls;
mod concat;
mod functions;
mod loops;
mod records;
mod statements;
mod types;

fn parse_expr(input: &str) -> Expr {
    let node = parser::test_parse_expr(input).syntax();
    Expr::cast(node).expect("parsing to have produced an Expr")
}

#[test]
fn simple_path() {
    let input = "my_func";

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);
    let ident = assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_eq!(ident.as_string(), "my_func");

    assert!(path.member().is_none());
}

#[test]
fn add_int_and_function() {
    // not a valid expression by the type checker, but should still produce a valid AST
    let input = "1 + (fun () -> { })";
    let expected_lhs = "1";
    let expected_rhs_param_list = None;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    let lhs = assert_matches!(lhs, Expr::IntLiteral);
    let actual_lhs = assert_some!(lhs.as_string());
    assert_eq!(actual_lhs, expected_lhs);

    let rhs = assert_some!(binary.rhs());
    let rhs = assert_matches!(rhs, Expr::Paren);
    let rhs = assert_some!(rhs.expr());
    let rhs = assert_matches!(rhs, Expr::Function);
    assert_eq!(rhs.param_list().params().next(), expected_rhs_param_list);
}
