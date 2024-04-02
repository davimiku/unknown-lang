use util_macros::{assert_matches, assert_some};

use crate::tests::parse_expr;
use crate::Expr;

#[test]
fn if_expr_empty() {
    let input = "if a {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    assert!(if_expr.else_branch().is_none());
}

#[test]
fn if_else_expr() {
    let input = "if a {} else {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    let else_branch = assert_some!(if_expr.else_branch());
    assert_matches!(else_branch, Expr::Block);
}

#[test]
fn if_else_if_expr() {
    let input = "if a {} else if b {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    let else_branch = assert_some!(if_expr.else_branch());
    assert_matches!(else_branch, Expr::If);
}
