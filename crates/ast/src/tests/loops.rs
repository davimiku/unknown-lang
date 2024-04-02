use util_macros::{assert_matches, assert_some};

use crate::tests::parse_expr;
use crate::Expr;

#[test]
fn loop_empty_body() {
    let input = "loop { }";

    let parsed = parse_expr(input);

    let loop_expr = assert_matches!(parsed, Expr::Loop);
    let block = assert_some!(loop_expr.block());
    assert!(block.exprs().count() == 0);
}

#[test]
fn loop_immediate_break() {
    let input = "loop { break }";

    let parsed = parse_expr(input);

    let loop_expr = assert_matches!(parsed, Expr::Loop);
    let block = assert_some!(loop_expr.block());

    assert!(block.exprs().count() == 1);
    let break_expr = block.exprs().next().unwrap();
    let break_expr = assert_matches!(break_expr, Expr::Break);
    assert!(break_expr.value().is_none());
}
