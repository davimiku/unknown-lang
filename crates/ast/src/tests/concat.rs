use util_macros::{assert_matches, assert_some};

use crate::{tests::parse_expr, Expr};

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    assert_matches!(lhs, Expr::StringLiteral);
    let rhs = assert_some!(binary.rhs());
    assert_matches!(rhs, Expr::StringLiteral);

    let op = assert_some!(binary.op());
    assert_eq!(op.text(), "++");
}
