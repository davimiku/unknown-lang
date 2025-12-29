use util_macros::{assert_matches, assert_some};

use crate::{tests::parse_expr, Expr};

#[test]
fn return_statement() {
    let input = "return 1";

    let parsed = parse_expr(input);

    let return_statement = assert_matches!(parsed, Expr::Return);
    let return_value = assert_some!(return_statement.return_value());
    assert_matches!(return_value, Expr::IntLiteral);
}
