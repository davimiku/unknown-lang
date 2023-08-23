use expect_test::expect;

use crate::check_expr;

#[test]
fn parse_array_literal() {
    check_expr(
        "[0, 1]",
        expect![[r#"
ArrayLiteral@0..6
  LBracket@0..1 "["
  IntLiteralExpr@1..2
    IntLiteral@1..2 "0"
  Comma@2..3 ","
  Emptyspace@3..4 " "
  IntLiteralExpr@4..5
    IntLiteral@4..5 "1"
  RBracket@5..6 "]""#]],
    );
}

#[test]
fn parse_array_index() {
    check_expr(
        "[0, 1].0",
        expect![[r#"
PathExpr@0..8
  ArrayLiteral@0..6
    LBracket@0..1 "["
    IntLiteralExpr@1..2
      IntLiteral@1..2 "0"
    Comma@2..3 ","
    Emptyspace@3..4 " "
    IntLiteralExpr@4..5
      IntLiteral@4..5 "1"
    RBracket@5..6 "]"
  Dot@6..7 "."
  IntLiteralExpr@7..8
    IntLiteral@7..8 "0""#]],
    );
}
