use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_tuple_int() {
    check_expr(
        "(1, 2, 3)",
        expect![[r#"
        ParenExpr@0..9
          LParen@0..1 "("
          ParenExprItem@1..2
            IntLiteralExpr@1..2
              IntLiteral@1..2 "1"
          Comma@2..3 ","
          Emptyspace@3..4 " "
          ParenExprItem@4..5
            IntLiteralExpr@4..5
              IntLiteral@4..5 "2"
          Comma@5..6 ","
          Emptyspace@6..7 " "
          IntLiteralExpr@7..8
            IntLiteral@7..8 "3"
          RParen@8..9 ")""#]],
    )
}

#[test]
fn parse_array_int() {
    check_expr(
        "[1, 2, 3]",
        expect![[r#"
ListLiteral@0..9
  LBracket@0..1 "["
  IntLiteralExpr@1..2
    IntLiteral@1..2 "1"
  Comma@2..3 ","
  Emptyspace@3..4 " "
  IntLiteralExpr@4..5
    IntLiteral@4..5 "2"
  Comma@5..6 ","
  Emptyspace@6..7 " "
  IntLiteralExpr@7..8
    IntLiteral@7..8 "3"
  RBracket@8..9 "]""#]],
    );
}
