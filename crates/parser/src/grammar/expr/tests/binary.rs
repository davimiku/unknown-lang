use expect_test::expect;

use crate::check_expr;

#[test]
fn parse_int_equality() {
    check_expr(
        "1 == 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  EqualsEquals@2..4 "=="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_int_not_equality() {
    check_expr(
        "1 != 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  BangEquals@2..4 "!="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_string_equality() {
    check_expr(
        r#""a" == "a""#,
        expect![[r#"
InfixExpr@0..10
  StringLiteralExpr@0..4
    StringLiteralExpr@0..3 "\"a\""
    Emptyspace@3..4 " "
  EqualsEquals@4..6 "=="
  Emptyspace@6..7 " "
  StringLiteralExpr@7..10
    StringLiteralExpr@7..10 "\"a\"""#]],
    );
}

#[test]
fn parse_string_not_equality() {
    check_expr(
        r#""a" != "a""#,
        expect![[r#"
InfixExpr@0..10
  StringLiteralExpr@0..4
    StringLiteralExpr@0..3 "\"a\""
    Emptyspace@3..4 " "
  BangEquals@4..6 "!="
  Emptyspace@6..7 " "
  StringLiteralExpr@7..10
    StringLiteralExpr@7..10 "\"a\"""#]],
    );
}
