use expect_test::expect;

use crate::check_expr;

#[test]
fn parse_negation() {
    check_expr(
        "-1",
        expect![[r#"
NegationExpr@0..2
  Dash@0..1 "-"
  IntLiteralExpr@1..2
    IntLiteral@1..2 "1""#]],
    );
}

#[test]
fn parse_not_false() {
    check_expr(
        "!false",
        expect![[r#"
NotExpr@0..6
  Bang@0..1 "!"
  BoolLiteralExpr@1..6
    FalseLiteral@1..6 "false""#]],
    );
}

#[test]
fn parse_not_true() {
    check_expr(
        "!true",
        expect![[r#"
NotExpr@0..5
  Bang@0..1 "!"
  BoolLiteralExpr@1..5
    TrueLiteral@1..5 "true""#]],
    );
}

#[test]
fn parse_not_variable_ref() {
    check_expr(
        "!a",
        expect![[r#"
NotExpr@0..2
  Bang@0..1 "!"
  PathExpr@1..2
    Ident@1..2
      Ident@1..2 "a""#]],
    );
}

#[test]
fn parse_tostring_int() {
    check_expr(
        "~1",
        expect![[r#"
IntoStringExpr@0..2
  Tilde@0..1 "~"
  IntLiteralExpr@1..2
    IntLiteral@1..2 "1""#]],
    );
}
