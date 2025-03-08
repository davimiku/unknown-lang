use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_single_ident() {
    check_expr(
        "a",
        expect![[r#"
PathExpr@0..1
  Ident@0..1
    Ident@0..1 "a""#]],
    )
}

#[test]
fn parse_one_path() {
    check_expr(
        "a.b",
        expect![[r#"
PathExpr@0..3
  Ident@0..1
    Ident@0..1 "a"
  Dot@1..2 "."
  PathExpr@2..3
    Ident@2..3
      Ident@2..3 "b""#]],
    )
}

#[test]
fn parse_two_nested_path() {
    check_expr(
        "a.b.c",
        expect![[r#"
PathExpr@0..5
  PathExpr@0..3
    Ident@0..1
      Ident@0..1 "a"
    Dot@1..2 "."
    Ident@2..3
      Ident@2..3 "b"
  Dot@3..4 "."
  PathExpr@4..5
    Ident@4..5
      Ident@4..5 "c""#]],
    )
}

#[test]
fn parse_path_higher_precedence_than_arithmetic() {
    check_expr(
        "a.b * c",
        expect![[r#"
InfixExpr@0..7
  PathExpr@0..4
    Ident@0..1
      Ident@0..1 "a"
    Dot@1..2 "."
    PathExpr@2..4
      Ident@2..4
        Ident@2..3 "b"
        Emptyspace@3..4 " "
  Star@4..5 "*"
  Emptyspace@5..6 " "
  PathExpr@6..7
    Ident@6..7
      Ident@6..7 "c""#]],
    )
}
