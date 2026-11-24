// function calls

use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_function_call_unit_arg() {
    check_expr(
        "func ()",
        expect![[r#"
Call@0..7
  PathExpr@0..5
    Ident@0..5
      Ident@0..4 "func"
      Emptyspace@4..5 " "
  CallArgs@5..7
    LParen@5..6 "("
    RParen@6..7 ")""#]],
    )
}

#[test]
fn parse_function_call_one_int() {
    check_expr(
        "print 1",
        expect![[r#"
Call@0..7
  PathExpr@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..7
    IntLiteralExpr@6..7
      IntLiteral@6..7 "1""#]],
    )
}

#[test]
fn parse_function_call_one_path() {
    check_expr(
        "print a",
        expect![[r#"
Call@0..7
  PathExpr@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..7
    PathExpr@6..7
      Ident@6..7
        Ident@6..7 "a""#]],
    )
}

#[test]
fn parse_function_call_nested_path() {
    check_expr(
        "print a.b.c",
        expect![[r#"
Call@0..11
  PathExpr@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..11
    PathExpr@6..11
      PathExpr@6..9
        Ident@6..7
          Ident@6..7 "a"
        Dot@7..8 "."
        Ident@8..9
          Ident@8..9 "b"
      Dot@9..10 "."
      PathExpr@10..11
        Ident@10..11
          Ident@10..11 "c""#]],
    )
}

#[test]
fn parse_function_call_two_args() {
    check_expr(
        "add (1, 2)",
        expect![[r#"
Call@0..10
  PathExpr@0..4
    Ident@0..4
      Ident@0..3 "add"
      Emptyspace@3..4 " "
  CallArgs@4..10
    LParen@4..5 "("
    IntLiteralExpr@5..6
      IntLiteral@5..6 "1"
    Comma@6..7 ","
    Emptyspace@7..8 " "
    IntLiteralExpr@8..9
      IntLiteral@8..9 "2"
    RParen@9..10 ")""#]],
    )
}

#[test]
fn parse_ufcs_call() {
    check_expr(
        "point.distance_from other",
        expect![[r#"
Call@0..25
  PathExpr@0..20
    Ident@0..5
      Ident@0..5 "point"
    Dot@5..6 "."
    PathExpr@6..20
      Ident@6..20
        Ident@6..19 "distance_from"
        Emptyspace@19..20 " "
  CallArgs@20..25
    PathExpr@20..25
      Ident@20..25
        Ident@20..25 "other""#]],
    );
}

#[test]
fn parse_ufcs_operator_call() {
    check_expr(
        "point.+ other",
        expect![[r#"
Call@0..13
  PathExpr@0..8
    Ident@0..5
      Ident@0..5 "point"
    Dot@5..6 "."
    Ident@6..8
      Plus@6..7 "+"
      Emptyspace@7..8 " "
  CallArgs@8..13
    PathExpr@8..13
      Ident@8..13
        Ident@8..13 "other""#]],
    );
}

#[test]
fn parse_escaped_operator_binding() {
    check_expr(
        "let `+` = 1",
        expect![[r#"
LetBinding@0..11
  LetKw@0..3 "let"
  Emptyspace@3..4 " "
  Ident@4..8
    Ident@4..7 "`+`"
    Emptyspace@7..8 " "
  Equals@8..9 "="
  Emptyspace@9..10 " "
  IntLiteralExpr@10..11
    IntLiteral@10..11 "1""#]],
    );
}
