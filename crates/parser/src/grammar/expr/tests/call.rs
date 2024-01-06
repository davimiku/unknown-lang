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
