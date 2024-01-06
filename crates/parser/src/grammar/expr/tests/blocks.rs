use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_block_with_one_expr() {
    check_expr(
        "{1}",
        expect![[r#"
BlockExpr@0..3
  LBrace@0..1 "{"
  IntLiteralExpr@1..2
    IntLiteral@1..2 "1"
  RBrace@2..3 "}""#]],
    )
}

#[test]
fn parse_block_newline() {
    check_expr(
        r#"{
  1
}"#,
        expect![[r#"
BlockExpr@0..7
  LBrace@0..1 "{"
  Newline@1..2 "\n"
  Emptyspace@2..4 "  "
  IntLiteralExpr@4..5
    IntLiteral@4..5 "1"
  Newline@5..6 "\n"
  RBrace@6..7 "}""#]],
    )
}

#[test]
fn parse_block_expressions_multiple_lines() {
    check_expr(
        r#"{
  let x = 1
  let y = 2
  x + y
}"#,
        expect![[r#"
BlockExpr@0..35
  LBrace@0..1 "{"
  Newline@1..2 "\n"
  Emptyspace@2..4 "  "
  LetBinding@4..13
    LetKw@4..7 "let"
    Emptyspace@7..8 " "
    Ident@8..10
      Ident@8..9 "x"
      Emptyspace@9..10 " "
    Equals@10..11 "="
    Emptyspace@11..12 " "
    IntLiteralExpr@12..13
      IntLiteral@12..13 "1"
  Newline@13..14 "\n"
  Emptyspace@14..16 "  "
  LetBinding@16..25
    LetKw@16..19 "let"
    Emptyspace@19..20 " "
    Ident@20..22
      Ident@20..21 "y"
      Emptyspace@21..22 " "
    Equals@22..23 "="
    Emptyspace@23..24 " "
    IntLiteralExpr@24..25
      IntLiteral@24..25 "2"
  Newline@25..26 "\n"
  Emptyspace@26..28 "  "
  InfixExpr@28..33
    PathExpr@28..30
      Ident@28..30
        Ident@28..29 "x"
        Emptyspace@29..30 " "
    Plus@30..31 "+"
    Emptyspace@31..32 " "
    PathExpr@32..33
      Ident@32..33
        Ident@32..33 "y"
  Newline@33..34 "\n"
  RBrace@34..35 "}""#]],
    )
}
