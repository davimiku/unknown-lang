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

#[test]
#[ignore = "FIXME - `blue` is being parsed as a CallExpr, as if it was `blue Color.green`"]
fn parse_block_with_type_definition() {
    let input = "{
        type Color = red | green | blue

        Color.green
}";

    check_expr(input, expect![[r#""#]]);
}

#[test]
fn parse_block_with_type_definition_parens() {
    let input = "{
        type Color = (red | green | blue)

        Color.green
}";

    check_expr(
        input,
        expect![[r#"
        BlockExpr@0..66
          LBrace@0..1 "{"
          Newline@1..2 "\n"
          Emptyspace@2..10 "        "
          TypeBinding@10..43
            TypeKw@10..14 "type"
            Emptyspace@14..15 " "
            Ident@15..21
              Ident@15..20 "Color"
              Emptyspace@20..21 " "
            Equals@21..22 "="
            Emptyspace@22..23 " "
            TypeExpr@23..43
              ParenExpr@23..43
                LParen@23..24 "("
                InfixExpr@24..42
                  Ident@24..28
                    Ident@24..27 "red"
                    Emptyspace@27..28 " "
                  Bar@28..29 "|"
                  Emptyspace@29..30 " "
                  InfixExpr@30..42
                    Ident@30..36
                      Ident@30..35 "green"
                      Emptyspace@35..36 " "
                    Bar@36..37 "|"
                    Emptyspace@37..38 " "
                    Ident@38..42
                      Ident@38..42 "blue"
                RParen@42..43 ")"
          Newline@43..44 "\n"
          Newline@44..45 "\n"
          Emptyspace@45..53 "        "
          PathExpr@53..64
            Ident@53..58
              Ident@53..58 "Color"
            Dot@58..59 "."
            PathExpr@59..64
              Ident@59..64
                Ident@59..64 "green"
          Newline@64..65 "\n"
          RBrace@65..66 "}""#]],
    );
}
