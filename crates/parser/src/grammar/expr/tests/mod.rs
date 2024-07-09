mod array;
mod binary;
mod bindings;
mod blocks;
mod call;
mod compound_literals;
mod compound_types;
mod functions;
mod if_else;
mod loops;
mod r#match;
mod path;
mod scalar_literals;
mod types;
mod unary;

use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_nested_parentheses() {
    check_expr(
        "((((((1))))))",
        expect![[r#"
ParenExpr@0..13
  LParen@0..1 "("
  ParenExpr@1..12
    LParen@1..2 "("
    ParenExpr@2..11
      LParen@2..3 "("
      ParenExpr@3..10
        LParen@3..4 "("
        ParenExpr@4..9
          LParen@4..5 "("
          ParenExpr@5..8
            LParen@5..6 "("
            IntLiteralExpr@6..7
              IntLiteral@6..7 "1"
            RParen@7..8 ")"
          RParen@8..9 ")"
        RParen@9..10 ")"
      RParen@10..11 ")"
    RParen@11..12 ")"
  RParen@12..13 ")""#]],
    );
}

#[test]
fn parentheses_affect_precedence() {
    check_expr(
        "3*(2+1)",
        expect![[r#"
InfixExpr@0..7
  IntLiteralExpr@0..1
    IntLiteral@0..1 "3"
  Star@1..2 "*"
  ParenExpr@2..7
    LParen@2..3 "("
    InfixExpr@3..6
      IntLiteralExpr@3..4
        IntLiteral@3..4 "2"
      Plus@4..5 "+"
      IntLiteralExpr@5..6
        IntLiteral@5..6 "1"
    RParen@6..7 ")""#]],
    );
}

#[test]
#[ignore = "robust error reporting is yet to be implemented"]
fn parse_unclosed_parentheses() {
    check_expr(
        "(hello",
        expect![[r#"
ParenExpr@0..6
  LParen@0..1 "("
  PathExpr@1..6
    Ident@1..6
      Ident@1..6 "hello"
error at 1..6: expected ‘:’, ‘,’ or ‘)’"#]],
    );
}
