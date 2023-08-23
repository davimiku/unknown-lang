use crate::check_expr;
use expect_test::expect;

#[test]
fn parse_nullary_function() {
    check_expr(
        "() -> { }",
        expect![[r#"
FunExpr@0..9
  ParenExpr@0..3
    LParen@0..1 "("
    RParen@1..2 ")"
    Emptyspace@2..3 " "
  Arrow@3..5 "->"
  Emptyspace@5..6 " "
  BlockExpr@6..9
    LBrace@6..7 "{"
    Emptyspace@7..8 " "
    RBrace@8..9 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_explicit_param_type() {
    check_expr(
        "(a: A) -> { }",
        expect![[r#"
            FunExpr@0..13
              ParenExpr@0..7
                LParen@0..1 "("
                ParenExprItem@1..5
                  PathExpr@1..2
                    Ident@1..2
                      Ident@1..2 "a"
                  Colon@2..3 ":"
                  Emptyspace@3..4 " "
                  TypeExpr@4..5
                    Ident@4..5
                      Ident@4..5 "A"
                RParen@5..6 ")"
                Emptyspace@6..7 " "
              Arrow@7..9 "->"
              Emptyspace@9..10 " "
              BlockExpr@10..13
                LBrace@10..11 "{"
                Emptyspace@11..12 " "
                RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_inferred_param_type() {
    check_expr(
        "id -> { }",
        expect![[r#"
            FunExpr@0..9
              PathExpr@0..3
                Ident@0..3
                  Ident@0..2 "id"
                  Emptyspace@2..3 " "
              Arrow@3..5 "->"
              Emptyspace@5..6 " "
              BlockExpr@6..9
                LBrace@6..7 "{"
                Emptyspace@7..8 " "
                RBrace@8..9 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_paren() {
    check_expr(
        "(id) -> {}",
        expect![[r#"
            FunExpr@0..10
              ParenExpr@0..5
                LParen@0..1 "("
                PathExpr@1..3
                  Ident@1..3
                    Ident@1..3 "id"
                RParen@3..4 ")"
                Emptyspace@4..5 " "
              Arrow@5..7 "->"
              Emptyspace@7..8 " "
              BlockExpr@8..10
                LBrace@8..9 "{"
                RBrace@9..10 "}""#]],
    );
}

#[test]
fn parse_binary_function_with_inferred_param_types() {
    check_expr(
        "(a, b) -> { }",
        expect![[r#"
            FunExpr@0..13
              ParenExpr@0..7
                LParen@0..1 "("
                ParenExprItem@1..2
                  PathExpr@1..2
                    Ident@1..2
                      Ident@1..2 "a"
                Comma@2..3 ","
                Emptyspace@3..4 " "
                ParenExprItem@4..5
                  PathExpr@4..5
                    Ident@4..5
                      Ident@4..5 "b"
                RParen@5..6 ")"
                Emptyspace@6..7 " "
              Arrow@7..9 "->"
              Emptyspace@9..10 " "
              BlockExpr@10..13
                LBrace@10..11 "{"
                Emptyspace@11..12 " "
                RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_binary_function_with_explicit_param_types() {
    check_expr(
        "(a: A, b: B) -> { }",
        expect![[r#"
            FunExpr@0..19
              ParenExpr@0..13
                LParen@0..1 "("
                ParenExprItem@1..5
                  PathExpr@1..2
                    Ident@1..2
                      Ident@1..2 "a"
                  Colon@2..3 ":"
                  Emptyspace@3..4 " "
                  TypeExpr@4..5
                    Ident@4..5
                      Ident@4..5 "A"
                Comma@5..6 ","
                Emptyspace@6..7 " "
                ParenExprItem@7..11
                  PathExpr@7..8
                    Ident@7..8
                      Ident@7..8 "b"
                  Colon@8..9 ":"
                  Emptyspace@9..10 " "
                  TypeExpr@10..11
                    Ident@10..11
                      Ident@10..11 "B"
                RParen@11..12 ")"
                Emptyspace@12..13 " "
              Arrow@13..15 "->"
              Emptyspace@15..16 " "
              BlockExpr@16..19
                LBrace@16..17 "{"
                Emptyspace@17..18 " "
                RBrace@18..19 "}""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression() {
    check_expr(
        "(() -> {}) ()",
        expect![[r#"
Call@0..13
  ParenExpr@0..11
    LParen@0..1 "("
    FunExpr@1..9
      ParenExpr@1..4
        LParen@1..2 "("
        RParen@2..3 ")"
        Emptyspace@3..4 " "
      Arrow@4..6 "->"
      Emptyspace@6..7 " "
      BlockExpr@7..9
        LBrace@7..8 "{"
        RBrace@8..9 "}"
    RParen@9..10 ")"
    Emptyspace@10..11 " "
  CallArgs@11..13
    LParen@11..12 "("
    RParen@12..13 ")""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression_one_arg() {
    check_expr(
        "((a: Int) -> a * a) 4",
        expect![[r#"
            Call@0..21
              ParenExpr@0..20
                LParen@0..1 "("
                FunExpr@1..18
                  ParenExpr@1..10
                    LParen@1..2 "("
                    ParenExprItem@2..8
                      PathExpr@2..3
                        Ident@2..3
                          Ident@2..3 "a"
                      Colon@3..4 ":"
                      Emptyspace@4..5 " "
                      TypeExpr@5..8
                        Ident@5..8
                          Ident@5..8 "Int"
                    RParen@8..9 ")"
                    Emptyspace@9..10 " "
                  Arrow@10..12 "->"
                  Emptyspace@12..13 " "
                  InfixExpr@13..18
                    PathExpr@13..15
                      Ident@13..15
                        Ident@13..14 "a"
                        Emptyspace@14..15 " "
                    Star@15..16 "*"
                    Emptyspace@16..17 " "
                    PathExpr@17..18
                      Ident@17..18
                        Ident@17..18 "a"
                RParen@18..19 ")"
                Emptyspace@19..20 " "
              CallArgs@20..21
                IntLiteralExpr@20..21
                  IntLiteral@20..21 "4""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression_two_args() {
    check_expr(
        "((a: Int, b: Int) -> a - b) (5, 3)",
        expect![[r#"
            Call@0..34
              ParenExpr@0..28
                LParen@0..1 "("
                FunExpr@1..26
                  ParenExpr@1..18
                    LParen@1..2 "("
                    ParenExprItem@2..8
                      PathExpr@2..3
                        Ident@2..3
                          Ident@2..3 "a"
                      Colon@3..4 ":"
                      Emptyspace@4..5 " "
                      TypeExpr@5..8
                        Ident@5..8
                          Ident@5..8 "Int"
                    Comma@8..9 ","
                    Emptyspace@9..10 " "
                    ParenExprItem@10..16
                      PathExpr@10..11
                        Ident@10..11
                          Ident@10..11 "b"
                      Colon@11..12 ":"
                      Emptyspace@12..13 " "
                      TypeExpr@13..16
                        Ident@13..16
                          Ident@13..16 "Int"
                    RParen@16..17 ")"
                    Emptyspace@17..18 " "
                  Arrow@18..20 "->"
                  Emptyspace@20..21 " "
                  InfixExpr@21..26
                    PathExpr@21..23
                      Ident@21..23
                        Ident@21..22 "a"
                        Emptyspace@22..23 " "
                    Dash@23..24 "-"
                    Emptyspace@24..25 " "
                    PathExpr@25..26
                      Ident@25..26
                        Ident@25..26 "b"
                RParen@26..27 ")"
                Emptyspace@27..28 " "
              CallArgs@28..34
                LParen@28..29 "("
                IntLiteralExpr@29..30
                  IntLiteral@29..30 "5"
                Comma@30..31 ","
                Emptyspace@31..32 " "
                IntLiteralExpr@32..33
                  IntLiteral@32..33 "3"
                RParen@33..34 ")""#]],
    )
}
