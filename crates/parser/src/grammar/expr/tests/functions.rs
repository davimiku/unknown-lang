use crate::check_expr;
use expect_test::expect;

#[test]
fn parse_nullary_function() {
    check_expr(
        "fun () -> { }",
        expect![[r#"
FunExpr@0..13
  FunKw@0..3 "fun"
  Emptyspace@3..4 " "
  FunParamList@4..7
    LParen@4..5 "("
    RParen@5..6 ")"
    Emptyspace@6..7 " "
  Arrow@7..9 "->"
  Emptyspace@9..10 " "
  FunBody@10..13
    BlockExpr@10..13
      LBrace@10..11 "{"
      Emptyspace@11..12 " "
      RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_explicit_param_type() {
    check_expr(
        "fun (a: A) -> { }",
        expect![[r#"
            FunExpr@0..17
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..11
                LParen@4..5 "("
                FunParam@5..9
                  Ident@5..6 "a"
                  Colon@6..7 ":"
                  Emptyspace@7..8 " "
                  TypeExpr@8..9
                    Ident@8..9
                      Ident@8..9 "A"
                RParen@9..10 ")"
                Emptyspace@10..11 " "
              Arrow@11..13 "->"
              Emptyspace@13..14 " "
              FunBody@14..17
                BlockExpr@14..17
                  LBrace@14..15 "{"
                  Emptyspace@15..16 " "
                  RBrace@16..17 "}""#]],
    )
}

#[test]
fn parse_unary_function_without_param_type() {
    check_expr(
        "fun id -> { }",
        expect![[r#"
            FunExpr@0..13
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..7
                FunParam@4..7
                  Ident@4..6 "id"
                  Emptyspace@6..7 " "
              Arrow@7..9 "->"
              Emptyspace@9..10 " "
              FunBody@10..13
                BlockExpr@10..13
                  LBrace@10..11 "{"
                  Emptyspace@11..12 " "
                  RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_paren() {
    check_expr(
        "fun (id) -> {}",
        expect![[r#"
            FunExpr@0..14
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..9
                LParen@4..5 "("
                FunParam@5..7
                  Ident@5..7 "id"
                RParen@7..8 ")"
                Emptyspace@8..9 " "
              Arrow@9..11 "->"
              Emptyspace@11..12 " "
              FunBody@12..14
                BlockExpr@12..14
                  LBrace@12..13 "{"
                  RBrace@13..14 "}""#]],
    );
}

#[test]
fn parse_binary_function_without_param_types() {
    check_expr(
        "fun (a, b) -> { }",
        expect![[r#"
            FunExpr@0..17
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..11
                LParen@4..5 "("
                FunParam@5..6
                  Ident@5..6 "a"
                Comma@6..7 ","
                Emptyspace@7..8 " "
                FunParam@8..9
                  Ident@8..9 "b"
                RParen@9..10 ")"
                Emptyspace@10..11 " "
              Arrow@11..13 "->"
              Emptyspace@13..14 " "
              FunBody@14..17
                BlockExpr@14..17
                  LBrace@14..15 "{"
                  Emptyspace@15..16 " "
                  RBrace@16..17 "}""#]],
    )
}

#[test]
fn parse_binary_function_with_param_types() {
    check_expr(
        "fun (a: A, b: B) -> { }",
        expect![[r#"
            FunExpr@0..23
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..17
                LParen@4..5 "("
                FunParam@5..9
                  Ident@5..6 "a"
                  Colon@6..7 ":"
                  Emptyspace@7..8 " "
                  TypeExpr@8..9
                    Ident@8..9
                      Ident@8..9 "A"
                Comma@9..10 ","
                Emptyspace@10..11 " "
                FunParam@11..15
                  Ident@11..12 "b"
                  Colon@12..13 ":"
                  Emptyspace@13..14 " "
                  TypeExpr@14..15
                    Ident@14..15
                      Ident@14..15 "B"
                RParen@15..16 ")"
                Emptyspace@16..17 " "
              Arrow@17..19 "->"
              Emptyspace@19..20 " "
              FunBody@20..23
                BlockExpr@20..23
                  LBrace@20..21 "{"
                  Emptyspace@21..22 " "
                  RBrace@22..23 "}""#]],
    )
}

#[test]
fn parse_binary_function_with_explicit_param_and_return_types() {
    check_expr(
        "fun (a: A, b: B) -> C { }",
        expect![[r#"
            FunExpr@0..25
              FunKw@0..3 "fun"
              Emptyspace@3..4 " "
              FunParamList@4..17
                LParen@4..5 "("
                FunParam@5..9
                  Ident@5..6 "a"
                  Colon@6..7 ":"
                  Emptyspace@7..8 " "
                  TypeExpr@8..9
                    Ident@8..9
                      Ident@8..9 "A"
                Comma@9..10 ","
                Emptyspace@10..11 " "
                FunParam@11..15
                  Ident@11..12 "b"
                  Colon@12..13 ":"
                  Emptyspace@13..14 " "
                  TypeExpr@14..15
                    Ident@14..15
                      Ident@14..15 "B"
                RParen@15..16 ")"
                Emptyspace@16..17 " "
              Arrow@17..19 "->"
              Emptyspace@19..20 " "
              TypeExpr@20..22
                Ident@20..22
                  Ident@20..21 "C"
                  Emptyspace@21..22 " "
              FunBody@22..25
                BlockExpr@22..25
                  LBrace@22..23 "{"
                  Emptyspace@23..24 " "
                  RBrace@24..25 "}""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression() {
    check_expr(
        "(fun () -> {}) ()",
        expect![[r#"
            Call@0..17
              ParenExpr@0..15
                LParen@0..1 "("
                FunExpr@1..13
                  FunKw@1..4 "fun"
                  Emptyspace@4..5 " "
                  FunParamList@5..8
                    LParen@5..6 "("
                    RParen@6..7 ")"
                    Emptyspace@7..8 " "
                  Arrow@8..10 "->"
                  Emptyspace@10..11 " "
                  FunBody@11..13
                    BlockExpr@11..13
                      LBrace@11..12 "{"
                      RBrace@12..13 "}"
                RParen@13..14 ")"
                Emptyspace@14..15 " "
              CallArgs@15..17
                LParen@15..16 "("
                RParen@16..17 ")""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression_one_arg() {
    check_expr(
        "(fun (a: Int) -> { a * a }) 4",
        expect![[r#"
            Call@0..29
              ParenExpr@0..28
                LParen@0..1 "("
                FunExpr@1..26
                  FunKw@1..4 "fun"
                  Emptyspace@4..5 " "
                  FunParamList@5..14
                    LParen@5..6 "("
                    FunParam@6..12
                      Ident@6..7 "a"
                      Colon@7..8 ":"
                      Emptyspace@8..9 " "
                      TypeExpr@9..12
                        Ident@9..12
                          Ident@9..12 "Int"
                    RParen@12..13 ")"
                    Emptyspace@13..14 " "
                  Arrow@14..16 "->"
                  Emptyspace@16..17 " "
                  FunBody@17..26
                    BlockExpr@17..26
                      LBrace@17..18 "{"
                      Emptyspace@18..19 " "
                      InfixExpr@19..25
                        PathExpr@19..21
                          Ident@19..21
                            Ident@19..20 "a"
                            Emptyspace@20..21 " "
                        Star@21..22 "*"
                        Emptyspace@22..23 " "
                        PathExpr@23..25
                          Ident@23..25
                            Ident@23..24 "a"
                            Emptyspace@24..25 " "
                      RBrace@25..26 "}"
                RParen@26..27 ")"
                Emptyspace@27..28 " "
              CallArgs@28..29
                IntLiteralExpr@28..29
                  IntLiteral@28..29 "4""#]],
    )
}

#[test]
fn parse_immediately_invoked_function_expression_two_args() {
    check_expr(
        "(fun (a: Int, b: Int) -> { a - b }) (5, 3)",
        expect![[r#"
            Call@0..42
              ParenExpr@0..36
                LParen@0..1 "("
                FunExpr@1..34
                  FunKw@1..4 "fun"
                  Emptyspace@4..5 " "
                  FunParamList@5..22
                    LParen@5..6 "("
                    FunParam@6..12
                      Ident@6..7 "a"
                      Colon@7..8 ":"
                      Emptyspace@8..9 " "
                      TypeExpr@9..12
                        Ident@9..12
                          Ident@9..12 "Int"
                    Comma@12..13 ","
                    Emptyspace@13..14 " "
                    FunParam@14..20
                      Ident@14..15 "b"
                      Colon@15..16 ":"
                      Emptyspace@16..17 " "
                      TypeExpr@17..20
                        Ident@17..20
                          Ident@17..20 "Int"
                    RParen@20..21 ")"
                    Emptyspace@21..22 " "
                  Arrow@22..24 "->"
                  Emptyspace@24..25 " "
                  FunBody@25..34
                    BlockExpr@25..34
                      LBrace@25..26 "{"
                      Emptyspace@26..27 " "
                      InfixExpr@27..33
                        PathExpr@27..29
                          Ident@27..29
                            Ident@27..28 "a"
                            Emptyspace@28..29 " "
                        Dash@29..30 "-"
                        Emptyspace@30..31 " "
                        PathExpr@31..33
                          Ident@31..33
                            Ident@31..32 "b"
                            Emptyspace@32..33 " "
                      RBrace@33..34 "}"
                RParen@34..35 ")"
                Emptyspace@35..36 " "
              CallArgs@36..42
                LParen@36..37 "("
                IntLiteralExpr@37..38
                  IntLiteral@37..38 "5"
                Comma@38..39 ","
                Emptyspace@39..40 " "
                IntLiteralExpr@40..41
                  IntLiteral@40..41 "3"
                RParen@41..42 ")""#]],
    )
}
