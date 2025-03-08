use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_empty_match() {
    let input = "match a {}";
    check_expr(
        input,
        expect![[r#"
MatchExpr@0..10
  MatchKw@0..5 "match"
  Emptyspace@5..6 " "
  ScrutineeExpr@6..8
    PathExpr@6..8
      Ident@6..8
        Ident@6..7 "a"
        Emptyspace@7..8 " "
  MatchBlock@8..10
    LBrace@8..9 "{"
    RBrace@9..10 "}""#]],
    )
}

#[test]
fn parse_match_one_arm() {
    let input = "match u {
    .a -> 4
}";
    check_expr(
        input,
        expect![[r#"
            MatchExpr@0..23
              MatchKw@0..5 "match"
              Emptyspace@5..6 " "
              ScrutineeExpr@6..8
                PathExpr@6..8
                  Ident@6..8
                    Ident@6..7 "u"
                    Emptyspace@7..8 " "
              MatchBlock@8..23
                LBrace@8..9 "{"
                Newline@9..10 "\n"
                Emptyspace@10..14 "    "
                MatchArm@14..22
                  DotPattern@14..17
                    Dot@14..15 "."
                    Ident@15..17
                      Ident@15..16 "a"
                      Emptyspace@16..17 " "
                  Arrow@17..19 "->"
                  Emptyspace@19..20 " "
                  IntLiteralExpr@20..21
                    IntLiteral@20..21 "4"
                  Newline@21..22 "\n"
                RBrace@22..23 "}""#]],
    )
}

#[test]
fn parse_match_one_with_payload() {
    let input = "match u {
    .a i -> (i + 2),
}";
    check_expr(
        input,
        expect![[r#"
            MatchExpr@0..32
              MatchKw@0..5 "match"
              Emptyspace@5..6 " "
              ScrutineeExpr@6..8
                PathExpr@6..8
                  Ident@6..8
                    Ident@6..7 "u"
                    Emptyspace@7..8 " "
              MatchBlock@8..32
                LBrace@8..9 "{"
                Newline@9..10 "\n"
                Emptyspace@10..14 "    "
                MatchArm@14..30
                  DotPattern@14..19
                    Dot@14..15 "."
                    Ident@15..17
                      Ident@15..16 "a"
                      Emptyspace@16..17 " "
                    IdentPattern@17..19
                      Ident@17..19
                        Ident@17..18 "i"
                        Emptyspace@18..19 " "
                  Arrow@19..21 "->"
                  Emptyspace@21..22 " "
                  ParenExpr@22..29
                    LParen@22..23 "("
                    InfixExpr@23..28
                      PathExpr@23..25
                        Ident@23..25
                          Ident@23..24 "i"
                          Emptyspace@24..25 " "
                      Plus@25..26 "+"
                      Emptyspace@26..27 " "
                      IntLiteralExpr@27..28
                        IntLiteral@27..28 "2"
                    RParen@28..29 ")"
                  Comma@29..30 ","
                Newline@30..31 "\n"
                RBrace@31..32 "}""#]],
    )
}

#[test]
fn parse_match_two_arms() {
    let input = "match u {
    .a -> 4
    .b -> 8
}";
    check_expr(
        input,
        expect![[r#"
            MatchExpr@0..35
              MatchKw@0..5 "match"
              Emptyspace@5..6 " "
              ScrutineeExpr@6..8
                PathExpr@6..8
                  Ident@6..8
                    Ident@6..7 "u"
                    Emptyspace@7..8 " "
              MatchBlock@8..35
                LBrace@8..9 "{"
                Newline@9..10 "\n"
                Emptyspace@10..14 "    "
                MatchArm@14..26
                  DotPattern@14..17
                    Dot@14..15 "."
                    Ident@15..17
                      Ident@15..16 "a"
                      Emptyspace@16..17 " "
                  Arrow@17..19 "->"
                  Emptyspace@19..20 " "
                  IntLiteralExpr@20..21
                    IntLiteral@20..21 "4"
                  Newline@21..22 "\n"
                  Emptyspace@22..26 "    "
                MatchArm@26..34
                  DotPattern@26..29
                    Dot@26..27 "."
                    Ident@27..29
                      Ident@27..28 "b"
                      Emptyspace@28..29 " "
                  Arrow@29..31 "->"
                  Emptyspace@31..32 " "
                  IntLiteralExpr@32..33
                    IntLiteral@32..33 "8"
                  Newline@33..34 "\n"
                RBrace@34..35 "}""#]],
    )
}
