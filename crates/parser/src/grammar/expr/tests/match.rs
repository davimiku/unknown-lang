use expect_test::expect;

use crate::grammar::{check, check_expr};

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

#[test]
fn unwrap_add_and_rewrap() {
    let input = "type Number = (int: Int | float: Float)
let main = fun (n: Number) -> {
    match n {
        .int i -> Number.int (i + 16)
        .float f -> Number.float (f + 16.0)
    }
}";
    check(input, expect![[r#"
        Root@0..175
          TypeBinding@0..39
            TypeKw@0..4 "type"
            Emptyspace@4..5 " "
            Ident@5..12
              Ident@5..11 "Number"
              Emptyspace@11..12 " "
            Equals@12..13 "="
            Emptyspace@13..14 " "
            TypeExpr@14..39
              ParenExpr@14..39
                LParen@14..15 "("
                InfixExpr@15..38
                  CompoundTypeItem@15..24
                    Ident@15..18
                      Ident@15..18 "int"
                    Colon@18..19 ":"
                    Emptyspace@19..20 " "
                    CompoundTypeItemType@20..24
                      Ident@20..24
                        Ident@20..23 "Int"
                        Emptyspace@23..24 " "
                  Bar@24..25 "|"
                  Emptyspace@25..26 " "
                  CompoundTypeItem@26..38
                    Ident@26..31
                      Ident@26..31 "float"
                    Colon@31..32 ":"
                    Emptyspace@32..33 " "
                    CompoundTypeItemType@33..38
                      Ident@33..38
                        Ident@33..38 "Float"
                RParen@38..39 ")"
          Newline@39..40
            Newline@39..40 "\n"
          LetBinding@40..175
            LetKw@40..43 "let"
            Emptyspace@43..44 " "
            Ident@44..49
              Ident@44..48 "main"
              Emptyspace@48..49 " "
            Equals@49..50 "="
            Emptyspace@50..51 " "
            FunExpr@51..175
              FunKw@51..54 "fun"
              Emptyspace@54..55 " "
              FunParamList@55..67
                LParen@55..56 "("
                FunParam@56..65
                  Ident@56..57
                    Ident@56..57 "n"
                  Colon@57..58 ":"
                  Emptyspace@58..59 " "
                  TypeExpr@59..65
                    Ident@59..65
                      Ident@59..65 "Number"
                RParen@65..66 ")"
                Emptyspace@66..67 " "
              Arrow@67..69 "->"
              Emptyspace@69..70 " "
              FunBody@70..175
                BlockExpr@70..175
                  LBrace@70..71 "{"
                  Newline@71..72 "\n"
                  Emptyspace@72..76 "    "
                  MatchExpr@76..173
                    MatchKw@76..81 "match"
                    Emptyspace@81..82 " "
                    ScrutineeExpr@82..84
                      PathExpr@82..84
                        Ident@82..84
                          Ident@82..83 "n"
                          Emptyspace@83..84 " "
                    MatchBlock@84..173
                      LBrace@84..85 "{"
                      Newline@85..86 "\n"
                      Emptyspace@86..94 "        "
                      MatchArm@94..132
                        DotPattern@94..101
                          Dot@94..95 "."
                          Ident@95..99
                            Ident@95..98 "int"
                            Emptyspace@98..99 " "
                          IdentPattern@99..101
                            Ident@99..101
                              Ident@99..100 "i"
                              Emptyspace@100..101 " "
                        Arrow@101..103 "->"
                        Emptyspace@103..104 " "
                        Call@104..123
                          PathExpr@104..115
                            Ident@104..110
                              Ident@104..110 "Number"
                            Dot@110..111 "."
                            PathExpr@111..115
                              Ident@111..115
                                Ident@111..114 "int"
                                Emptyspace@114..115 " "
                          CallArgs@115..123
                            LParen@115..116 "("
                            InfixExpr@116..122
                              PathExpr@116..118
                                Ident@116..118
                                  Ident@116..117 "i"
                                  Emptyspace@117..118 " "
                              Plus@118..119 "+"
                              Emptyspace@119..120 " "
                              IntLiteralExpr@120..122
                                IntLiteral@120..122 "16"
                            RParen@122..123 ")"
                        Newline@123..124 "\n"
                        Emptyspace@124..132 "        "
                      MatchArm@132..172
                        DotPattern@132..141
                          Dot@132..133 "."
                          Ident@133..139
                            Ident@133..138 "float"
                            Emptyspace@138..139 " "
                          IdentPattern@139..141
                            Ident@139..141
                              Ident@139..140 "f"
                              Emptyspace@140..141 " "
                        Arrow@141..143 "->"
                        Emptyspace@143..144 " "
                        Call@144..167
                          PathExpr@144..157
                            Ident@144..150
                              Ident@144..150 "Number"
                            Dot@150..151 "."
                            PathExpr@151..157
                              Ident@151..157
                                Ident@151..156 "float"
                                Emptyspace@156..157 " "
                          CallArgs@157..167
                            LParen@157..158 "("
                            InfixExpr@158..166
                              PathExpr@158..160
                                Ident@158..160
                                  Ident@158..159 "f"
                                  Emptyspace@159..160 " "
                              Plus@160..161 "+"
                              Emptyspace@161..162 " "
                              FloatLiteralExpr@162..166
                                FloatLiteral@162..166 "16.0"
                            RParen@166..167 ")"
                        Newline@167..168 "\n"
                        Emptyspace@168..172 "    "
                      RBrace@172..173 "}"
                  Newline@173..174 "\n"
                  RBrace@174..175 "}""#]]);
}
