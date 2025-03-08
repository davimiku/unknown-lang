use expect_test::expect;

use crate::{grammar::check_expr, test_parse_type_expr};

fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = test_parse_type_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[test]
fn union_no_payloads() {
    let input = "a | b";
    check(
        input,
        expect![[r#"
TypeExpr@0..5
  InfixExpr@0..5
    Ident@0..2
      Ident@0..1 "a"
      Emptyspace@1..2 " "
    Bar@2..3 "|"
    Emptyspace@3..4 " "
    Ident@4..5
      Ident@4..5 "b""#]],
    );
}

#[test]
fn union_explicit_unit_type() {
    let input = "a: () | b: ()";
    check(
        input,
        expect![[r#"
            TypeExpr@0..13
              InfixExpr@0..13
                CompoundTypeItem@0..6
                  Ident@0..1
                    Ident@0..1 "a"
                  Colon@1..2 ":"
                  Emptyspace@2..3 " "
                  CompoundTypeItemType@3..6
                    ParenExpr@3..6
                      LParen@3..4 "("
                      RParen@4..5 ")"
                      Emptyspace@5..6 " "
                Bar@6..7 "|"
                Emptyspace@7..8 " "
                CompoundTypeItem@8..13
                  Ident@8..9
                    Ident@8..9 "b"
                  Colon@9..10 ":"
                  Emptyspace@10..11 " "
                  CompoundTypeItemType@11..13
                    ParenExpr@11..13
                      LParen@11..12 "("
                      RParen@12..13 ")""#]],
    );
}

#[test]
fn union_with_variant_types() {
    let input = "i: Int | b: Bool";
    check(
        input,
        expect![[r#"
        TypeExpr@0..16
          InfixExpr@0..16
            CompoundTypeItem@0..7
              Ident@0..1
                Ident@0..1 "i"
              Colon@1..2 ":"
              Emptyspace@2..3 " "
              CompoundTypeItemType@3..7
                Ident@3..7
                  Ident@3..6 "Int"
                  Emptyspace@6..7 " "
            Bar@7..8 "|"
            Emptyspace@8..9 " "
            CompoundTypeItem@9..16
              Ident@9..10
                Ident@9..10 "b"
              Colon@10..11 ":"
              Emptyspace@11..12 " "
              CompoundTypeItemType@12..16
                Ident@12..16
                  Ident@12..16 "Bool""#]],
    );
}

#[test]
fn create_unit_union_instance() {
    let input = "{
  type Color = red | green | blue

  Color.green
}";

    check_expr(
        input,
        expect![[r#"
        BlockExpr@0..52
          LBrace@0..1 "{"
          Newline@1..2 "\n"
          Emptyspace@2..4 "  "
          TypeBinding@4..35
            TypeKw@4..8 "type"
            Emptyspace@8..9 " "
            Ident@9..15
              Ident@9..14 "Color"
              Emptyspace@14..15 " "
            Equals@15..16 "="
            Emptyspace@16..17 " "
            TypeExpr@17..35
              InfixExpr@17..35
                Ident@17..21
                  Ident@17..20 "red"
                  Emptyspace@20..21 " "
                Bar@21..22 "|"
                Emptyspace@22..23 " "
                InfixExpr@23..35
                  Ident@23..29
                    Ident@23..28 "green"
                    Emptyspace@28..29 " "
                  Bar@29..30 "|"
                  Emptyspace@30..31 " "
                  Ident@31..35
                    Ident@31..35 "blue"
          Newline@35..36 "\n"
          Newline@36..37 "\n"
          Emptyspace@37..39 "  "
          PathExpr@39..50
            Ident@39..44
              Ident@39..44 "Color"
            Dot@44..45 "."
            PathExpr@45..50
              Ident@45..50
                Ident@45..50 "green"
          Newline@50..51 "\n"
          RBrace@51..52 "}""#]],
    );
}

#[test]
fn create_unit_union_instance_with_variable() {
    let input = "{
  type Color = (red | green | blue)

  let c = Color.green
  c
}";

    check_expr(
        input,
        expect![[r#"
        BlockExpr@0..66
          LBrace@0..1 "{"
          Newline@1..2 "\n"
          Emptyspace@2..4 "  "
          TypeBinding@4..37
            TypeKw@4..8 "type"
            Emptyspace@8..9 " "
            Ident@9..15
              Ident@9..14 "Color"
              Emptyspace@14..15 " "
            Equals@15..16 "="
            Emptyspace@16..17 " "
            TypeExpr@17..37
              ParenExpr@17..37
                LParen@17..18 "("
                InfixExpr@18..36
                  Ident@18..22
                    Ident@18..21 "red"
                    Emptyspace@21..22 " "
                  Bar@22..23 "|"
                  Emptyspace@23..24 " "
                  InfixExpr@24..36
                    Ident@24..30
                      Ident@24..29 "green"
                      Emptyspace@29..30 " "
                    Bar@30..31 "|"
                    Emptyspace@31..32 " "
                    Ident@32..36
                      Ident@32..36 "blue"
                RParen@36..37 ")"
          Newline@37..38 "\n"
          Newline@38..39 "\n"
          Emptyspace@39..41 "  "
          LetBinding@41..60
            LetKw@41..44 "let"
            Emptyspace@44..45 " "
            Ident@45..47
              Ident@45..46 "c"
              Emptyspace@46..47 " "
            Equals@47..48 "="
            Emptyspace@48..49 " "
            PathExpr@49..60
              Ident@49..54
                Ident@49..54 "Color"
              Dot@54..55 "."
              PathExpr@55..60
                Ident@55..60
                  Ident@55..60 "green"
          Newline@60..61 "\n"
          Emptyspace@61..63 "  "
          PathExpr@63..64
            Ident@63..64
              Ident@63..64 "c"
          Newline@64..65 "\n"
          RBrace@65..66 "}""#]],
    );
}

#[test]
fn create_data_union_instance() {
    let input = "{
  type Number = (int: Int | float: Float)

  let n = Number.int 16
  n
}";

    check_expr(
        input,
        expect![[r#"
        BlockExpr@0..74
          LBrace@0..1 "{"
          Newline@1..2 "\n"
          Emptyspace@2..4 "  "
          TypeBinding@4..43
            TypeKw@4..8 "type"
            Emptyspace@8..9 " "
            Ident@9..16
              Ident@9..15 "Number"
              Emptyspace@15..16 " "
            Equals@16..17 "="
            Emptyspace@17..18 " "
            TypeExpr@18..43
              ParenExpr@18..43
                LParen@18..19 "("
                InfixExpr@19..42
                  CompoundTypeItem@19..28
                    Ident@19..22
                      Ident@19..22 "int"
                    Colon@22..23 ":"
                    Emptyspace@23..24 " "
                    CompoundTypeItemType@24..28
                      Ident@24..28
                        Ident@24..27 "Int"
                        Emptyspace@27..28 " "
                  Bar@28..29 "|"
                  Emptyspace@29..30 " "
                  CompoundTypeItem@30..42
                    Ident@30..35
                      Ident@30..35 "float"
                    Colon@35..36 ":"
                    Emptyspace@36..37 " "
                    CompoundTypeItemType@37..42
                      Ident@37..42
                        Ident@37..42 "Float"
                RParen@42..43 ")"
          Newline@43..44 "\n"
          Newline@44..45 "\n"
          Emptyspace@45..47 "  "
          LetBinding@47..68
            LetKw@47..50 "let"
            Emptyspace@50..51 " "
            Ident@51..53
              Ident@51..52 "n"
              Emptyspace@52..53 " "
            Equals@53..54 "="
            Emptyspace@54..55 " "
            Call@55..68
              PathExpr@55..66
                Ident@55..61
                  Ident@55..61 "Number"
                Dot@61..62 "."
                PathExpr@62..66
                  Ident@62..66
                    Ident@62..65 "int"
                    Emptyspace@65..66 " "
              CallArgs@66..68
                IntLiteralExpr@66..68
                  IntLiteral@66..68 "16"
          Newline@68..69 "\n"
          Emptyspace@69..71 "  "
          PathExpr@71..72
            Ident@71..72
              Ident@71..72 "n"
          Newline@72..73 "\n"
          RBrace@73..74 "}""#]],
    );
}
