use expect_test::expect;

use crate::test_parse_type_expr;

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
    check(input, expect![[r#"
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
                  Ident@12..16 "Bool""#]]);
}
