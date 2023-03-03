use expect_test::expect;

use crate::test_parse_type_expr;

fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = test_parse_type_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[test]
fn type_function_no_params() {
    let input = "() -> A";
    check(
        input,
        expect![[r#"
TypeExpr@0..7
  InfixExpr@0..7
    ParenExpr@0..3
      LParen@0..1 "("
      RParen@1..2 ")"
      Emptyspace@2..3 " "
    Arrow@3..5 "->"
    Emptyspace@5..6 " "
    Call@6..7
      Path@6..7
        Ident@6..7
          Ident@6..7 "A""#]],
    );
}

#[test]
fn type_function_one_param() {
    let input = "A -> B";
    check(
        input,
        expect![[r#"
TypeExpr@0..6
  InfixExpr@0..6
    Path@0..2
      Ident@0..2
        Ident@0..1 "A"
        Emptyspace@1..2 " "
    Arrow@2..4 "->"
    Emptyspace@4..5 " "
    Call@5..6
      Path@5..6
        Ident@5..6
          Ident@5..6 "B""#]],
    );
}

#[test]
fn type_function_one_param_with_paren() {
    let input = "(A) -> B";
    check(
        input,
        expect![[r#"
TypeExpr@0..8
  InfixExpr@0..8
    ParenExpr@0..4
      LParen@0..1 "("
      Path@1..2
        Ident@1..2
          Ident@1..2 "A"
      RParen@2..3 ")"
      Emptyspace@3..4 " "
    Arrow@4..6 "->"
    Emptyspace@6..7 " "
    Call@7..8
      Path@7..8
        Ident@7..8
          Ident@7..8 "B""#]],
    );
}

#[test]
fn type_function_two_params() {
    let input = "(A, B) -> C";
    check(
        input,
        expect![[r#"
TypeExpr@0..11
  InfixExpr@0..11
    ParenExpr@0..7
      LParen@0..1 "("
      Path@1..2
        Ident@1..2
          Ident@1..2 "A"
      Comma@2..3 ","
      Emptyspace@3..4 " "
      Path@4..5
        Ident@4..5
          Ident@4..5 "B"
      RParen@5..6 ")"
      Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    Call@10..11
      Path@10..11
        Ident@10..11
          Ident@10..11 "C""#]],
    );
}

#[test]
fn type_union() {
    let input = "union { a: A, b: B }";
    check(
        input,
        expect![[r#"
TypeExpr@0..20
  UnionTypeExpr@0..20
    Union@0..5 "union"
    Emptyspace@5..6 " "
    CompoundTypeBlock@6..20
      LBrace@6..7 "{"
      Emptyspace@7..8 " "
      CompoundTypeItem@8..12
        Ident@8..9
          Ident@8..9 "a"
        Colon@9..10 ":"
        Emptyspace@10..11 " "
        Path@11..12
          Ident@11..12
            Ident@11..12 "A"
      Comma@12..13 ","
      Emptyspace@13..14 " "
      CompoundTypeItem@14..19
        Ident@14..15
          Ident@14..15 "b"
        Colon@15..16 ":"
        Emptyspace@16..17 " "
        Path@17..19
          Ident@17..19
            Ident@17..18 "B"
            Emptyspace@18..19 " "
      RBrace@19..20 "}""#]],
    )
}

#[test]
fn type_struct() {
    let input = "struct { a: A, b: B }";
    check(
        input,
        expect![[r#"
TypeExpr@0..21
  StructTypeExpr@0..21
    Struct@0..6 "struct"
    Emptyspace@6..7 " "
    CompoundTypeBlock@7..21
      LBrace@7..8 "{"
      Emptyspace@8..9 " "
      CompoundTypeItem@9..13
        Ident@9..10
          Ident@9..10 "a"
        Colon@10..11 ":"
        Emptyspace@11..12 " "
        Path@12..13
          Ident@12..13
            Ident@12..13 "A"
      Comma@13..14 ","
      Emptyspace@14..15 " "
      CompoundTypeItem@15..20
        Ident@15..16
          Ident@15..16 "b"
        Colon@16..17 ":"
        Emptyspace@17..18 " "
        Path@18..20
          Ident@18..20
            Ident@18..19 "B"
            Emptyspace@19..20 " "
      RBrace@20..21 "}""#]],
    )
}