use expect_test::expect;

use crate::grammar::check;

#[test]
fn type_union() {
    let input = "union { a: A, b: B }";
    check(
        input,
        expect![[r#"
TypeExpr@0..20
  UnionTypeExpr@0..20
    UnionKw@0..5 "union"
    Emptyspace@5..6 " "
    CompoundTypeBlock@6..20
      LBrace@6..7 "{"
      Emptyspace@7..8 " "
      CompoundTypeItem@8..12
        Ident@8..9
          Ident@8..9 "a"
        Colon@9..10 ":"
        Emptyspace@10..11 " "
        Ident@11..12
          Ident@11..12 "A"
      Comma@12..13 ","
      Emptyspace@13..14 " "
      CompoundTypeItem@14..19
        Ident@14..15
          Ident@14..15 "b"
        Colon@15..16 ":"
        Emptyspace@16..17 " "
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
    StructKw@0..6 "struct"
    Emptyspace@6..7 " "
    CompoundTypeBlock@7..21
      LBrace@7..8 "{"
      Emptyspace@8..9 " "
      CompoundTypeItem@9..13
        Ident@9..10
          Ident@9..10 "a"
        Colon@10..11 ":"
        Emptyspace@11..12 " "
        Ident@12..13
          Ident@12..13 "A"
      Comma@13..14 ","
      Emptyspace@14..15 " "
      CompoundTypeItem@15..20
        Ident@15..16
          Ident@15..16 "b"
        Colon@16..17 ":"
        Emptyspace@17..18 " "
        Ident@18..20
          Ident@18..19 "B"
          Emptyspace@19..20 " "
      RBrace@20..21 "}""#]],
    )
}
