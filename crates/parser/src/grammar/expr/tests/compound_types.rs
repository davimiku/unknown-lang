use expect_test::expect;

use crate::grammar::check_type_expr;

#[test]
#[ignore = "update when syntax is finalized"]
fn type_struct() {
    let input = "[ a: A, b: B ]";
    check_type_expr(
        input,
        expect![[r#"
            TypeExpr@0..21
              StructTypeExpr@0..21
                StructKw@0..6 "struct"
                Emptyspace@6..7 " "
                CompoundTypeBlock@7..21
                  LParen@7..8 "("
                  Emptyspace@8..9 " "
                  CompoundTypeItem@9..13
                    CompoundTypeItemIdent@9..10
                      Ident@9..10
                        Ident@9..10 "a"
                    Colon@10..11 ":"
                    Emptyspace@11..12 " "
                    CompoundTypeItemType@12..13
                      Ident@12..13
                        Ident@12..13 "A"
                  Comma@13..14 ","
                  Emptyspace@14..15 " "
                  CompoundTypeItem@15..20
                    CompoundTypeItemIdent@15..16
                      Ident@15..16
                        Ident@15..16 "b"
                    Colon@16..17 ":"
                    Emptyspace@17..18 " "
                    CompoundTypeItemType@18..20
                      Ident@18..20
                        Ident@18..19 "B"
                        Emptyspace@19..20 " "
                  RParen@20..21 ")""#]],
    )
}
