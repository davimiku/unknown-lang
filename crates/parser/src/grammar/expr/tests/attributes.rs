use expect_test::expect;

use crate::grammar::check;

#[test]
fn parse_attribute_on_let_binding() {
    let input = "#export
        let foo = bar";
    check(
        input,
        expect![[r##"
        Root@0..29
          Attribute@0..7
            Attribute@0..7 "#export"
          Newline@7..16
            Newline@7..8 "\n"
            Emptyspace@8..16 "        "
          LetBinding@16..29
            LetKw@16..19 "let"
            Emptyspace@19..20 " "
            Ident@20..24
              Ident@20..23 "foo"
              Emptyspace@23..24 " "
            Equals@24..25 "="
            Emptyspace@25..26 " "
            PathExpr@26..29
              Ident@26..29
                Ident@26..29 "bar""##]],
    );
}
