use expect_test::expect;

use crate::grammar::{check, check_expr, check_type_expr};

#[test]
fn parse_empty_record_type() {
    let input = "type EmptyRecord = []";
    check(
        input,
        expect![[r#"
        Root@0..21
          TypeBinding@0..21
            TypeKw@0..4 "type"
            Emptyspace@4..5 " "
            Ident@5..17
              Ident@5..16 "EmptyRecord"
              Emptyspace@16..17 " "
            Equals@17..18 "="
            Emptyspace@18..19 " "
            TypeExpr@19..21
              RecordType@19..21
                LBracket@19..20 "["
                RBracket@20..21 "]""#]],
    );
}

#[test]
fn parse_record_type_one_field() {
    let input = "type OneField = [ field: Type ]";
    check(
        input,
        expect![[r#"
        Root@0..31
          TypeBinding@0..31
            TypeKw@0..4 "type"
            Emptyspace@4..5 " "
            Ident@5..14
              Ident@5..13 "OneField"
              Emptyspace@13..14 " "
            Equals@14..15 "="
            Emptyspace@15..16 " "
            TypeExpr@16..31
              RecordType@16..31
                LBracket@16..17 "["
                Emptyspace@17..18 " "
                CompoundTypeItem@18..30
                  Ident@18..23
                    Ident@18..23 "field"
                  Colon@23..24 ":"
                  Emptyspace@24..25 " "
                  Ident@25..30
                    Ident@25..29 "Type"
                    Emptyspace@29..30 " "
                RBracket@30..31 "]""#]],
    );
}

#[test]
fn parse_record_type_two_fields() {
    let input = "type TwoFields = [ field1: Type1, field2: Type2 ]";
    check(
        input,
        expect![[r#"
        Root@0..49
          TypeBinding@0..49
            TypeKw@0..4 "type"
            Emptyspace@4..5 " "
            Ident@5..15
              Ident@5..14 "TwoFields"
              Emptyspace@14..15 " "
            Equals@15..16 "="
            Emptyspace@16..17 " "
            TypeExpr@17..49
              RecordType@17..49
                LBracket@17..18 "["
                Emptyspace@18..19 " "
                CompoundTypeItem@19..32
                  Ident@19..25
                    Ident@19..25 "field1"
                  Colon@25..26 ":"
                  Emptyspace@26..27 " "
                  Ident@27..32
                    Ident@27..32 "Type1"
                Comma@32..33 ","
                Emptyspace@33..34 " "
                CompoundTypeItem@34..48
                  Ident@34..40
                    Ident@34..40 "field2"
                  Colon@40..41 ":"
                  Emptyspace@41..42 " "
                  Ident@42..48
                    Ident@42..47 "Type2"
                    Emptyspace@47..48 " "
                RBracket@48..49 "]""#]],
    )
}

#[test]
fn parse_record_type() {
    let input = "[ x: Float, y: Float ]";
    check_type_expr(
        input,
        expect![[r#"
            TypeExpr@0..22
              RecordType@0..22
                LBracket@0..1 "["
                Emptyspace@1..2 " "
                CompoundTypeItem@2..10
                  Ident@2..3
                    Ident@2..3 "x"
                  Colon@3..4 ":"
                  Emptyspace@4..5 " "
                  Ident@5..10
                    Ident@5..10 "Float"
                Comma@10..11 ","
                Emptyspace@11..12 " "
                CompoundTypeItem@12..21
                  Ident@12..13
                    Ident@12..13 "y"
                  Colon@13..14 ":"
                  Emptyspace@14..15 " "
                  Ident@15..21
                    Ident@15..20 "Float"
                    Emptyspace@20..21 " "
                RBracket@21..22 "]""#]],
    );
}

#[test]
fn parse_record_type_binding() {
    let input = "type Point = [ x: Float, y: Float ]";
    check(
        input,
        expect![[r#"
            Root@0..35
              TypeBinding@0..35
                TypeKw@0..4 "type"
                Emptyspace@4..5 " "
                Ident@5..11
                  Ident@5..10 "Point"
                  Emptyspace@10..11 " "
                Equals@11..12 "="
                Emptyspace@12..13 " "
                TypeExpr@13..35
                  RecordType@13..35
                    LBracket@13..14 "["
                    Emptyspace@14..15 " "
                    CompoundTypeItem@15..23
                      Ident@15..16
                        Ident@15..16 "x"
                      Colon@16..17 ":"
                      Emptyspace@17..18 " "
                      Ident@18..23
                        Ident@18..23 "Float"
                    Comma@23..24 ","
                    Emptyspace@24..25 " "
                    CompoundTypeItem@25..34
                      Ident@25..26
                        Ident@25..26 "y"
                      Colon@26..27 ":"
                      Emptyspace@27..28 " "
                      Ident@28..34
                        Ident@28..33 "Float"
                        Emptyspace@33..34 " "
                    RBracket@34..35 "]""#]],
    );
}

#[test]
fn parse_record_two_fields() {
    check_expr(
        "[ field_a = value_a, field_b = value_b ]",
        expect![[r#"
        RecordLiteral@0..40
          LBracket@0..1 "["
          Emptyspace@1..2 " "
          Ident@2..10
            Ident@2..9 "field_a"
            Emptyspace@9..10 " "
          Equals@10..11 "="
          Emptyspace@11..12 " "
          PathExpr@12..19
            Ident@12..19
              Ident@12..19 "value_a"
          Comma@19..20 ","
          Emptyspace@20..21 " "
          Ident@21..29
            Ident@21..28 "field_b"
            Emptyspace@28..29 " "
          Equals@29..30 "="
          Emptyspace@30..31 " "
          PathExpr@31..39
            Ident@31..39
              Ident@31..38 "value_b"
              Emptyspace@38..39 " "
          RBracket@39..40 "]""#]],
    )
}
