use expect_test::expect;

use crate::grammar::check;

#[test]
fn parse_let_binding() {
    let input = "let foo = bar";
    check(
        input,
        expect![[r#"
Root@0..13
  LetBinding@0..13
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..8
      Ident@4..7 "foo"
      Emptyspace@7..8 " "
    Equals@8..9 "="
    Emptyspace@9..10 " "
    PathExpr@10..13
      Ident@10..13
        Ident@10..13 "bar""#]],
    );
}

#[test]
fn parse_let_binding_with_type() {
    let input = "let x: Int = 1";
    check(
        input,
        expect![[r#"
Root@0..14
  LetBinding@0..14
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5
      Ident@4..5 "x"
    Colon@5..6 ":"
    Emptyspace@6..7 " "
    TypeExpr@7..11
      Ident@7..11
        Ident@7..10 "Int"
        Emptyspace@10..11 " "
    Equals@11..12 "="
    Emptyspace@12..13 " "
    IntLiteralExpr@13..14
      IntLiteral@13..14 "1""#]],
    )
}

#[test]
fn parse_let_mut_binding() {
    let input = "let mut foo = bar";
    check(
        input,
        expect![[r#"
Root@0..17
  LetBinding@0..17
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    MutKw@4..7 "mut"
    Emptyspace@7..8 " "
    Ident@8..12
      Ident@8..11 "foo"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    PathExpr@14..17
      Ident@14..17
        Ident@14..17 "bar""#]],
    );
}

#[test]
fn parse_let_mut_binding_with_type() {
    let input = "let mut x: Int = 1";
    check(
        input,
        expect![[r#"
Root@0..18
  LetBinding@0..18
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    MutKw@4..7 "mut"
    Emptyspace@7..8 " "
    Ident@8..9
      Ident@8..9 "x"
    Colon@9..10 ":"
    Emptyspace@10..11 " "
    TypeExpr@11..15
      Ident@11..15
        Ident@11..14 "Int"
        Emptyspace@14..15 " "
    Equals@15..16 "="
    Emptyspace@16..17 " "
    IntLiteralExpr@17..18
      IntLiteral@17..18 "1""#]],
    )
}

#[test]
fn parse_binding_reassignment() {
    let input = "let mut a = 10
a = 20";
    check(
        input,
        expect![[r#"
Root@0..21
  LetBinding@0..14
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    MutKw@4..7 "mut"
    Emptyspace@7..8 " "
    Ident@8..10
      Ident@8..9 "a"
      Emptyspace@9..10 " "
    Equals@10..11 "="
    Emptyspace@11..12 " "
    IntLiteralExpr@12..14
      IntLiteral@12..14 "10"
  Newline@14..15
    Newline@14..15 "\n"
  InfixExpr@15..21
    PathExpr@15..17
      Ident@15..17
        Ident@15..16 "a"
        Emptyspace@16..17 " "
    Equals@17..18 "="
    Emptyspace@18..19 " "
    IntLiteralExpr@19..21
      IntLiteral@19..21 "20""#]],
    )
}

#[test]
fn parse_let_binding_no_ident() {
    let input = "let = 1";
    check(
        input,
        expect![[r#"
Root@0..7
  LetBinding@0..7
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    Equals@4..5 "="
    Emptyspace@5..6 " "
    IntLiteralExpr@6..7
      IntLiteral@6..7 "1""#]],
    )
}

#[test]
#[ignore = "Recovery not implemented yet"]
fn recover_on_let_token() {
    let input = "let a =\nlet b = a";
    check(
        input,
        expect![[r#"
Root@0..17
  LetBinding@0..8
    LetKw@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "a"
    Emptyspace@5..6 " "
    Equals@6..7 "="
    Error@7..8
      Error@7..8 "\n"
  LetBinding@8..17
    LetKw@8..11 "let"
    Emptyspace@11..12 " "
    Ident@12..13 "b"
    Emptyspace@13..14 " "
    Equals@14..15 "="
    Emptyspace@15..16 " "
    Path@16..17
      Ident@16..17 "a"
error at 8..11: expected Int, identifier, ‘-’ or ‘(’ but found ‘let’"#]],
    );
}

#[test]
fn parse_type_binding_alias() {
    let input = "type A = String";
    check(
        input,
        expect![[r#"
Root@0..15
  TypeBinding@0..15
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..7
      Ident@5..6 "A"
      Emptyspace@6..7 " "
    Equals@7..8 "="
    Emptyspace@8..9 " "
    TypeExpr@9..15
      Ident@9..15
        Ident@9..15 "String""#]],
    )
}

#[test]
fn parse_type_binding_no_ident() {
    let input = "type = String";
    check(
        input,
        expect![[r#"
Root@0..13
  TypeBinding@0..13
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Equals@5..6 "="
    Emptyspace@6..7 " "
    TypeExpr@7..13
      Ident@7..13
        Ident@7..13 "String""#]],
    )
}

#[test]
fn parse_type_binding_union() {
    let input = "type Status = pending | done";
    check(
        input,
        expect![[r#"
Root@0..28
  TypeBinding@0..28
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..12
      Ident@5..11 "Status"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    TypeExpr@14..28
      InfixExpr@14..28
        Ident@14..22
          Ident@14..21 "pending"
          Emptyspace@21..22 " "
        Bar@22..23 "|"
        Emptyspace@23..24 " "
        Ident@24..28
          Ident@24..28 "done""#]],
    )
}

#[test]
fn parse_type_binding_union_payload1() {
    let input = "type Status = pending: Int | done";
    check(
        input,
        expect![[r#"
Root@0..33
  TypeBinding@0..33
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..12
      Ident@5..11 "Status"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    TypeExpr@14..33
      InfixExpr@14..33
        CompoundTypeItem@14..27
          Ident@14..21
            Ident@14..21 "pending"
          Colon@21..22 ":"
          Emptyspace@22..23 " "
          CompoundTypeItemType@23..27
            Ident@23..27
              Ident@23..26 "Int"
              Emptyspace@26..27 " "
        Bar@27..28 "|"
        Emptyspace@28..29 " "
        Ident@29..33
          Ident@29..33 "done""#]],
    )
}

#[test]
fn parse_type_binding_union_payload2() {
    let input = "type Status = pending | done: Int";
    check(
        input,
        expect![[r#"
Root@0..33
  TypeBinding@0..33
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..12
      Ident@5..11 "Status"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    TypeExpr@14..33
      InfixExpr@14..33
        Ident@14..22
          Ident@14..21 "pending"
          Emptyspace@21..22 " "
        Bar@22..23 "|"
        Emptyspace@23..24 " "
        CompoundTypeItem@24..33
          Ident@24..28
            Ident@24..28 "done"
          Colon@28..29 ":"
          Emptyspace@29..30 " "
          CompoundTypeItemType@30..33
            Ident@30..33
              Ident@30..33 "Int""#]],
    )
}

#[test]
fn parse_type_binding_union_payload_both() {
    let input = "type Status = pending: Int | done: Int";
    check(
        input,
        expect![[r#"
Root@0..38
  TypeBinding@0..38
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..12
      Ident@5..11 "Status"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    TypeExpr@14..38
      InfixExpr@14..38
        CompoundTypeItem@14..27
          Ident@14..21
            Ident@14..21 "pending"
          Colon@21..22 ":"
          Emptyspace@22..23 " "
          CompoundTypeItemType@23..27
            Ident@23..27
              Ident@23..26 "Int"
              Emptyspace@26..27 " "
        Bar@27..28 "|"
        Emptyspace@28..29 " "
        CompoundTypeItem@29..38
          Ident@29..33
            Ident@29..33 "done"
          Colon@33..34 ":"
          Emptyspace@34..35 " "
          CompoundTypeItemType@35..38
            Ident@35..38
              Ident@35..38 "Int""#]],
    )
}

#[test]
fn parse_type_binding_union_three() {
    let input = "type Status = not_started | in_progress | done";
    check(
        input,
        expect![[r#"
Root@0..46
  TypeBinding@0..46
    TypeKw@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..12
      Ident@5..11 "Status"
      Emptyspace@11..12 " "
    Equals@12..13 "="
    Emptyspace@13..14 " "
    TypeExpr@14..46
      InfixExpr@14..46
        Ident@14..26
          Ident@14..25 "not_started"
          Emptyspace@25..26 " "
        Bar@26..27 "|"
        Emptyspace@27..28 " "
        InfixExpr@28..46
          Ident@28..40
            Ident@28..39 "in_progress"
            Emptyspace@39..40 " "
          Bar@40..41 "|"
          Emptyspace@41..42 " "
          Ident@42..46
            Ident@42..46 "done""#]],
    )
}
