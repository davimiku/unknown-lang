#[cfg(test)]
mod tests {
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
}
