//! This module represents bindings to variables and type variables.
//!
//! These are known as "let binding" and "type binding". In the future,
//! other kinds of bindings may be added here.
//!
//! For example:
//!
//! ```ignore
//! type A = Int
//!
//! let a: A = 1
//!
//! let b = "Hello, World"
//! ```

use lexer::TokenKind;

use crate::grammar::expr::types::parse_type_expr;
use crate::grammar::expr::{parse_expr, parse_ident_token};
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_let_binding(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Let));
    let m = p.start();
    p.bump();

    if !p.at(TokenKind::Equals) {
        // TODO: Pattern rather than Ident for destructuring
        parse_ident_token(p);

        if p.at(TokenKind::Colon) {
            p.bump();
            parse_type_expr(p);
        }

        p.expect(TokenKind::Equals);
    } else {
        // invalid syntax, but parse gracefully anyways
        // lowering will report an error
        p.bump();
    }

    parse_expr(p);

    m.complete(p, SyntaxKind::LetBinding)
}

pub(super) fn parse_type_binding(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Type));
    let m = p.start();
    p.bump();

    if !p.at(TokenKind::Equals) {
        parse_ident_token(p);

        p.expect(TokenKind::Equals);
    } else {
        // invalid syntax but parse gracefully anyways
        p.bump();
    }

    parse_type_expr(p);

    m.complete(p, SyntaxKind::TypeBinding)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_let_binding() {
        let input = "let foo = bar";
        check(
            input,
            expect![[r#"
Root@0..13
  LetBinding@0..13
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..8
      Ident@4..7 "foo"
      Emptyspace@7..8 " "
    Equals@8..9 "="
    Emptyspace@9..10 " "
    Call@10..13
      Path@10..13
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
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5
      Ident@4..5 "x"
    Colon@5..6 ":"
    Emptyspace@6..7 " "
    TypeExpr@7..11
      Path@7..11
        Ident@7..11
          Ident@7..10 "Int"
          Emptyspace@10..11 " "
    Equals@11..12 "="
    Emptyspace@12..13 " "
    IntExpr@13..14
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
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Equals@4..5 "="
    Emptyspace@5..6 " "
    IntExpr@6..7
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
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "a"
    Emptyspace@5..6 " "
    Equals@6..7 "="
    Error@7..8
      Error@7..8 "\n"
  LetBinding@8..17
    Let@8..11 "let"
    Emptyspace@11..12 " "
    Ident@12..13 "b"
    Emptyspace@13..14 " "
    Equals@14..15 "="
    Emptyspace@15..16 " "
    Path@16..17
      Ident@16..17 "a"
error at 8..11: expected Int, identifier, ‘-’ or ‘(’, but found ‘let’"#]],
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
    Type@0..4 "type"
    Emptyspace@4..5 " "
    Ident@5..7
      Ident@5..6 "A"
      Emptyspace@6..7 " "
    Equals@7..8 "="
    Emptyspace@8..9 " "
    TypeExpr@9..15
      Path@9..15
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
    Type@0..4 "type"
    Emptyspace@4..5 " "
    Equals@5..6 "="
    Emptyspace@6..7 " "
    TypeExpr@7..13
      Path@7..13
        Ident@7..13
          Ident@7..13 "String""#]],
        )
    }
}
