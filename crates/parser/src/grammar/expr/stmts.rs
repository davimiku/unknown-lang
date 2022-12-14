//!
//! This module represents what users would consider to be statements.
//!
//! In the compiler, they are represented as a variant of Expr that happens
//! to always return Unit.
//!
//! For example:
//!
//! ```ignore
//! import { read_file } from "stdlib.io"
//! ```

use lexer::TokenKind;

use crate::grammar::expr::{parse_expr, parse_ident, parse_type};
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_let_binding(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Let));
    let m = p.start();
    p.bump();

    if !p.at(TokenKind::Equals) {
        // TODO: Pattern rather than Ident for destructuring
        parse_ident(p);

        if p.at(TokenKind::Colon) {
            p.bump();
            parse_type(p);
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

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
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
    fn parse_variable_with_type_annotation() {
        check(
            "let x: Int = 1",
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
    fn parse_variable_without_identifier() {
        check(
            "let = 1",
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
        check(
            "let a =\nlet b = a",
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
}
