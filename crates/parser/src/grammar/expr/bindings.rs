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
//! let mut i = 0
//! ```

use lexer::TokenKind;

use crate::grammar::expr::types::parse_type_expr;
use crate::grammar::expr::{parse_expr, parse_ident};
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_let_binding(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(TokenKind::Let);
    let m = p.start();
    p.bump();

    p.bump_if(TokenKind::Mut);

    if !p.at(TokenKind::Equals) {
        parse_pattern(p);

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

fn parse_pattern(p: &mut Parser) -> CompletedMarker {
    // TODO: other kinds of patterns
    parse_ident(p)
}

pub(super) fn parse_type_binding(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(TokenKind::Type);
    let m = p.start();
    p.bump();

    if !p.at(TokenKind::Equals) {
        parse_ident(p);

        p.expect(TokenKind::Equals);
    } else {
        // invalid syntax but parse gracefully anyways
        p.bump();
    }

    parse_type_expr(p);

    m.complete(p, SyntaxKind::TypeBinding)
}
