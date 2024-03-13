use lexer::TokenKind as T;

use crate::grammar::expr::parse_ident;
use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::{expr_binding_power, parse_bool_literal, parse_int_literal, parse_string_literal};

pub(super) fn parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    expr_binding_power(p, 0, parse_lhs);

    Some(m.complete(p, SyntaxKind::TypeExpr))
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(T::IntLiteral) {
        parse_int_literal(p)
    } else if p.at(T::StringLiteral) {
        parse_string_literal(p)
    } else if p.at(T::False) || p.at(T::True) {
        parse_bool_literal(p)
    } else if p.at(T::Ident) {
        parse_ident(p)
    } else if p.at(T::Union) {
        parse_union(p)
    } else if p.at(T::Struct) {
        parse_struct(p)
    } else if p.at(T::LParen) {
        parse_paren_expr(p)
    } else if p.at(T::LBracket) {
        parse_array_type(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

/// Parses a type expression beginning with a LParen
///
/// This expression could be any one of a "regular" parenthesized expression,
/// a unit type, tuple type, or
///
/// ```text
/// (Texpr + Texpr) * Texpr // paren type expr
/// ^^^^^^^^^^^^^^^
///
/// () // unit type
/// ^^
///
/// (Texpr, Texpr, Texpr) // tuple type
/// ^^^^^^^^^^^^^^^^^^^^^
/// ```
fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LParen);

    let m = p.start();
    p.bump();

    // early exit for `()`
    if p.at(T::RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::ParenExpr);
    }

    loop {
        let expr_marker = expr_binding_power(p, 0, parse_lhs);

        if expr_marker.is_none() {
            break;
        }

        if p.at(T::Comma) {
            p.bump();
        } else {
            break;
        }
    }

    p.expect(T::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_array_type(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LBracket);

    let m = p.start();

    p.bump();
    p.expect(T::RBracket);

    parse_lhs(p);

    m.complete(p, SyntaxKind::ArrayType)
}

fn parse_union(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Union);

    let m = p.start();
    p.bump();
    parse_compound_type_block(p);
    m.complete(p, SyntaxKind::UnionTypeExpr)
}

fn parse_struct(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Struct);

    let m = p.start();
    p.bump();
    parse_compound_type_block(p);
    m.complete(p, SyntaxKind::StructTypeExpr)
}

/// Parses a "compound type block"
///
/// This is a parenthesized list of identifiers with optional
/// type expressions after a colon.
///
/// ```text
/// union ( a, b )
///       ^^^^^^^^
///
/// union ( a, b: B )
///       ^^^^^^^^^^^
/// ```
///
/// TODO: remove the notes below after syntax of default parameters is finalized
/// Note that this could potentially be reused as function parameter syntax,
/// but there is an open design decision on default parameters
/// ```ignore
/// fun (a: Int = 42) -> { ... }
/// ```
///
/// This likely could be re-used given that the `= X` is optional and could
/// even be useful for default initialization of struct members.
///
/// ```ignore
/// type A = struct ( a: Int = 42 )
///
/// let a1 = A (a=1)
/// let a42 = A ()
/// ```
// TODO: better name?
fn parse_compound_type_block(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LParen);

    let m = p.start();
    p.bump();
    loop {
        if p.bump_if(T::RParen) {
            break;
        }

        parse_compound_type_item(p);

        if p.bump_if(T::RParen) {
            break;
        }

        p.expect(T::Comma);
    }

    m.complete(p, SyntaxKind::CompoundTypeBlock)
}

fn parse_compound_type_item(p: &mut Parser) -> CompletedMarker {
    // FIXME: remove assertion because it's not true?
    p.debug_assert_at(T::Ident);

    let m = p.start();

    let ident_marker = p.start();
    parse_ident(p);
    ident_marker.complete(p, SyntaxKind::CompoundTypeItemIdent);

    if p.bump_if(T::Colon) {
        let type_marker = p.start();
        expr_binding_power(p, 0, parse_lhs);
        type_marker.complete(p, SyntaxKind::CompoundTypeItemType);
    }
    if p.bump_if(T::Equals) {
        let default_marker = p.start();
        expr_binding_power(p, 0, parse_lhs);
        default_marker.complete(p, SyntaxKind::CompoundTypeItemDefault);
    }

    m.complete(p, SyntaxKind::CompoundTypeItem)
}
