use lexer::TokenKind;

use crate::grammar::expr::parse_ident_token;
use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::{
    expr_binding_power, parse_bool_literal, parse_int_literal, parse_path, parse_string_literal,
};

pub(super) fn parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    expr_binding_power(p, 0, parse_lhs);

    Some(m.complete(p, SyntaxKind::TypeExpr))
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::IntLiteral) {
        parse_int_literal(p)
    } else if p.at(TokenKind::StringLiteral) {
        parse_string_literal(p)
    } else if p.at(TokenKind::False) || p.at(TokenKind::True) {
        parse_bool_literal(p)
    } else if p.at(TokenKind::Ident) {
        parse_path(p)
    } else if p.at(TokenKind::Union) {
        parse_union(p)
    } else if p.at(TokenKind::Struct) {
        parse_struct(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr_or_function_params(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

// paren expr or function params
// paren expr: (1 + 2) * 3
//
// empty paren expr == 0 params: ()
// 1 param with explicit type: (Int)
// N params with explicit types: (Int, Int)
fn parse_paren_expr_or_function_params(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();

    // early exit for `()`
    if p.at(TokenKind::RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::ParenExpr);
    }

    loop {
        let expr_marker = expr_binding_power(p, 0, parse_lhs);

        if expr_marker.is_none() {
            break;
        }

        if p.at(TokenKind::Comma) {
            p.bump();
        } else {
            break;
        }
    }

    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_union(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Union));

    let m = p.start();
    p.bump();
    parse_compound_type_block(p);
    m.complete(p, SyntaxKind::UnionTypeExpr)
}

fn parse_struct(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Struct));

    let m = p.start();
    p.bump();
    parse_compound_type_block(p);
    m.complete(p, SyntaxKind::StructTypeExpr)
}

/// Parses a "compound type block"
// TODO: better name?
fn parse_compound_type_block(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::LBrace));

    let m = p.start();
    p.bump();
    loop {
        p.bump_all_space();
        parse_compound_type_item(p);
        p.bump_all_space();

        if p.at(TokenKind::RBrace) || p.at_end() {
            p.bump_if(TokenKind::Comma);
            break;
        } else {
            p.expect(TokenKind::Comma);
        }
    }

    p.expect(TokenKind::RBrace);

    m.complete(p, SyntaxKind::CompoundTypeBlock)
}

fn parse_compound_type_item(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Ident));

    let m = p.start();

    parse_ident_token(p);
    p.expect(TokenKind::Colon);
    expr_binding_power(p, 0, parse_lhs);

    m.complete(p, SyntaxKind::CompoundTypeItem)
}
