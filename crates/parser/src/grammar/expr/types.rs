use lexer::TokenKind as T;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::{
    parse_ident, parse_int_literal, parse_string_literal, BinaryOp, CALL_ARG_START,
    PATH_RIGHT_BINDING_POWER,
};

pub(super) fn parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    type_expr_binding_power(p, 0);

    Some(m.complete(p, SyntaxKind::TypeExpr))
}

fn type_expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    if p.peek() == Some(T::Newline) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, SyntaxKind::Newline));
    }

    // Uses `parse_lhs` from either value expressions or type expressions
    let mut lhs = parse_lhs(p)?;

    loop {
        let curr = p.peek();
        if curr.is_none() {
            return Some(lhs);
        }
        let curr = curr.unwrap();
        let op = match make_binary_op(curr) {
            Some(op) => op,

            // Not at an operator, so is not a binary expression, so break
            // The "lhs" in this case is really the entire expression
            None => break,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Consume the operator token
        p.bump();

        // Starts a new marker that "wraps" the already parsed LHS, so that we can have
        // an expression surrounding the LHS and RHS
        let m = lhs.precede(p);

        let rhs = type_expr_binding_power(p, right_binding_power);

        lhs = match op {
            BinaryOp::Function => m.complete(p, SyntaxKind::FunExpr),
            BinaryOp::Path => m.complete(p, SyntaxKind::PathExpr),

            _ => m.complete(p, SyntaxKind::InfixExpr),
        };

        if rhs.is_none() {
            // TODO: add error like "expected expression on RHS"
            break;
        }
    }

    // Having just finished parsing an expression, check if the next token
    // could be the start of function arguments, thereby making this just-parsed
    // expression the callee of a Call expression.
    if p.at_set(&CALL_ARG_START) && minimum_binding_power < PATH_RIGHT_BINDING_POWER {
        // Starts a new marker that "wraps" the already parsed callee, so that we
        // can have a CallExpr with callee and args
        let m = lhs.precede(p);

        parse_type_call_arguments(p);

        lhs = m.complete(p, SyntaxKind::Call);
    }

    Some(lhs)
}

/// Parse arguments to a type application.
///
/// A single type argument can omit parentheses ex. `List Int`,
/// otherwise parentheses are used for multiple arguments ex. `Result (Int, String)`
fn parse_type_call_arguments(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at_set(&CALL_ARG_START));
    let m = p.start();

    if !p.at(T::LParen) {
        type_expr_binding_power(p, 0);
    } else {
        p.bump();

        while p.at_set(&CALL_ARG_START) {
            type_expr_binding_power(p, 0);

            if p.at(T::Comma) {
                p.bump();
            }
        }
        p.expect(T::RParen);
    }

    m.complete(p, SyntaxKind::CallArgs)
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(T::IntLiteral) {
        parse_int_literal(p)
    } else if p.at(T::StringLiteral) {
        parse_string_literal(p)
    } else if p.at(T::Ident) {
        parse_compound_type_item(p)
    } else if p.at(T::TypeParameter) {
        parse_type_parameter(p)
    } else if p.at(T::Intrinsic) {
        let m = p.start();
        p.bump();
        m.complete(p, SyntaxKind::IntrinsicTypeExpr)
    } else if p.at(T::LParen) {
        parse_paren_expr(p)
    } else if p.at(T::LBracket) {
        parse_record_type(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

/// Type parameters, ex. the `'A` part of `List 'A`
/// These are represented as identifiers for now; semantic generic substitution will be
/// added with the module/type-application work.
fn parse_type_parameter(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::TypeParameter);
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::Ident)
}

/// Parses a type expression beginning with a LParen
///
/// This expression could be a parenthesized expression or tuple type
///
/// ```text
/// (Texpr + Texpr) * Texpr // paren type expr
/// ^^^^^^^^^^^^^^^
///
/// (Texpr, Texpr, Texpr) // tuple type
/// ^^^^^^^^^^^^^^^^^^^^^
///
/// () // invalid
/// ^^
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
        let expr_marker = type_expr_binding_power(p, 0);

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

fn parse_record_type(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LBracket);

    let m = p.start();

    p.bump();
    // early exit for `[]`
    if p.at(T::RBracket) {
        p.bump();
        return m.complete(p, SyntaxKind::RecordType);
    }

    loop {
        let m = p.start();
        parse_ident(p);
        p.expect(T::Colon);
        type_expr_binding_power(p, 0);
        m.complete(p, SyntaxKind::CompoundTypeItem);

        if p.at(T::Comma) {
            p.bump();
        } else {
            break;
        }
    }

    p.expect(T::RBracket);

    m.complete(p, SyntaxKind::RecordType)
}

fn parse_compound_type_item(p: &mut Parser) -> CompletedMarker {
    // FIXME: remove assertion because it's not true?
    p.debug_assert_at(T::Ident);

    let ident_marker = parse_ident(p);

    if p.bump_if_at(T::Colon) {
        let compound_marker = ident_marker.precede(p);

        let type_marker = p.start();
        type_expr_binding_power(p, 2);
        type_marker.complete(p, SyntaxKind::CompoundTypeItemType);
        if p.bump_all_if_at(T::Equals) {
            let default_marker = p.start();
            type_expr_binding_power(p, 0);
            default_marker.complete(p, SyntaxKind::CompoundTypeItemDefault);
        }
        compound_marker.complete(p, SyntaxKind::CompoundTypeItem)
    } else {
        ident_marker
    }
}

fn make_binary_op(token: T) -> Option<BinaryOp> {
    Some(match token {
        T::Plus => BinaryOp::Add,
        T::PlusPlus => BinaryOp::Concat,
        T::Dash => BinaryOp::Sub,
        T::Star => BinaryOp::Mul,
        T::Slash => BinaryOp::Div,
        T::Percent => BinaryOp::Rem,
        T::Caret => BinaryOp::Exp,
        T::Bar => BinaryOp::Union,
        T::And => BinaryOp::And,
        T::Or => BinaryOp::Or,
        T::Dot => BinaryOp::Path,
        T::EqualsEquals => BinaryOp::Eq,
        T::BangEquals => BinaryOp::Ne,
        T::LAngle => BinaryOp::Lt,
        T::LAngleEquals => BinaryOp::Le,
        T::RAngle => BinaryOp::Gt,
        T::RAngleEquals => BinaryOp::Ge,
        T::Arrow => BinaryOp::Function,

        _ => return None,
    })
}
