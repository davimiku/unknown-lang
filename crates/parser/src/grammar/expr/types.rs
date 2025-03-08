use lexer::TokenKind as T;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::{
    parse_call_arguments, parse_ident, parse_int_literal, parse_string_literal, BinaryOp,
    CALL_ARG_START,
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
    if p.at_set(&CALL_ARG_START) {
        // Starts a new marker that "wraps" the already parsed callee, so that we
        // can have a CallExpr with callee and args
        let m = lhs.precede(p);

        parse_call_arguments(p);

        lhs = m.complete(p, SyntaxKind::Call);
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(T::IntLiteral) {
        parse_int_literal(p)
    } else if p.at(T::StringLiteral) {
        parse_string_literal(p)
    } else if p.at(T::Ident) {
        parse_compound_type_item(p)
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

fn parse_array_type(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LBracket);

    let m = p.start();

    p.bump();
    p.expect(T::RBracket);

    parse_lhs(p);

    m.complete(p, SyntaxKind::ArrayType)
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

    let ident_marker = parse_ident(p);

    if p.bump_if(T::Colon) {
        let compound_marker = ident_marker.precede(p);

        let type_marker = p.start();
        type_expr_binding_power(p, 2);
        type_marker.complete(p, SyntaxKind::CompoundTypeItemType);
        if p.bump_if(T::Equals) {
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
