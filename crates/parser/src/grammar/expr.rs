mod bindings;
#[cfg(test)]
mod tests;
pub(super) mod types;

use lexer::TokenKind::{self, *};

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use self::bindings::{parse_let_binding, parse_type_binding};
use self::types::parse_type_expr;

/// Tokens that may be the start of a function argument
// Note that although blocks are expressions, LBrace can't be the
// start of a function arg due to ambiguity with `if a {}`
// (is the condition `a` or is the condition `a` called with empty block as arg)
// Blocks can be a function arg but need to be surrounded by parentheses.
// Leaving this comment here until language syntax is documented better
const FUNCTION_ARG_START: [TokenKind; 7] = [
    LParen,
    Ident,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    False, // TODO: remove when true/false are turned into idents
    True,  // TODO: remove when true/false are turned into idents
];

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

pub(super) fn parse_type(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Ident)); // Update for literal types?

    let m = p.start();
    parse_type_expr(p);
    m.complete(p, SyntaxKind::TypeExpr)
}

// TODO: remove this after language is more developed and switch to more opaque box tests
pub(super) fn test_parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    parse_type_expr(p)
}

/// Parses a block of code, which is an expression delimited by
/// curly braces. A block may contain zero to many expressions.
pub(super) fn parse_block(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(LBrace));
    let m = p.start();
    p.bump();

    while !p.at(RBrace) && !p.at_end() {
        p.bump_all_space();
        parse_expr(p);
        p.bump_all_space();
    }

    p.expect(RBrace);

    m.complete(p, SyntaxKind::BlockExpr)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        let curr = p.peek();
        if curr.is_none() {
            return Some(lhs);
        }
        let curr = curr.unwrap();
        let op = match curr {
            Plus => BinaryOp::Add,
            PlusPlus => BinaryOp::Concat,
            Dash => BinaryOp::Sub,
            Star => BinaryOp::Mul,
            Slash => BinaryOp::Div,
            Percent => BinaryOp::Rem,
            Caret => BinaryOp::Exp,
            And => BinaryOp::And,
            Or => BinaryOp::Or,
            Dot => BinaryOp::Path,
            Arrow => BinaryOp::Function,

            // Not at an operator, so is not a binary expression, so break having
            // just parsed the "lhs"
            _ => break,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Consume the operator token
        p.bump();

        // Starts a new marker that "wraps" the already parsed LHS, so that we can have
        // an InfixExpr with LHS and RHS
        let m = lhs.precede(p);

        let rhs = expr_binding_power(p, right_binding_power);
        lhs = m.complete(p, SyntaxKind::InfixExpr);

        if rhs.is_none() {
            // TODO: add error like "expected expression on RHS"
            break;
        }
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let curr = p.peek();
    if curr.is_none() {
        p.error();
        return None;
    }
    let curr = curr.unwrap();

    let cm = match curr {
        IntLiteral => parse_int_literal(p),
        FloatLiteral => parse_float_literal(p),
        StringLiteral => parse_string_literal(p),
        False => parse_bool_literal(p),
        True => parse_bool_literal(p),

        Ident => parse_ident(p)?,

        Dash => parse_negation_expr(p),
        Not => parse_not_expr(p),
        // TODO: this might also be the start of a tuple?
        // ex.
        // let point = (1, 2)
        //             ^
        // parentheses are already overloaded as a grouping expression & parameters,
        // probably can't overload again as tuples
        LParen => parse_paren_expr_or_function_params(p),
        LBrace => parse_block(p),
        Loop => parse_loop_expr(p),

        Let => parse_let_binding(p),
        Type => parse_type_binding(p),

        If => parse_if_expr(p),

        _ => {
            p.error();
            return None;
        }
    };

    Some(cm)
}

fn parse_ident(p: &mut Parser) -> Option<CompletedMarker> {
    Some(if p.last_token_kind == Arrow {
        // "-> Type { ... }"
        //     ^^^^
        parse_type(p);

        parse_expr(p)?
    } else {
        parse_variable_ident(p)
    })
}

fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(IntLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntExpr)
}

fn parse_float_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(FloatLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FloatExpr)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(StringLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringExpr)
}

fn parse_bool_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(False) || p.at(True));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolExpr)
}

/// Parses starting with an identifier from an expression context.
///
/// This could take a few forms:
///
/// ```txt
/// let example = b
/// //            ^   simple identifier
///
/// let example = b.c
/// //            ^^^ path
///
/// let example = f ()
/// //            ^^^^ function call no arguments
///
/// let example = f b
/// //            ^^^  function call one argument
///
/// let example = f (b, c)
///               ^^^^^^^^ function call multiple arguments
/// ```
fn parse_variable_ident(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Ident));

    let m = p.start();
    parse_path(p);

    if p.at_set(&FUNCTION_ARG_START) {
        parse_function_arguments(p);
    }

    m.complete(p, SyntaxKind::Call)
}

fn parse_path(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Ident));
    let m = p.start();

    parse_ident_token(p);

    while p.at(Dot) {
        p.bump(); // eat Dot

        // TODO: would be better with recovery here, at least to the next newline or '}'
        // or potentially recover right away and continue to parse the next thing
        parse_ident_token(p);
    }

    m.complete(p, SyntaxKind::Path)
}

pub(crate) fn parse_ident_token(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(Ident);
    m.complete(p, SyntaxKind::Ident)
}

fn parse_function_arguments(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at_set(&FUNCTION_ARG_START));
    let m = p.start();

    // single arg may omit the parentheses
    if !p.at(LParen) {
        parse_expr(p);
    } else {
        p.bump();

        while p.at_set(&FUNCTION_ARG_START) {
            parse_expr(p);

            if p.at(Comma) {
                p.bump();
            }
        }
        p.expect(RParen);
    }

    m.complete(p, SyntaxKind::CallArgs)
}

fn parse_negation_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Dash));

    let m = p.start();

    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Consume the operator’s token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Not));

    let m = p.start();

    let op = UnaryOp::Not;
    let ((), right_binding_power) = op.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NotExpr)
}

// paren expr or function params??
// paren expr: (1 + 2) * 3
//
// empty paren expr == 0 params: ()
// 1 param with explicit type: (a: Int)
// 1 param with inferred type: (a)
// 2 params with explicit types: (a: Int, b: Int)
// 2 params with inferred types: (a, b)
fn parse_paren_expr_or_function_params(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(LParen));

    let m = p.start();
    p.bump();

    // early exit for `()`
    if p.at(RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::ParenExpr);
    }

    loop {
        let e = expr_binding_power(p, 0);

        if e.is_none() {
            break;
        }
        let e = e.unwrap();

        if p.at(Colon) {
            p.bump();

            let m = e.precede(p);

            parse_type(p);
            m.complete(p, SyntaxKind::FunParam);
        }

        if p.at(Comma) {
            p.bump();
        } else {
            break;
        }
    }

    p.expect(RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_loop_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Loop));
    let m = p.start();
    p.bump();

    parse_block(p);

    m.complete(p, SyntaxKind::LoopExpr)
}

fn parse_if_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(If));
    let m = p.start();
    p.bump();

    parse_condition_expr(p);

    if p.at(LBrace) {
        parse_then_branch(p);
    } else {
        p.error();
    }

    if p.at(Else) {
        parse_else_branch(p);
    }

    m.complete(p, SyntaxKind::IfExpr)
}

fn parse_condition_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    parse_expr(p);
    m.complete(p, SyntaxKind::ConditionExpr)
}

fn parse_then_branch(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(LBrace)); // TODO: be fault tolerant

    let m = p.start();
    parse_block(p);
    m.complete(p, SyntaxKind::ThenBranchExpr)
}

fn parse_else_branch(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(Else));

    let m = p.start();
    p.bump();

    if p.at(If) {
        parse_if_expr(p);
    } else if p.at(LBrace) {
        parse_block(p);
    } else {
        p.error();
    }

    m.complete(p, SyntaxKind::ElseBranchExpr)
}

enum BinaryOp {
    /// `+`
    Add,

    /// `-`
    Sub,

    /// `*`
    Mul,

    /// `/`
    Div,

    /// `++`
    Concat,

    /// `%`
    Rem,

    /// `**`
    Exp,

    /// `and`
    And,

    /// `or`
    Or,

    /// `.`
    Path,

    // `->`
    Function,
}

impl BinaryOp {
    /// Binding power tuple of (left, right)
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Function => (1, 1),
            Self::Or => (3, 4),
            Self::And => (5, 6),
            Self::Add | Self::Sub | Self::Concat => (7, 8),
            Self::Mul | Self::Div | Self::Rem => (9, 10),
            Self::Exp => (12, 11),
            Self::Path => (13, 14),
        }
    }
}

enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 11),
            Self::Not => ((), 5),
        }
    }
}
