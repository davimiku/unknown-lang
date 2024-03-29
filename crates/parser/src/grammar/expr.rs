mod bindings;
#[cfg(test)]
mod tests;
pub(super) mod types;

use std::fmt;

use lexer::TokenKind::{self, *};

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use self::bindings::{parse_let_binding, parse_type_binding};
use self::types::parse_type_expr;

/// Tokens that may be the start of a call argument
// Note that although blocks are expressions, LBrace can't be the
// start of a function arg due to ambiguity with `if a {}`
// (is the condition `a` or is the condition `a` called with empty block as arg)
// Blocks can be a function arg but need to be surrounded by parentheses.
// Leaving this comment here until language syntax is documented better
const CALL_ARG_START: [TokenKind; 10] = [
    LParen,
    LBracket,
    Ident,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    Bang,  // expression is higher precedence than function application
    Tilde, // temporary IntoString operator
    False, // TODO: remove when true/false are turned into idents
    True,  // TODO: remove when true/false are turned into idents
];

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0, parse_lhs)
}

// TODO: remove this after language is more developed and switch to more opaque box tests
pub(super) fn test_parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    parse_type_expr(p)
}

/// Parses a block of code, which is an expression delimited by
/// curly braces. A block may contain zero to many expressions.
pub(super) fn parse_block(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(LBrace));
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

fn expr_binding_power<F>(
    p: &mut Parser,
    minimum_binding_power: u8,
    lhs_parser: F,
) -> Option<CompletedMarker>
where
    F: Fn(&mut Parser) -> Option<CompletedMarker>,
{
    if p.peek() == Some(Newline) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, SyntaxKind::Newline));
    }

    // Uses `parse_lhs` from either value expressions or type expressions
    let mut lhs = lhs_parser(p)?;

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
            EqualsEquals => BinaryOp::Eq,
            BangEquals => BinaryOp::Ne,

            // Not at an operator, so is not a binary expression, so break
            // The "lhs" in this case is really the entire expression
            _ => break,
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

        let rhs = expr_binding_power(p, right_binding_power, parse_lhs);

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

        Ident => parse_path_ident(p),

        Dash => parse_negation_expr(p),
        Bang => parse_not_expr(p),
        Tilde => parse_tostring_expr(p),

        LParen => parse_paren_expr_or_function_params(p),
        LBrace => parse_block(p),
        LBracket => parse_array_literal(p),
        Loop => parse_loop_expr(p),

        Let => parse_let_binding(p),
        Type => parse_type_binding(p),

        Return => parse_return(p),

        If => parse_if_expr(p),
        For => parse_for_in_loop(p),

        _ => {
            p.error();
            return None;
        }
    };

    Some(cm)
}

fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(IntLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntLiteralExpr)
}

fn parse_float_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(FloatLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FloatLiteralExpr)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(StringLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteralExpr)
}

fn parse_bool_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(False) || p.at(True));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolLiteralExpr)
}

// Helps wrap a lone Ident into a Path to make AST easier
// TODO: is this hacky?
fn parse_path_ident(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Ident));

    let cm = parse_ident(p);

    if let Some(TokenKind::Dot) = p.peek() {
        cm
    } else {
        cm.precede(p).complete(p, SyntaxKind::PathExpr)
    }
}

pub(crate) fn parse_ident(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(Ident);
    m.complete(p, SyntaxKind::Ident)
}

fn parse_call_arguments(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at_set(&CALL_ARG_START));
    let m = p.start();

    // single arg may omit the parentheses
    if !p.at(LParen) {
        // TODO: call expr_binding_power instead with the binding power of function application?
        // check for precedence, function application `f g h` should be like `f (g h)`
        // (right associative)
        parse_expr(p);
    } else {
        p.bump();

        while p.at_set(&CALL_ARG_START) {
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
    debug_assert!(p.debug_at(Dash));

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::Neg.binding_power();

    // Consume the operator’s token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Bang));

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::Not.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::NotExpr)
}

fn parse_tostring_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Tilde));

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::IntoString.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::IntoStringExpr)
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
    debug_assert!(p.debug_at(LParen));

    let m = p.start();
    p.bump();

    // early exit for `()`
    if p.at(RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::ParenExpr);
    }

    let mut maybe_a_parameter_list = false;

    loop {
        let expr_marker = expr_binding_power(p, 0, parse_lhs);

        if expr_marker.is_none() {
            break;
        }
        let expr_marker = expr_marker.unwrap();

        if p.at(Colon) {
            p.bump();

            parse_type_expr(p);
            maybe_a_parameter_list = true;
        }

        if p.at(Comma) {
            let m = expr_marker.precede(p);
            m.complete(p, SyntaxKind::ParenExprItem);
            p.bump();
            maybe_a_parameter_list = true;
        } else {
            if maybe_a_parameter_list {
                let m = expr_marker.precede(p);
                m.complete(p, SyntaxKind::ParenExprItem);
            }
            break;
        }
    }

    p.expect(RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_array_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(LBracket));

    let m = p.start();
    p.bump();

    // early exit for `[]`
    if p.at(RBracket) {
        p.bump();
        return m.complete(p, SyntaxKind::ArrayLiteral);
    }

    loop {
        parse_expr(p);
        if p.at(RBracket) {
            p.bump();
            break;
        }
        p.expect(Comma); // TODO: recover at next comma if possible? `["ok", -}.?*, "ok"]
    }

    m.complete(p, SyntaxKind::ArrayLiteral)
}

fn parse_loop_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Loop));
    let m = p.start();
    p.bump();

    parse_block(p);

    m.complete(p, SyntaxKind::LoopExpr)
}

fn parse_if_expr(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(If));
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

fn parse_for_in_loop(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(For));

    let m = p.start();

    p.expect(For);
    parse_ident(p);
    p.expect(In);

    parse_lhs(p);

    parse_block(p);

    m.complete(p, SyntaxKind::ForInLoop)
}

fn parse_return(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Return));
    let m = p.start();
    p.bump();

    if p.at_end() || p.at(Newline) {
        p.bump_all_space();
    } else {
        parse_expr(p);
    }

    m.complete(p, SyntaxKind::ReturnStatement)
}

fn parse_condition_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    parse_expr(p);
    m.complete(p, SyntaxKind::ConditionExpr)
}

fn parse_then_branch(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(LBrace)); // TODO: be fault tolerant

    let m = p.start();
    parse_block(p);
    m.complete(p, SyntaxKind::ThenBranchExpr)
}

fn parse_else_branch(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.debug_at(Else));

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

#[derive(Debug, PartialEq)]
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

    /// `^`
    Exp,

    /// `and`
    And,

    /// `or`
    Or,

    /// `==`
    Eq,

    /// `!=`
    Ne,

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
            Self::Eq | Self::Ne => (7, 8),
            Self::Add | Self::Sub | Self::Concat => (9, 10),
            Self::Mul | Self::Div | Self::Rem => (11, 12),
            Self::Exp => (14, 13),
            Self::Path => (15, 16),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    IntoString,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::IntoString => write!(f, "~"),
        }
    }
}

impl UnaryOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 11), // TODO: should be higher than Exp but less than Path?
            Self::IntoString => ((), 9),
            Self::Not => ((), 7),
        }
    }
}
