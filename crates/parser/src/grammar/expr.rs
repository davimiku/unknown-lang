mod bindings;
#[cfg(test)]
mod tests;
pub(super) mod types;

use std::fmt;

use lexer::TokenKind as T;

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
const CALL_ARG_START: [lexer::TokenKind; 10] = [
    T::LParen,
    T::LBracket,
    T::Ident,
    T::IntLiteral,
    T::FloatLiteral,
    T::StringLiteral,
    T::Bang,  // expression is higher precedence than function application
    T::Tilde, // temporary IntoString operator
    T::False, // TODO: remove when true/false are turned into idents
    T::True,  // TODO: remove when true/false are turned into idents
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
    p.debug_assert_at(T::LBrace);
    let m = p.start();
    p.bump();

    while !p.at(T::RBrace) && !p.at_end() {
        p.bump_all_space();
        parse_expr(p);
        p.bump_all_space();
    }

    p.expect(T::RBrace);

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
    if p.peek() == Some(T::Newline) {
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
            T::Plus => BinaryOp::Add,
            T::PlusPlus => BinaryOp::Concat,
            T::Dash => BinaryOp::Sub,
            T::Star => BinaryOp::Mul,
            T::Slash => BinaryOp::Div,
            T::Percent => BinaryOp::Rem,
            T::Caret => BinaryOp::Exp,
            T::And => BinaryOp::And,
            T::Or => BinaryOp::Or,
            T::Dot => BinaryOp::Path,
            // TODO: Arrow only applies for type expressions, should this be handled differently?
            T::Arrow => BinaryOp::Function,
            T::EqualsEquals => BinaryOp::Eq,
            T::BangEquals => BinaryOp::Ne,

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
        T::IntLiteral => parse_int_literal(p),
        T::FloatLiteral => parse_float_literal(p),
        T::StringLiteral => parse_string_literal(p),
        T::False => parse_bool_literal(p),
        T::True => parse_bool_literal(p),

        T::Ident => parse_path_ident(p),

        T::Dash => parse_negation_expr(p),
        T::Bang => parse_not_expr(p),
        T::Tilde => parse_tostring_expr(p),

        T::LParen => parse_paren_expr(p),
        T::LBrace => parse_block(p),
        T::LBracket => parse_array_literal(p),
        T::Loop => parse_loop_expr(p),

        T::Let => parse_let_binding(p),
        T::Type => parse_type_binding(p),

        T::Return => parse_return(p),

        T::If => parse_if_expr(p),
        T::For => parse_for_in_loop(p),
        T::Fun => parse_function(p),

        _ => {
            p.error();
            return None;
        }
    };

    Some(cm)
}

fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::IntLiteral);

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntLiteralExpr)
}

fn parse_float_literal(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::FloatLiteral);

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FloatLiteralExpr)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::StringLiteral);

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteralExpr)
}

fn parse_bool_literal(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at_set(&[T::False, T::True]));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolLiteralExpr)
}

// Helps wrap a lone Ident into a Path to make AST easier
// TODO: is this hacky?
fn parse_path_ident(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Ident);

    let cm = parse_ident(p);

    if let Some(T::Dot) = p.peek() {
        cm
    } else {
        cm.precede(p).complete(p, SyntaxKind::PathExpr)
    }
}

pub(crate) fn parse_ident(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(T::Ident);
    m.complete(p, SyntaxKind::Ident)
}

fn parse_call_arguments(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at_set(&CALL_ARG_START));
    let m = p.start();

    // single arg may omit the parentheses
    if !p.at(T::LParen) {
        // TODO: call expr_binding_power instead with the binding power of function application?
        // check for precedence, function application `f g h` should be like `f (g h)`
        // (right associative)
        parse_expr(p);
    } else {
        p.bump();

        while p.at_set(&CALL_ARG_START) {
            parse_expr(p);

            if p.at(T::Comma) {
                p.bump();
            }
        }
        p.expect(T::RParen);
    }

    m.complete(p, SyntaxKind::CallArgs)
}

fn parse_negation_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Dash);

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::Neg.binding_power();

    // Consume the operator’s token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Bang);

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::Not.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::NotExpr)
}

fn parse_tostring_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Tilde);

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::IntoString.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power, parse_lhs);

    m.complete(p, SyntaxKind::IntoStringExpr)
}

/// Parses an expression beginning with a LParen
///
/// This expression could be any one of a "regular" parenthesized expression,
/// a unit (empty tuple/record), a tuple, or a record.
///
/// ```text
/// (expr + expr) * expr // paren expr
/// ^^^^^^^^^^^^^
///
/// () // unit
/// ^^
///
/// (expr, expr, expr) // tuple
/// ^^^^^^^^^^^^^^^^^^
///
/// (a=expr, b=expr, c=expr) // record
/// ^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LParen);

    let m = p.start();
    p.bump();

    loop {
        if p.at(T::RParen) {
            p.bump();
            return m.complete(p, SyntaxKind::ParenExpr);
        }

        let expr_marker = expr_binding_power(p, 0, parse_lhs);
        if expr_marker.is_none() {
            break;
        }
        let expr_marker = expr_marker.unwrap();

        if p.at(T::Comma) {
            let m = expr_marker.precede(p);
            m.complete(p, SyntaxKind::ParenExprItem);
            p.bump();
        }
    }

    p.expect(T::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_array_literal(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LBracket);

    let m = p.start();
    p.bump();

    loop {
        if p.bump_if(T::RBracket) {
            break;
        }
        parse_expr(p);

        if p.bump_if(T::RBracket) {
            break;
        }
        p.expect(T::Comma); // TODO: recover at next comma if possible? `["ok", -}.?*, "ok"]
    }

    m.complete(p, SyntaxKind::ArrayLiteral)
}

fn parse_loop_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Loop);
    let m = p.start();
    p.bump();

    parse_block(p);

    m.complete(p, SyntaxKind::LoopExpr)
}

fn parse_if_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::If);
    let m = p.start();
    p.bump();

    parse_condition_expr(p);

    if p.at(T::LBrace) {
        parse_then_branch(p);
    } else {
        p.error();
    }

    if p.at(T::Else) {
        parse_else_branch(p);
    }

    m.complete(p, SyntaxKind::IfExpr)
}

fn parse_for_in_loop(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::For);

    let m = p.start();

    p.expect(T::For);
    parse_ident(p);
    p.expect(T::In);

    parse_lhs(p);

    parse_block(p);

    m.complete(p, SyntaxKind::ForInLoop)
}

/// Parses a function expression, including:
///
/// - `fun` keyword
/// - Parameter list
///   - parameter names optionally paired with a type expression
/// - Arrow
/// - Optional return type
/// - Function body (block expression)
///
/// ```text
/// fun (param: Type, param2: Type) -> Type { ... }
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
///
/// fun (param, param2) -> { ... }
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
fn parse_function(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Fun);
    let m = p.start();
    p.bump();

    if p.at(T::Ident) {
        // single parameter function can omit parens
        let param_marker = p.start();
        parse_function_param(p);
        param_marker.complete(p, SyntaxKind::FunParamList);
    } else if p.at(T::LParen) {
        parse_function_param_list(p);
    }

    p.expect(T::Arrow);

    if !p.at(T::LBrace) {
        parse_type_expr(p);
    }

    debug_assert!(p.at(T::LBrace));

    let body_marker = p.start();
    parse_expr(p);
    body_marker.complete(p, SyntaxKind::FunBody);

    m.complete(p, SyntaxKind::FunExpr)
}

/// Parses a function parameter list, which is a list of identifiers
/// where each is potentially followed by a colon and a type expression.
///
/// ```text
/// (param: Type, param2: Type) -> { }
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^
///
/// (param, param2) -> { }
/// ^^^^^^^^^^^^^^^
/// ```
fn parse_function_param_list(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::LParen);
    let m = p.start();
    p.bump();

    loop {
        if p.at(T::RParen) {
            p.bump();
            break;
        }
        parse_function_param(p);
        if p.at(T::RParen) {
            p.bump();
            break;
        }
        p.expect(T::Comma);
    }

    m.complete(p, SyntaxKind::FunParamList)
}

/// Parses a single function parameter, which may or may not
/// have a type expression
///
/// ```text
/// (param: Type, param2: Type) -> { }
///  ^^^^^^^^^^^
///
/// (param, param2) -> { }
///  ^^^^^
/// ```
fn parse_function_param(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Ident);
    let m = p.start();
    parse_ident(p);

    if p.bump_if(T::Colon) {
        parse_type_expr(p);
    }

    m.complete(p, SyntaxKind::FunParam)
}

fn parse_return(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Return);
    let m = p.start();
    p.bump();

    if p.at_end() || p.at(T::Newline) {
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
    p.debug_assert_at(T::LBrace); // TODO: be fault tolerant

    let m = p.start();
    parse_block(p);
    m.complete(p, SyntaxKind::ThenBranchExpr)
}

fn parse_else_branch(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Else);

    let m = p.start();
    p.bump();

    if p.at(T::If) {
        parse_if_expr(p);
    } else if p.at(T::LBrace) {
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
