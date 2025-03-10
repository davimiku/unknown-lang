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
// start of a function arg due to ambiguity with `match a {}`
// (is the scrutinee `a` or is the scrutinee `a` called with empty block as arg)
// Blocks can be a function arg but need to be surrounded by parentheses.
// Leaving this comment here until language syntax is documented better
const CALL_ARG_START: [lexer::TokenKind; 8] = [
    T::LParen,
    T::LBracket,
    T::Ident,
    T::IntLiteral,
    T::FloatLiteral,
    T::StringLiteral,
    T::Bang,  // expression is higher precedence than function application
    T::Tilde, // temporary IntoString operator
];

/// Tokens that may be used at the start of a pattern
const PATTERN_START: [lexer::TokenKind; 6] = [
    T::Dot,
    T::Ident,
    T::IntLiteral,
    T::FloatLiteral,
    T::StringLiteral,
    T::Underscore,
    // TODO - LBracket, probably, for record destructuring
    // type Point = [x: Float, y: Float]
    // let [x, y] = point
];

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
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

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
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
        let op = match try_make_binary_op(curr) {
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

        let rhs = expr_binding_power(p, right_binding_power);

        lhs = match op {
            BinaryOp::Function => m.complete(p, SyntaxKind::FunExpr),
            BinaryOp::Path => m.complete(p, SyntaxKind::PathExpr),

            _ => m.complete(p, SyntaxKind::InfixExpr),
        };

        if rhs.is_none() {
            // TODO: add error like "expected expression on RHS", then recover
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

        T::Break => parse_break(p),
        T::Return => parse_return(p),

        T::If => parse_if_expr(p),
        T::Match => parse_match_expr(p),
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

pub(crate) fn parse_pattern(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at_set(&PATTERN_START);

    let m = p.start();
    let kind: SyntaxKind;
    match p.peek() {
        Some(T::Dot) => {
            kind = SyntaxKind::DotPattern;
            p.bump();
            parse_ident(p);
            if p.at_set(&PATTERN_START) {
                parse_pattern(p);
            }
        }
        Some(T::Ident) => {
            kind = SyntaxKind::IdentPattern;
            parse_ident(p);
        }
        Some(T::IntLiteral) => {
            kind = SyntaxKind::IntLiteralPattern;
            parse_int_literal(p);
        }
        Some(T::FloatLiteral) => {
            kind = SyntaxKind::FloatLiteralPattern;
            parse_float_literal(p);
        }
        Some(T::StringLiteral) => {
            kind = SyntaxKind::StringLiteralPattern;
            parse_string_literal(p);
        }
        Some(T::Underscore) => {
            kind = SyntaxKind::WildcardPattern;
            p.bump();
        }
        Some(t) => todo!("TODO - matching on pattern token {t}"),
        None => todo!(),
    };

    m.complete(p, kind)
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

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Bang);

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::Not.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NotExpr)
}

fn parse_tostring_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Tilde);

    let m = p.start();

    let ((), right_binding_power) = UnaryOp::IntoString.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power);

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

        let expr_marker = expr_binding_power(p, 0);
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

    m.complete(p, SyntaxKind::ListLiteral)
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
    p.expect(T::If);

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

fn parse_match_expr(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Match);
    let m = p.start();
    p.expect(T::Match);

    parse_scrutinee_expr(p);

    parse_match_block(p);

    m.complete(p, SyntaxKind::MatchExpr)
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

fn parse_break(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Break);
    let m = p.start();
    p.bump();

    if p.at_end() || p.at_set(&[T::Newline, T::RBrace]) {
        p.bump_all_space();
    } else {
        parse_expr(p);
    }

    m.complete(p, SyntaxKind::BreakStatement)
}

fn parse_return(p: &mut Parser) -> CompletedMarker {
    p.debug_assert_at(T::Return);
    let m = p.start();
    p.bump();

    if p.at_end() || p.at_set(&[T::Newline, T::RBrace]) {
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

fn parse_scrutinee_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    parse_expr(p);
    m.complete(p, SyntaxKind::ScrutineeExpr)
}

fn parse_match_block(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.expect(T::LBrace);
    if p.bump_if(T::RBrace) {
        return m.complete(p, SyntaxKind::MatchBlock);
    }
    loop {
        let arm_marker = p.start();
        parse_pattern(p);
        p.expect(T::Arrow);
        parse_expr(p);
        p.expect_one_of([T::Comma, T::Newline]);
        arm_marker.complete(p, SyntaxKind::MatchArm);
        if p.bump_if(T::RBrace) {
            return m.complete(p, SyntaxKind::MatchBlock);
        }
    }
    // TODO - this greedily consumes everything if there's no RBrace?
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

    /// `|`
    Union,

    /// `and`
    And,

    /// `or`
    Or,

    /// `==`
    Eq,

    /// `!=`
    Ne,

    /// `<`
    Lt,

    /// `<=`
    Le,

    /// `>`
    Gt,

    /// `>=`
    Ge,

    /// `.`
    Path,

    /// `->`
    Function,

    /// `=`  (initial assignment handled by let_binding)
    ReAssign,
}

fn try_make_binary_op(token: T) -> Option<BinaryOp> {
    Some(match token {
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
        T::Equals => BinaryOp::ReAssign,
        T::EqualsEquals => BinaryOp::Eq,
        T::BangEquals => BinaryOp::Ne,
        T::LAngle => BinaryOp::Lt,
        T::LAngleEquals => BinaryOp::Le,
        T::RAngle => BinaryOp::Gt,
        T::RAngleEquals => BinaryOp::Ge,

        _ => return None,
    })
}

const PATH_RIGHT_BINDING_POWER: u8 = 16;

impl BinaryOp {
    fn binding_power(self) -> (u8, u8) {
        match self {
            Self::Function => (1, 1),
            Self::Union => (1, 1),
            Self::ReAssign => (1, 2),
            Self::Or => (3, 4),
            Self::And => (5, 6),
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => (7, 8),
            Self::Add | Self::Sub | Self::Concat => (9, 10),
            Self::Mul | Self::Div | Self::Rem => (11, 12),
            Self::Exp => (14, 13),
            Self::Path => (15, PATH_RIGHT_BINDING_POWER),
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
