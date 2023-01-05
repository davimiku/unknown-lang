use lexer::TokenKind::{self, *};

use crate::grammar::expr::{parse_ident, parse_type};
use crate::parser::Parser;
use crate::SyntaxKind;
use crate::{grammar::expr::parse_path, parser::marker::CompletedMarker};

use super::{parse_bool_literal, parse_int_literal, parse_string_literal, BinaryOp};

pub(super) fn parse_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

/// Parses a type expression.
///
/// This could take a few forms:
///
/// ```rs
/// type Example = A
/// //             ^  simple identifier
///
/// type Example = A.B
/// //             ^^^ path
///
/// type Example = fun () -> A
/// //             ^^^^^^^^^^^  function no arguments
///
/// type Example = fun A -> B
/// //             ^^^^^^^^^^  function one argument
///
/// type Example = fun (A, B) -> C
///                ^^^^^^^^^^^^^^^ function call multiple arguments
/// ```
///
/// TODO: Split examples out to separate function docs
fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        // for now, the only binary operator in type expressions is Path
        let op = if p.at(TokenKind::Dot) {
            BinaryOp::Path
        // todo: union? intersection?
        // } else if p.at(TokenKind::Arrow) {
        // function definition like `A -> B`
        } else {
            // Not at an operator, so is not a binary expression, so break having
            // just parsed the "lhs"
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Consume the operator token
        p.bump();

        let m = lhs.precede(p);
        let parsed_rhs = expr_binding_power(p, right_binding_power).is_some();
        lhs = m.complete(p, SyntaxKind::InfixExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
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
    } else {
        p.error();
        return None;
    };

    Some(cm)
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
    while !p.at(RBrace) && !p.at_end() {
        p.bump_all_space();
        parse_compound_type_item(p);
        p.bump_all_space();
    }

    p.expect(RBrace);

    m.complete(p, SyntaxKind::CompoundTypeBlock)
}

fn parse_compound_type_item(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Ident));

    let m = p.start();
    parse_ident(p);
    p.expect(TokenKind::Colon);
    parse_type(p);
    p.expect(TokenKind::Comma);

    m.complete(p, SyntaxKind::CompoundTypeItem)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parse;

    fn check(input: &str, expected_tree: expect_test::Expect) {
        let parse = parse(input);

        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn parse_union() {
        let input = "type AB = union { a: A, b: B }";
        check(input, expect![[r#""#]])
    }
}
