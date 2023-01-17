use lexer::TokenKind::{self, *};

use crate::grammar::expr::{parse_ident_token, parse_type};
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
    loop {
        p.bump_all_space();
        parse_compound_type_item(p);
        p.bump_all_space();

        if p.at(RBrace) || p.at_end() {
            p.bump_if(Comma);
            break;
        } else {
            p.expect(Comma);
        }
    }

    p.expect(RBrace);

    m.complete(p, SyntaxKind::CompoundTypeBlock)
}

fn parse_compound_type_item(p: &mut Parser) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::Ident));

    let m = p.start();

    parse_ident_token(p);
    p.expect(TokenKind::Colon);
    parse_type(p);

    m.complete(p, SyntaxKind::CompoundTypeItem)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::test_parse_type_expr;

    fn check(input: &str, expected_tree: expect_test::Expect) {
        let parse = test_parse_type_expr(input);

        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn parse_union() {
        let input = "union { a: A, b: B }";
        check(
            input,
            expect![[r#"
UnionTypeExpr@0..20
  Union@0..5 "union"
  Emptyspace@5..6 " "
  CompoundTypeBlock@6..20
    LBrace@6..7 "{"
    Emptyspace@7..8 " "
    CompoundTypeItem@8..12
      Ident@8..9
        Ident@8..9 "a"
      Colon@9..10 ":"
      Emptyspace@10..11 " "
      TypeExpr@11..12
        Path@11..12
          Ident@11..12
            Ident@11..12 "A"
    Comma@12..13 ","
    Emptyspace@13..14 " "
    CompoundTypeItem@14..19
      Ident@14..15
        Ident@14..15 "b"
      Colon@15..16 ":"
      Emptyspace@16..17 " "
      TypeExpr@17..19
        Path@17..19
          Ident@17..19
            Ident@17..18 "B"
            Emptyspace@18..19 " "
    RBrace@19..20 "}""#]],
        )
    }

    #[test]
    fn parse_struct() {
        let input = "struct { a: A, b: B }";
        check(
            input,
            expect![[r#"
StructTypeExpr@0..21
  Struct@0..6 "struct"
  Emptyspace@6..7 " "
  CompoundTypeBlock@7..21
    LBrace@7..8 "{"
    Emptyspace@8..9 " "
    CompoundTypeItem@9..13
      Ident@9..10
        Ident@9..10 "a"
      Colon@10..11 ":"
      Emptyspace@11..12 " "
      TypeExpr@12..13
        Path@12..13
          Ident@12..13
            Ident@12..13 "A"
    Comma@13..14 ","
    Emptyspace@14..15 " "
    CompoundTypeItem@15..20
      Ident@15..16
        Ident@15..16 "b"
      Colon@16..17 ":"
      Emptyspace@17..18 " "
      TypeExpr@18..20
        Path@18..20
          Ident@18..20
            Ident@18..19 "B"
            Emptyspace@19..20 " "
    RBrace@20..21 "}""#]],
        )
    }
}
