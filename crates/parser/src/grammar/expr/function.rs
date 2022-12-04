use lexer::TokenKind::*;

use crate::grammar::expr::parse_block;
use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::{parse_ident, parse_type};

/// Parses function definition as an expression
///
/// ```txt
/// fun () -> {}            // no args, returns Unit (elided)
/// fun () -> A {}          // no args, returns A
/// fun (a: A) -> {}        // one arg, returns Unit (elided)
/// fun (a: A) -> B {}      // one arg, returns B
/// fun (a: A, b: B) -> {} // two parameters, same possibilities for body/return
/// ```
///
pub(super) fn parse_fun_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(Fun));

    let m = p.start();
    p.bump();

    p.expect(LParen);
    parse_fun_param_list(p);
    p.expect(RParen);

    p.expect(Arrow);

    parse_fun_return_type(p);
    parse_fun_body(p);

    m.complete(p, SyntaxKind::FunExpr)
}

fn parse_fun_param_list(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    loop {
        if p.at(RParen) || p.at(Arrow) {
            // TODO: need other recovery here
            break;
        }

        parse_fun_param(p);

        if p.at(RParen) || p.at(Arrow) {
            // TODO: need other recovery here
            break;
        }

        p.expect(Comma);
    }

    m.complete(p, SyntaxKind::FunParamList)
}

fn parse_fun_param(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    parse_ident(p);
    p.expect(Colon);

    parse_type(p);

    m.complete(p, SyntaxKind::FunParam)
}

fn parse_fun_return_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(LBrace) {
        None
    } else {
        Some(parse_type(p))
    }
}

// TODO Design Decision: handle function bodies without curly braces?
// ex. let square = (a: Int) -> a ** 2
fn parse_fun_body(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(LBrace));

    let m = p.start();

    parse_block(p);

    m.complete(p, SyntaxKind::FunBody)
}

#[cfg(test)]
mod tests {
    use crate::check_expr;
    use expect_test::expect;

    #[test]
    fn parse_nullary_function() {
        check_expr(
            "() -> { }",
            expect![[r#"
Root@0..9
  InfixExpr@0..9
    ParenExpr@0..3
      LParen@0..1 "("
      RParen@1..2 ")"
      Emptyspace@2..3 " "
    Arrow@3..5 "->"
    Emptyspace@5..6 " "
    BlockExpr@6..9
      LBrace@6..7 "{"
      Emptyspace@7..8 " "
      RBrace@8..9 "}""#]],
        )
    }

    #[test]
    fn parse_nullary_function_with_return_type() {
        check_expr(
            "() -> A { }",
            expect![[r#"
Root@0..11
  InfixExpr@0..11
    ParenExpr@0..3
      LParen@0..1 "("
      RParen@1..2 ")"
      Emptyspace@2..3 " "
    Arrow@3..5 "->"
    Emptyspace@5..6 " "
    TypeExpr@6..8
      Path@6..8
        Ident@6..8
          Ident@6..7 "A"
          Emptyspace@7..8 " "
    BlockExpr@8..11
      LBrace@8..9 "{"
      Emptyspace@9..10 " "
      RBrace@10..11 "}""#]],
        )
    }

    #[test]
    fn parse_unary_function_with_explicit_param_type() {
        check_expr(
            "(a: A) -> { }",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    ParenExpr@0..7
      LParen@0..1 "("
      FunParam@1..5
        Call@1..2
          Path@1..2
            Ident@1..2
              Ident@1..2 "a"
        Colon@2..3 ":"
        Emptyspace@3..4 " "
        TypeExpr@4..5
          Path@4..5
            Ident@4..5
              Ident@4..5 "A"
      RParen@5..6 ")"
      Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    BlockExpr@10..13
      LBrace@10..11 "{"
      Emptyspace@11..12 " "
      RBrace@12..13 "}""#]],
        )
    }

    #[test]
    fn parse_unary_function_with_inferred_param_type() {
        check_expr(
            "id -> { }",
            expect![[r#"
Root@0..9
  InfixExpr@0..9
    Call@0..3
      Path@0..3
        Ident@0..3
          Ident@0..2 "id"
          Emptyspace@2..3 " "
    Arrow@3..5 "->"
    Emptyspace@5..6 " "
    BlockExpr@6..9
      LBrace@6..7 "{"
      Emptyspace@7..8 " "
      RBrace@8..9 "}""#]],
        )
    }

    #[test]
    fn parse_binary_function_with_inferred_param_types() {
        check_expr(
            "(a, b) -> { }",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    ParenExpr@0..7
      LParen@0..1 "("
      Call@1..2
        Path@1..2
          Ident@1..2
            Ident@1..2 "a"
      Comma@2..3 ","
      Emptyspace@3..4 " "
      Call@4..5
        Path@4..5
          Ident@4..5
            Ident@4..5 "b"
      RParen@5..6 ")"
      Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    BlockExpr@10..13
      LBrace@10..11 "{"
      Emptyspace@11..12 " "
      RBrace@12..13 "}""#]],
        )
    }

    #[test]
    fn parse_binary_function_with_explicit_param_types() {
        check_expr(
            "(a: A, b: B) -> { }",
            expect![[r#"
Root@0..19
  InfixExpr@0..19
    ParenExpr@0..13
      LParen@0..1 "("
      FunParam@1..5
        Call@1..2
          Path@1..2
            Ident@1..2
              Ident@1..2 "a"
        Colon@2..3 ":"
        Emptyspace@3..4 " "
        TypeExpr@4..5
          Path@4..5
            Ident@4..5
              Ident@4..5 "A"
      Comma@5..6 ","
      Emptyspace@6..7 " "
      FunParam@7..11
        Call@7..8
          Path@7..8
            Ident@7..8
              Ident@7..8 "b"
        Colon@8..9 ":"
        Emptyspace@9..10 " "
        TypeExpr@10..11
          Path@10..11
            Ident@10..11
              Ident@10..11 "B"
      RParen@11..12 ")"
      Emptyspace@12..13 " "
    Arrow@13..15 "->"
    Emptyspace@15..16 " "
    BlockExpr@16..19
      LBrace@16..17 "{"
      Emptyspace@17..18 " "
      RBrace@18..19 "}""#]],
        )
    }
}
