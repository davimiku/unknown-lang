use lexer::TokenKind;

use crate::grammar::expr::parse_block;
use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use super::parse_type;

pub(super) fn parse_fun_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Fun));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::LParen);
    parse_fun_param_list(p);
    p.expect(TokenKind::RParen);

    p.expect(TokenKind::Arrow);

    parse_fun_return_type(p);
    parse_fun_body(p);

    m.complete(p, SyntaxKind::FunExpr)
}

fn parse_fun_param_list(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    loop {
        if p.at(TokenKind::RParen) || p.at(TokenKind::Arrow) {
            // TODO: need other recovery here
            break;
        }

        parse_fun_param(p);

        if p.at(TokenKind::RParen) || p.at(TokenKind::Arrow) {
            // TODO: need other recovery here
            break;
        }

        // FIXME: comma should be optional on the last parameter
        // Currently causes an infinite loop if comma is missing!
        p.expect(TokenKind::Comma);
    }

    m.complete(p, SyntaxKind::FunParamList)
}

fn parse_fun_param(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Colon);

    parse_type(p);

    m.complete(p, SyntaxKind::FunParam)
}

fn parse_fun_return_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LBrace) {
        None
    } else {
        Some(parse_type(p))
    }
}

fn parse_fun_body(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LBrace));

    let m = p.start();

    parse_block(p);

    m.complete(p, SyntaxKind::FunBody)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_empty_function() {
        check(
            "let x = fun () -> {}",
            expect![[r#"
Root@0..20
  VariableDef@0..20
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "x"
    Emptyspace@5..6 " "
    Equals@6..7 "="
    Emptyspace@7..8 " "
    FunExpr@8..20
      Fun@8..11 "fun"
      Emptyspace@11..12 " "
      LParen@12..13 "("
      FunParamList@13..13
      RParen@13..14 ")"
      Emptyspace@14..15 " "
      Arrow@15..17 "->"
      Emptyspace@17..18 " "
      FunBody@18..20
        BlockExpr@18..20
          LBrace@18..19 "{"
          RBrace@19..20 "}""#]],
        );
    }

    #[test]
    fn parse_function_block_body() {
        check(
            r#"fun () -> {
          let a = 1
          let b = 2
          a + b
        }"#,
            expect![[r#"
Root@0..77
  FunExpr@0..77
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..5
    RParen@5..6 ")"
    Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    FunBody@10..77
      BlockExpr@10..77
        LBrace@10..11 "{"
        Newline@11..12 "\n"
        Emptyspace@12..22 "          "
        VariableDef@22..31
          Let@22..25 "let"
          Emptyspace@25..26 " "
          Ident@26..27 "a"
          Emptyspace@27..28 " "
          Equals@28..29 "="
          Emptyspace@29..30 " "
          IntExpr@30..31
            Int@30..31 "1"
        Newline@31..32 "\n"
        Emptyspace@32..42 "          "
        VariableDef@42..51
          Let@42..45 "let"
          Emptyspace@45..46 " "
          Ident@46..47 "b"
          Emptyspace@47..48 " "
          Equals@48..49 "="
          Emptyspace@49..50 " "
          IntExpr@50..51
            Int@50..51 "2"
        Newline@51..52 "\n"
        Emptyspace@52..62 "          "
        InfixExpr@62..67
          IdentExpr@62..64
            Ident@62..63 "a"
            Emptyspace@63..64 " "
          Plus@64..65 "+"
          Emptyspace@65..66 " "
          IdentExpr@66..67
            Ident@66..67 "b"
        Newline@67..68 "\n"
        Emptyspace@68..76 "        "
        RBrace@76..77 "}""#]],
        )
    }

    #[test]
    fn parse_function_return_type() {
        check(
            "fun () -> int { 1 }",
            expect![[r#"
Root@0..19
  FunExpr@0..19
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..5
    RParen@5..6 ")"
    Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    TypeExpr@10..14
      IdentExpr@10..14
        Ident@10..13 "int"
        Emptyspace@13..14 " "
    FunBody@14..19
      BlockExpr@14..19
        LBrace@14..15 "{"
        Emptyspace@15..16 " "
        IntExpr@16..18
          Int@16..17 "1"
          Emptyspace@17..18 " "
        RBrace@18..19 "}""#]],
        )
    }

    #[test]
    fn parse_function_one_param() {
        check(
            "fun (a: int) -> int { a + 1 }",
            expect![[r#"
Root@0..29
  FunExpr@0..29
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..11
      FunParam@5..11
        Ident@5..6 "a"
        Colon@6..7 ":"
        Emptyspace@7..8 " "
        TypeExpr@8..11
          IdentExpr@8..11
            Ident@8..11 "int"
    RParen@11..12 ")"
    Emptyspace@12..13 " "
    Arrow@13..15 "->"
    Emptyspace@15..16 " "
    TypeExpr@16..20
      IdentExpr@16..20
        Ident@16..19 "int"
        Emptyspace@19..20 " "
    FunBody@20..29
      BlockExpr@20..29
        LBrace@20..21 "{"
        Emptyspace@21..22 " "
        InfixExpr@22..28
          IdentExpr@22..24
            Ident@22..23 "a"
            Emptyspace@23..24 " "
          Plus@24..25 "+"
          Emptyspace@25..26 " "
          IntExpr@26..28
            Int@26..27 "1"
            Emptyspace@27..28 " "
        RBrace@28..29 "}""#]],
        )
    }

    #[test]
    fn parse_function_two_params() {
        check(
            "fun (a: int, b: int) -> int { a + b }",
            expect![[r#"
Root@0..37
  FunExpr@0..37
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..19
      FunParam@5..11
        Ident@5..6 "a"
        Colon@6..7 ":"
        Emptyspace@7..8 " "
        TypeExpr@8..11
          IdentExpr@8..11
            Ident@8..11 "int"
      Comma@11..12 ","
      Emptyspace@12..13 " "
      FunParam@13..19
        Ident@13..14 "b"
        Colon@14..15 ":"
        Emptyspace@15..16 " "
        TypeExpr@16..19
          IdentExpr@16..19
            Ident@16..19 "int"
    RParen@19..20 ")"
    Emptyspace@20..21 " "
    Arrow@21..23 "->"
    Emptyspace@23..24 " "
    TypeExpr@24..28
      IdentExpr@24..28
        Ident@24..27 "int"
        Emptyspace@27..28 " "
    FunBody@28..37
      BlockExpr@28..37
        LBrace@28..29 "{"
        Emptyspace@29..30 " "
        InfixExpr@30..36
          IdentExpr@30..32
            Ident@30..31 "a"
            Emptyspace@31..32 " "
          Plus@32..33 "+"
          Emptyspace@33..34 " "
          IdentExpr@34..36
            Ident@34..35 "b"
            Emptyspace@35..36 " "
        RBrace@36..37 "}""#]],
        )
    }
}
