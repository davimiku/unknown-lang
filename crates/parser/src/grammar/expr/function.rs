use lexer::TokenKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

pub(super) fn parse_fun_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Fun));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::LParen);
    parse_fun_param_list(p);
    p.expect(TokenKind::RParen);

    p.expect(TokenKind::Arrow);

    // optional return type parameter
    // this is an ident basically
    parse_fun_return_type(p);

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

    m.complete(p, SyntaxKind::ParamList)
}

fn parse_fun_param(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Colon);
    p.expect(TokenKind::Ident);

    m.complete(p, SyntaxKind::Param)
}

fn parse_fun_return_type(p: &mut Parser) -> CompletedMarker {
    todo!()
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
      ParamList@13..13
      RParen@13..14 ")"
      Emptyspace@14..15 " "
      Arrow@15..17 "->"
      Emptyspace@17..18 " "
      BlockExpr@18..20
        LBrace@18..19 "{"
        RBrace@19..20 "}""#]],
        );
    }

    #[test]
    fn parse_function_block_body() {
        check(
            r#"let x = fun () -> {
          let a = 1
          let b = 2
          a + b
        }"#,
            expect![[r#"
Root@0..85
  VariableDef@0..85
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "x"
    Emptyspace@5..6 " "
    Equals@6..7 "="
    Emptyspace@7..8 " "
    FunExpr@8..85
      Fun@8..11 "fun"
      Emptyspace@11..12 " "
      LParen@12..13 "("
      ParamList@13..13
      RParen@13..14 ")"
      Emptyspace@14..15 " "
      Arrow@15..17 "->"
      Emptyspace@17..18 " "
      BlockExpr@18..85
        LBrace@18..19 "{"
        Newline@19..20 "\n"
        Emptyspace@20..30 "          "
        VariableDef@30..39
          Let@30..33 "let"
          Emptyspace@33..34 " "
          Ident@34..35 "a"
          Emptyspace@35..36 " "
          Equals@36..37 "="
          Emptyspace@37..38 " "
          IntExpr@38..39
            Int@38..39 "1"
        Newline@39..40 "\n"
        Emptyspace@40..50 "          "
        VariableDef@50..59
          Let@50..53 "let"
          Emptyspace@53..54 " "
          Ident@54..55 "b"
          Emptyspace@55..56 " "
          Equals@56..57 "="
          Emptyspace@57..58 " "
          IntExpr@58..59
            Int@58..59 "2"
        Newline@59..60 "\n"
        Emptyspace@60..70 "          "
        InfixExpr@70..75
          VariableRef@70..72
            Ident@70..71 "a"
            Emptyspace@71..72 " "
          Plus@72..73 "+"
          Emptyspace@73..74 " "
          VariableRef@74..75
            Ident@74..75 "b"
        Newline@75..76 "\n"
        Emptyspace@76..84 "        "
        RBrace@84..85 "}""#]],
        )
    }

    #[test]
    fn parse_function_two_params() {
        check(
            "let add = fun (a: int, b: int,) -> int { a + b }",
            expect![[r#"
Root@0..40
  VariableDef@0..40
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..7 "add"
    Emptyspace@7..8 " "
    Equals@8..9 "="
    Emptyspace@9..10 " "
    FunExpr@10..40
      Fun@10..13 "fun"
      Emptyspace@13..14 " "
      LParen@14..15 "("
      ParamList@15..30
        Param@15..21
          Ident@15..16 "a"
          Colon@16..17 ":"
          Emptyspace@17..18 " "
          Ident@18..21 "int"
        Comma@21..22 ","
        Emptyspace@22..23 " "
        Param@23..29
          Ident@23..24 "b"
          Colon@24..25 ":"
          Emptyspace@25..26 " "
          Ident@26..29 "int"
        Comma@29..30 ","
      RParen@30..31 ")"
      Emptyspace@31..32 " "
      Arrow@32..34 "->"
      Emptyspace@34..35 " "
      InfixExpr@35..40
        VariableRef@35..37
          Ident@35..36 "a"
          Emptyspace@36..37 " "
        Plus@37..38 "+"
        Emptyspace@38..39 " "
        VariableRef@39..40
          Ident@39..40 "b""#]],
        )
    }

    #[test]
    fn parse_function_return_type() {
        check("let one = fun () -> int { 1 }", expect![[r#""#]])
    }
}
