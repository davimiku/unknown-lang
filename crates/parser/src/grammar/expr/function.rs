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
    fn parse_unary_function_with_explicit_param_type() {
        check_expr(
            "(a: A) -> { }",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    ParenExpr@0..7
      LParen@0..1 "("
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

    #[test]
    fn parse_empty_function() {
        check_expr(
            "fun () -> {}",
            expect![[r#"
Root@0..12
  FunExpr@0..12
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..5
    RParen@5..6 ")"
    Emptyspace@6..7 " "
    Arrow@7..9 "->"
    Emptyspace@9..10 " "
    FunBody@10..12
      BlockExpr@10..12
        LBrace@10..11 "{"
        RBrace@11..12 "}""#]],
        );
    }

    #[test]
    fn parse_function_block_body() {
        check_expr(
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
          Ident@26..28
            Ident@26..27 "a"
            Emptyspace@27..28 " "
          Equals@28..29 "="
          Emptyspace@29..30 " "
          IntExpr@30..31
            IntLiteral@30..31 "1"
        Newline@31..32 "\n"
        Emptyspace@32..42 "          "
        VariableDef@42..51
          Let@42..45 "let"
          Emptyspace@45..46 " "
          Ident@46..48
            Ident@46..47 "b"
            Emptyspace@47..48 " "
          Equals@48..49 "="
          Emptyspace@49..50 " "
          IntExpr@50..51
            IntLiteral@50..51 "2"
        Newline@51..52 "\n"
        Emptyspace@52..62 "          "
        ExprStmt@62..76
          InfixExpr@62..67
            Call@62..64
              Path@62..64
                Ident@62..64
                  Ident@62..63 "a"
                  Emptyspace@63..64 " "
            Plus@64..65 "+"
            Emptyspace@65..66 " "
            Call@66..67
              Path@66..67
                Ident@66..67
                  Ident@66..67 "b"
          Newline@67..68 "\n"
          Emptyspace@68..76 "        "
        RBrace@76..77 "}""#]],
        )
    }

    #[test]
    fn parse_function_return_type() {
        check_expr(
            "fun () -> Int { 1 }",
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
      Path@10..14
        Ident@10..14
          Ident@10..13 "Int"
          Emptyspace@13..14 " "
    FunBody@14..19
      BlockExpr@14..19
        LBrace@14..15 "{"
        Emptyspace@15..16 " "
        ExprStmt@16..18
          IntExpr@16..18
            IntLiteral@16..17 "1"
            Emptyspace@17..18 " "
        RBrace@18..19 "}""#]],
        )
    }

    #[test]
    fn parse_function_one_param() {
        check_expr(
            "fun (a: Int) -> Int { a + 1 }",
            expect![[r#"
Root@0..29
  FunExpr@0..29
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..11
      FunParam@5..11
        Ident@5..6
          Ident@5..6 "a"
        Colon@6..7 ":"
        Emptyspace@7..8 " "
        TypeExpr@8..11
          Path@8..11
            Ident@8..11
              Ident@8..11 "Int"
    RParen@11..12 ")"
    Emptyspace@12..13 " "
    Arrow@13..15 "->"
    Emptyspace@15..16 " "
    TypeExpr@16..20
      Path@16..20
        Ident@16..20
          Ident@16..19 "Int"
          Emptyspace@19..20 " "
    FunBody@20..29
      BlockExpr@20..29
        LBrace@20..21 "{"
        Emptyspace@21..22 " "
        ExprStmt@22..28
          InfixExpr@22..28
            Call@22..24
              Path@22..24
                Ident@22..24
                  Ident@22..23 "a"
                  Emptyspace@23..24 " "
            Plus@24..25 "+"
            Emptyspace@25..26 " "
            IntExpr@26..28
              IntLiteral@26..27 "1"
              Emptyspace@27..28 " "
        RBrace@28..29 "}""#]],
        )
    }

    #[test]
    fn parse_function_two_params() {
        check_expr(
            "fun (a: Int, b: Int) -> Int { a + b }",
            expect![[r#"
Root@0..37
  FunExpr@0..37
    Fun@0..3 "fun"
    Emptyspace@3..4 " "
    LParen@4..5 "("
    FunParamList@5..19
      FunParam@5..11
        Ident@5..6
          Ident@5..6 "a"
        Colon@6..7 ":"
        Emptyspace@7..8 " "
        TypeExpr@8..11
          Path@8..11
            Ident@8..11
              Ident@8..11 "Int"
      Comma@11..12 ","
      Emptyspace@12..13 " "
      FunParam@13..19
        Ident@13..14
          Ident@13..14 "b"
        Colon@14..15 ":"
        Emptyspace@15..16 " "
        TypeExpr@16..19
          Path@16..19
            Ident@16..19
              Ident@16..19 "Int"
    RParen@19..20 ")"
    Emptyspace@20..21 " "
    Arrow@21..23 "->"
    Emptyspace@23..24 " "
    TypeExpr@24..28
      Path@24..28
        Ident@24..28
          Ident@24..27 "Int"
          Emptyspace@27..28 " "
    FunBody@28..37
      BlockExpr@28..37
        LBrace@28..29 "{"
        Emptyspace@29..30 " "
        ExprStmt@30..36
          InfixExpr@30..36
            Call@30..32
              Path@30..32
                Ident@30..32
                  Ident@30..31 "a"
                  Emptyspace@31..32 " "
            Plus@32..33 "+"
            Emptyspace@33..34 " "
            Call@34..36
              Path@34..36
                Ident@34..36
                  Ident@34..35 "b"
                  Emptyspace@35..36 " "
        RBrace@36..37 "}""#]],
        )
    }
}
