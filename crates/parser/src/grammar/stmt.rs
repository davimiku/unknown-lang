use lexer::TokenKind;

use crate::grammar::expr::{parse_block, parse_expr, parse_type};
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at_end() {
        return None;
    }

    while p.at(TokenKind::Newline) {
        p.bump();
    }

    if p.at(TokenKind::Let) {
        return Some(parse_variable_def(p));
    }

    if p.at(TokenKind::Type) {
        // parse_type_def
        todo!();
    }

    if p.at(TokenKind::Module) {
        return Some(parse_module_def(p));
    }

    let cm = parse_expr(p)?;

    while p.at(TokenKind::Newline) {
        p.bump();
    }

    if p.at(TokenKind::RBrace) {
        return Some(cm);
    }

    let m = cm.precede(p);
    // expect something

    Some(m.complete(p, SyntaxKind::ExprStmt))
}

fn parse_variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Let));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);

    if p.at(TokenKind::Colon) {
        p.bump();
        parse_type(p);
    }

    p.expect(TokenKind::Equals);

    parse_expr(p);

    m.complete(p, SyntaxKind::VariableDef)
}

fn parse_module_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Module));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Equals);

    parse_block(p);

    m.complete(p, SyntaxKind::ModuleDef)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_empty() {
        check("", expect![[r#"Root@0..0"#]])
    }

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
Root@0..13
  VariableDef@0..13
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..7 "foo"
    Emptyspace@7..8 " "
    Equals@8..9 "="
    Emptyspace@9..10 " "
    NameRef@10..13
      Ident@10..13 "bar""#]],
        );
    }

    #[test]
    fn parse_variable_with_type_annotation() {
        check(
            "let x: int = 1",
            expect![[r#"
Root@0..14
  VariableDef@0..14
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "x"
    Colon@5..6 ":"
    Emptyspace@6..7 " "
    TypeExpr@7..11
      NameRef@7..11
        Ident@7..10 "int"
        Emptyspace@10..11 " "
    Equals@11..12 "="
    Emptyspace@12..13 " "
    IntExpr@13..14
      IntLiteral@13..14 "1""#]],
        )
    }

    #[test]
    fn parse_expr_statement() {
        check(
            "123",
            expect![[r#"
Root@0..3
  ExprStmt@0..3
    IntExpr@0..3
      IntLiteral@0..3 "123""#]],
        )
    }

    #[test]
    fn parse_module_definition() {
        check(
            "module turn_manager = { }",
            expect![[r#"
Root@0..25
  ModuleDef@0..25
    Module@0..6 "module"
    Emptyspace@6..7 " "
    Ident@7..19 "turn_manager"
    Emptyspace@19..20 " "
    Equals@20..21 "="
    Emptyspace@21..22 " "
    BlockExpr@22..25
      LBrace@22..23 "{"
      Emptyspace@23..24 " "
      RBrace@24..25 "}""#]],
        )
    }

    #[test]
    #[ignore = "Recovery not implemented yet"]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a",
            expect![[r#"
Root@0..17
  VariableDef@0..8
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5 "a"
    Emptyspace@5..6 " "
    Equals@6..7 "="
    Error@7..8
      Error@7..8 "\n"
  VariableDef@8..17
    Let@8..11 "let"
    Emptyspace@11..12 " "
    Ident@12..13 "b"
    Emptyspace@13..14 " "
    Equals@14..15 "="
    Emptyspace@15..16 " "
    NameRef@16..17
      Ident@16..17 "a"
error at 8..11: expected int, identifier, ‘-’ or ‘(’, but found ‘let’"#]],
        );
    }
}
