use lexer::TokenKind;

use crate::grammar::expr::{parse_block, parse_expr, parse_ident, parse_type};
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    use TokenKind as T;
    if p.at_end() {
        return None;
    }

    while p.at(T::Newline) {
        p.bump();
    }

    if p.at(T::Let) {
        return Some(parse_variable_def(p));
    } else if p.at(T::Type) {
        // parse_type_def
        todo!();
    } else if p.at(T::Module) {
        return Some(parse_module_def(p));
    }

    // start ExprStmt
    let m = p.start();

    loop {
        if p.at_set(&[T::Newline, T::RBrace]) || p.at_end() {
            break;
        }

        let cm = parse_expr(p);
        if cm.is_none() {
            break;
        }
    }

    while p.at(T::Newline) {
        p.bump();
    }

    let cm = m.complete(p, SyntaxKind::ExprStmt);

    Some(cm)
}

fn parse_variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Let));
    let m = p.start();
    p.bump();

    if !p.at(TokenKind::Equals) {
        // TODO: Pattern rather than Ident for destructuring
        parse_ident(p);

        if p.at(TokenKind::Colon) {
            p.bump();
            parse_type(p);
        }

        p.expect(TokenKind::Equals);
    } else {
        // invalid syntax, but parse gracefully anyways
        // lowering will report an error
        p.bump();
    }

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
    Ident@4..8
      Ident@4..7 "foo"
      Emptyspace@7..8 " "
    Equals@8..9 "="
    Emptyspace@9..10 " "
    Call@10..13
      Path@10..13
        Ident@10..13
          Ident@10..13 "bar""#]],
        );
    }

    #[test]
    fn parse_variable_with_type_annotation() {
        check(
            "let x: Int = 1",
            expect![[r#"
Root@0..14
  VariableDef@0..14
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..5
      Ident@4..5 "x"
    Colon@5..6 ":"
    Emptyspace@6..7 " "
    TypeExpr@7..11
      Path@7..11
        Ident@7..11
          Ident@7..10 "Int"
          Emptyspace@10..11 " "
    Equals@11..12 "="
    Emptyspace@12..13 " "
    IntExpr@13..14
      IntLiteral@13..14 "1""#]],
        )
    }

    #[test]
    fn parse_variable_without_identifier() {
        check(
            "let = 1",
            expect![[r#"
Root@0..7
  VariableDef@0..7
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Equals@4..5 "="
    Emptyspace@5..6 " "
    IntExpr@6..7
      IntLiteral@6..7 "1""#]],
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
    fn parse_function_call() {
        let input = "print a";
        check(
            input,
            expect![[r#"
Root@0..7
  ExprStmt@0..7
    Call@0..7
      Path@0..6
        Ident@0..6
          Ident@0..5 "print"
          Emptyspace@5..6 " "
      CallArgs@6..7
        Call@6..7
          Path@6..7
            Ident@6..7
              Ident@6..7 "a""#]],
        )
    }

    #[test]
    fn parse_module_definition() {
        check(
            "module a = { }",
            expect![[r#"
Root@0..14
  ModuleDef@0..14
    Module@0..6 "module"
    Emptyspace@6..7 " "
    Ident@7..8 "a"
    Emptyspace@8..9 " "
    Equals@9..10 "="
    Emptyspace@10..11 " "
    BlockExpr@11..14
      LBrace@11..12 "{"
      Emptyspace@12..13 " "
      RBrace@13..14 "}""#]],
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
    Path@16..17
      Ident@16..17 "a"
error at 8..11: expected Int, identifier, ‘-’ or ‘(’, but found ‘let’"#]],
        );
    }
}
