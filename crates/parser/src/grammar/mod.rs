mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseEntryPoint;
#[cfg(test)]
use crate::parser::ParseError;
use crate::parser::Parser;

use crate::syntax::SyntaxKind;

// TODO: Remove this and all tests from AST that relies on these internal details
pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    match p.entry_point {
        ParseEntryPoint::Root => parse_root(p),
        ParseEntryPoint::ExprTest => expr::parse_expr(p).expect("successfully parsed an Expr"),
        ParseEntryPoint::TypeTest => {
            expr::test_parse_type_expr(p).expect("successfully parsed a type Expr")
        }
    }
}

fn parse_root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    while !p.at_end() {
        expr::parse_expr(p);
    }
    m.complete(p, SyntaxKind::Root)
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    use crate::parse;

    let parse = parse(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
fn check_error(input: &str, expected_tree: expect_test::Expect, expected_errors: Vec<ParseError>) {
    use crate::parse;

    let parse = parse(input);

    expected_tree.assert_eq(&parse.debug_tree());
    assert_eq!(parse.errors, expected_errors);
}

// Convenience function to test expression parsing directly
#[cfg(test)]
fn check_expr(input: &str, expected_tree: expect_test::Expect) {
    use crate::test_parse_expr;

    let parse = test_parse_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

// Convenience function to test type expression parsing directly.
#[cfg(test)]
fn check_type_expr(input: &str, expected_tree: expect_test::Expect) {
    use crate::test_parse_type_expr;

    let parse = test_parse_type_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::check;

    #[test]
    fn parse_empty() {
        check("", expect![[r#"Root@0..0"#]])
    }

    #[test]
    fn parse_multiple_string_literals() {
        check(
            r#""a1"
"b2"
"c3"
"d4""#,
            expect![[r#"
Root@0..19
  StringLiteralExpr@0..4
    StringLiteralExpr@0..4 "\"a1\""
  Newline@4..5
    Newline@4..5 "\n"
  StringLiteralExpr@5..9
    StringLiteralExpr@5..9 "\"b2\""
  Newline@9..10
    Newline@9..10 "\n"
  StringLiteralExpr@10..14
    StringLiteralExpr@10..14 "\"c3\""
  Newline@14..15
    Newline@14..15 "\n"
  StringLiteralExpr@15..19
    StringLiteralExpr@15..19 "\"d4\"""#]],
        )
    }

    #[test]
    fn parse_function_def_and_call() {
        let input = r#"let print_param = fun (a: String) -> { print a }
        print_param "Hello!""#;
        let expected = expect![[r#"
            Root@0..77
              LetBinding@0..48
                LetKw@0..3 "let"
                Emptyspace@3..4 " "
                Ident@4..16
                  Ident@4..15 "print_param"
                  Emptyspace@15..16 " "
                Equals@16..17 "="
                Emptyspace@17..18 " "
                FunExpr@18..48
                  FunKw@18..21 "fun"
                  Emptyspace@21..22 " "
                  FunParamList@22..34
                    LParen@22..23 "("
                    FunParam@23..32
                      Ident@23..24
                        Ident@23..24 "a"
                      Colon@24..25 ":"
                      Emptyspace@25..26 " "
                      TypeExpr@26..32
                        Ident@26..32
                          Ident@26..32 "String"
                    RParen@32..33 ")"
                    Emptyspace@33..34 " "
                  Arrow@34..36 "->"
                  Emptyspace@36..37 " "
                  FunBody@37..48
                    BlockExpr@37..48
                      LBrace@37..38 "{"
                      Emptyspace@38..39 " "
                      Call@39..47
                        PathExpr@39..45
                          Ident@39..45
                            Ident@39..44 "print"
                            Emptyspace@44..45 " "
                        CallArgs@45..47
                          PathExpr@45..47
                            Ident@45..47
                              Ident@45..46 "a"
                              Emptyspace@46..47 " "
                      RBrace@47..48 "}"
              Newline@48..57
                Newline@48..49 "\n"
                Emptyspace@49..57 "        "
              Call@57..77
                PathExpr@57..69
                  Ident@57..69
                    Ident@57..68 "print_param"
                    Emptyspace@68..69 " "
                CallArgs@69..77
                  StringLiteralExpr@69..77
                    StringLiteralExpr@69..77 "\"Hello!\"""#]];
        check(input, expected);
    }

    #[test]
    fn parse_plain_return() {
        let input = r#"return 1"#;
        let expected = expect![[r#"
Root@0..8
  ReturnStatement@0..8
    ReturnKw@0..6 "return"
    Emptyspace@6..7 " "
    IntLiteralExpr@7..8
      IntLiteral@7..8 "1""#]];

        check(input, expected)
    }
}
