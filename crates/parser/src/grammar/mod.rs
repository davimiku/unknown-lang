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
