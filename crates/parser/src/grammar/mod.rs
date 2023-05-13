mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseEntryPoint;
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
mod tests {
    use expect_test::expect;

    use crate::check;

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
  Newline@4..5 "\n"
  StringLiteralExpr@5..9
    StringLiteralExpr@5..9 "\"b2\""
  Newline@9..10 "\n"
  StringLiteralExpr@10..14
    StringLiteralExpr@10..14 "\"c3\""
  Newline@14..15 "\n"
  StringLiteralExpr@15..19
    StringLiteralExpr@15..19 "\"d4\"""#]],
        )
    }
}
