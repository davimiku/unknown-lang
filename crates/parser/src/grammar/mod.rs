mod expr;

use crate::parser::marker::CompletedMarker;
#[cfg(test)]
use crate::parser::ParseEntryPoint;
use crate::parser::Parser;

use crate::syntax::SyntaxKind;

#[cfg(not(test))]
pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    parse_root(p)
}

#[cfg(test)]
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
}
