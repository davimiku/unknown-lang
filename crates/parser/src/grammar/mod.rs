mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::{ParseEntryPoint, Parser};
use crate::syntax::SyntaxKind;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    match p.entry_point {
        ParseEntryPoint::Root => {
            let m = p.start();
            while !p.at_end() {
                expr::parse_expr(p);
            }
            m.complete(p, SyntaxKind::Root)
        }
        ParseEntryPoint::ExprText => expr::parse_expr(p).expect("successfully parsed an Expr"),
    }
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
