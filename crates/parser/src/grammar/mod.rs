mod expr;
mod stmt;

use crate::parser::marker::CompletedMarker;
use crate::parser::{ParseEntryPoint, Parser};
use crate::syntax::SyntaxKind;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    match p.entry_point {
        ParseEntryPoint::Root => {
            while !p.at_end() {
                stmt::parse_stmt(p);
            }
        }
        ParseEntryPoint::Expr => {
            expr::parse_expr(p);
        }
    };

    m.complete(p, SyntaxKind::Root)
}
