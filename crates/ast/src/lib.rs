mod expr;
mod validation;

#[cfg(test)]
mod tests;

pub use expr::{
    Binary, Block, BoolLiteral, CallExpr, FloatLiteral, Function, If, IntLiteral, LetBinding, Loop,
    PathExpr, StringLiteral, Unary,
};
pub use expr::{Expr, TypeExpr};
use parser::{Parse, SyntaxKind, SyntaxNode};
use text_size::TextRange;

impl From<Parse> for Root {
    fn from(parse_tree: Parse) -> Self {
        Root::cast(parse_tree.syntax()).expect("a Root node")
    }
}

// TODO: investigate macros to reduce boilerplate
//
// Each node probably has a "cast" function
// Each node should have a "range" function for reporting diagnostics

// TODO: Make a function / trait / macro for this pattern
// .children_with_tokens()
// .filter_map(SyntaxElement::into_token)
// .find(|token| matches!(token.kind(), Foo | Bar | Baz))

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
