pub mod expr;
mod type_expr;

#[cfg(test)]
mod tests;

pub use expr::Expr;
// TODO: don't export these individually, have the HIR use them as `ast::expr::T`
// motivation: can duplicate things in type_expr if necessary without renaming and
// don't need "Expr" suffix, it's clear from full name
pub use expr::{
    Binary, Block, Function, Ident, LetBinding, Mutability, PathExpr, Pattern, ReAssignment,
    TypeBinding, Unary,
};
pub use expr::{BreakStatement, ForInLoop, If, Loop, ReturnStatement};
pub use expr::{FloatLiteral, IntLiteral, ListLiteral, StringLiteral};
use parser::{Parse, SyntaxKind, SyntaxNode};
use text_size::TextRange;
pub use type_expr::{PathExpr as TypePathExpr, TypeExpr, Union};

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
