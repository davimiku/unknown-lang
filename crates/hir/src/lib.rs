mod context;
mod database;
mod expr;
mod fmt_expr;
mod interner;
mod name_res;
mod scope;
mod type_expr;
mod typecheck;

#[cfg(test)]
mod tests;

pub use context::{Context, Diagnostic, COMPILER_BRAND};
pub use expr::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, IfExpr, LocalDefExpr,
    LocalDefKey, LocalRefExpr, LocalRefName, UnaryExpr, UnaryOp,
};
pub use typecheck::Type;

use database::Database;

use interner::Key;
use la_arena::Idx;
use type_expr::TypeExpr;

pub fn lower(ast: ast::Root) -> (Idx<Expr>, Context) {
    let mut context = Context::default();

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr(Some(expr)))
        .collect();

    // wrap everything in a block
    // TODO: instead wrap in a pseudo `main` function?
    // or wrap in a "Module" kind of structure?
    let root = Expr::Block(BlockExpr { exprs });
    let root = context.alloc_expr(root, None);

    let typecheck_results = typecheck::check(root, &context.database, &mut context.interner);
    context.typecheck_results = typecheck_results;

    (root, context)
}

pub fn lower_from_input(input: &str) -> (Idx<Expr>, Context) {
    let parsed = parser::parse(input).syntax();
    let root = ast::Root::cast(parsed).expect("valid Root node");

    lower(root)
}
