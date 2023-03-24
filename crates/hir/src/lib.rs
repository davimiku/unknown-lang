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

pub use interner::Interner;
use interner::Key;
use la_arena::Idx;
use type_expr::TypeExpr;

pub fn lower<'a>(ast: &ast::Root, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let mut context = Context::new(interner);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr(Some(expr)))
        .collect();

    // wrap everything in a block
    // TODO: instead wrap in a pseudo `main` function?
    // or wrap in a "Module" kind of structure?
    let root = Expr::Block(BlockExpr { exprs });
    let root = context.alloc_expr(root, None);

    context.type_check(root, &Type::Top);

    (root, context)
}

pub fn lower_from_input<'a>(input: &str, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let parsed = parser::parse(input).syntax();
    let root = ast::Root::cast(parsed).expect("valid Root node");

    lower(&root, interner)
}
