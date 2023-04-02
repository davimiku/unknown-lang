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
use fmt_expr::fmt_root;
pub use typecheck::{FunctionType, Type};

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

    let main_body = Expr::Block(BlockExpr { exprs });
    let main_body = context.alloc_expr(main_body, None);

    let main = Expr::Function(FunctionExpr {
        params: vec![],
        body: main_body,
    });
    let main = context.alloc_expr(main, None);

    context.type_check(
        main,
        &Type::Function(FunctionType {
            params: vec![],
            return_ty: Box::new(Type::Top),
        }),
    );

    (main, context)
}

pub fn lower_from_input<'a>(input: &str, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let parsed = parser::parse(input).syntax();
    let root = ast::Root::cast(parsed).expect("valid Root node");

    lower(&root, interner)
}

pub fn fmt(ast: &ast::Root, interner: &'_ mut Interner) -> String {
    let (root, context) = lower(ast, interner);

    fmt_root(root, &context)
}
