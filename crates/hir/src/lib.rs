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
    LocalDefKey, LocalRefExpr, UnaryExpr, UnaryOp,
};
use fmt_expr::fmt_root;
pub use typecheck::{ArrayType, FunctionType, Type};

use database::Database;

pub use interner::Interner;
use interner::Key;
use la_arena::Idx;
use type_expr::TypeExpr;

pub fn lower<'a>(ast: &ast::Root, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let mut context = Context::new(interner);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    let program = Expr::Block(BlockExpr { exprs });
    let program = context.alloc_expr(program, None);

    context.type_check(program, &Type::Top);

    (program, context)
}

pub fn lower_from_input<'a>(input: &str, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let parsed = parser::parse(input);
    if !parsed.errors().is_empty() {
        panic!("found errors while parsing");
    }

    let root = ast::Root::cast(parsed.syntax()).expect("valid Root node");

    lower(&root, interner)
}

pub fn fmt(ast: &ast::Root, interner: &mut Interner) -> String {
    let (root, context) = lower(ast, interner);

    fmt_root(root, &context)
}
