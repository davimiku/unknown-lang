mod database;
mod diagnostic;
mod display;
mod expr;
mod interner;
mod lowering_context;
mod scope;
mod type_expr;
mod typecheck;

#[cfg(test)]
mod tests;

pub use diagnostic::Diagnostic;
pub use display::display_root;
pub use expr::{
    ArrayLiteralExpr, BinaryExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, IfExpr,
    IndexIntExpr, UnaryExpr, UnaryOp, ValueSymbol, VarDefExpr, VarRefExpr,
};
pub use lowering_context::{Context, COMPILER_BRAND};
pub use typecheck::{ArrayType, FunctionType, Type};

use database::Database;

pub use interner::Interner;
use interner::Key;
use la_arena::Idx;
use type_expr::TypeExpr;

pub fn lower_ast<'a>(ast: &ast::Root, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let mut context = Context::new(interner);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    let program = Expr::Block(BlockExpr { exprs });
    let program = context.alloc_expr(program, None);

    context.type_check(program, context.type_database.top());

    (program, context)
}

// TODO: remove this, only used for codegen tests
pub fn lower_input<'a>(input: &str, interner: &'a mut Interner) -> (Idx<Expr>, Context<'a>) {
    let parsed = parser::parse(input);
    if !parsed.errors().is_empty() {
        panic!("found errors while parsing");
    }

    let root = ast::Root::cast(parsed.syntax()).expect("valid Root node");

    lower_ast(&root, interner)
}
