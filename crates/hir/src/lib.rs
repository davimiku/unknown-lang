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
    ArrayLiteralExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, IfExpr, IndexIntExpr,
    UnaryExpr, UnaryOp, ValueSymbol, VarDefExpr, VarRefExpr,
};
pub use lowering_context::{Context, COMPILER_BRAND};
pub use typecheck::{ArrayType, FunctionType, Type};

use database::Database;

pub use interner::Interner;
use interner::Key;
use la_arena::Idx;
use type_expr::TypeExpr;

fn lower_module(ast: &ast::Root) -> (Idx<Expr>, Context) {
    let interner = Interner::default();
    let mut context = Context::new(interner);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    let program = if exprs.is_empty() {
        Expr::Block(BlockExpr::Empty)
    } else {
        Expr::Block(BlockExpr::NonEmpty { exprs })
    };
    let program = context.alloc_expr(program, None);

    context.type_check(program, context.type_database.top());

    (program, context)
}

fn lower_function(ast: &ast::Root) -> (Idx<Expr>, Context) {
    let interner = Interner::default();
    let mut context = Context::new(interner);

    let ast_function = ast.exprs().next().unwrap();
    let function_idx = context.lower_expr(Some(ast_function));
    let function = context.expr(function_idx);

    assert!(matches!(function, Expr::Function(_)));

    (function_idx, context)
}

pub fn lower(input: &str, target: LowerTarget) -> (Idx<Expr>, Context) {
    let parsed = parser::parse(input);
    if !parsed.errors().is_empty() {
        panic!("found errors while parsing");
    }

    let root = ast::Root::cast(parsed.syntax()).expect("valid Root node");

    match target {
        LowerTarget::Function => lower_function(&root),
        LowerTarget::Module => lower_module(&root),
    }
}

pub enum LowerTarget {
    /// The AST to lower is for a single function
    Function,

    /// The AST to lower is a list of statements representing a module
    Module,
}
