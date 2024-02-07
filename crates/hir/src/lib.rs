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
pub use display::{display_root, ContextDisplay};
pub use expr::{
    ArrayLiteralExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, IfExpr, IndexIntExpr,
    IntrinsicExpr, UnaryExpr, UnaryOp, ValueSymbol, VarDefExpr, VarRefExpr,
};
pub use lowering_context::{Context, COMPILER_BRAND};
pub use typecheck::{ArrayType, FunctionType, Type};

use database::Database;

pub use interner::{Interner, Key};
use la_arena::Idx;
use type_expr::TypeExpr;

/// In "module mode", which is the normal/default, lowers a module that potentially
/// contains many top-level functions and const expressions.
fn lower_module(ast: &ast::Root, mut context: Context) -> (Idx<Expr>, Context) {
    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    let module = Expr::Module(exprs);
    let module = context.alloc_expr(module, None);

    context.type_check(module, context.type_database.top());

    (module, context)
}

/// In "function mode", the only top-level expression is a function expression
fn lower_function(ast: &ast::Root, mut context: Context) -> (Idx<Expr>, Context) {
    let ast_function = ast
        .exprs()
        .next()
        .expect("the first expression to be the function to lower");

    // FIXME: assert that there is only 1 top-level expression? (the function)

    let function_idx = context.lower_expr(Some(ast_function));
    let function = context.expr(function_idx);

    assert!(matches!(function, Expr::Function(_)));

    context.type_check(function_idx, context.type_database.top());

    (function_idx, context)
}

/// In "script mode", wrap the list of expressions in the script into a synthetic
/// "main" function
fn lower_script(ast: &ast::Root, mut context: Context) -> (Idx<Expr>, Context) {
    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    // FIXME: this doesn't seem to handle an empty script
    let body = Expr::Block(BlockExpr::NonEmpty { exprs });
    let body = context.alloc_expr(body, None);
    let function = Expr::Function(FunctionExpr {
        params: vec![], // FIXME: `args: Array String` for CLI scripts?
        body,
        name: Some(context.interner.intern("main")),
    });
    let function_idx = context.alloc_expr(function, None);

    context.type_check(function_idx, context.type_database.top());

    (function_idx, context)
}

pub fn lower(input: &str, target: LowerTarget) -> (Idx<Expr>, Context) {
    let parsed = parser::parse(input);
    if !parsed.errors().is_empty() {
        panic!("found errors while parsing");
    }

    let root = ast::Root::cast(parsed.syntax()).expect("valid Root node");

    let interner = Interner::default();
    let context = Context::new(interner);

    match target {
        LowerTarget::Function => lower_function(&root, context),
        LowerTarget::Module => lower_module(&root, context),
        LowerTarget::Script => lower_script(&root, context),
    }
}

pub enum LowerTarget {
    /// The AST to lower is a list of statements representing a module
    Module,

    /// The AST to lower is a list of statements for a "script", which is syntax
    /// sugar for an immediately executed "main" function
    Script,

    /// The AST to lower is for a single function
    Function,
}
