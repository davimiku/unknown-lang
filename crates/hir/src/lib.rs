mod database;
mod diagnostic;
mod display;
mod expr;
mod interner;
mod intrinsics;
mod lowering_context;
mod scope;
mod type_expr;
mod typecheck;

#[cfg(test)]
mod tests;

pub use ast::Mutability;
use ast::Root;
pub use diagnostic::Diagnostic;
pub use display::{display_root, ContextDisplay};
pub use expr::{
    ArrayLiteralExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, FunctionExprGroup,
    FunctionParam, IfExpr, IndexIntExpr, IntrinsicExpr, LoopExpr, MatchExpr, Pattern, ReAssignment,
    UnaryExpr, UnaryOp, ValueSymbol, VarDefExpr, VarRefExpr,
};
pub use lowering_context::{Context, COMPILER_BRAND};
pub use typecheck::{ArrayType, FuncSignature, FunctionType, Type};

use database::Database;

pub use interner::{Interner, Key};
use la_arena::Idx;
use type_expr::TypeExpr;

pub struct Module {
    pub id: u32,

    pub exprs: Box<[Idx<Expr>]>,
}

pub fn lower(input: &str) -> (Module, Context) {
    let (ast, mut context) = parse(input);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    let module = Module {
        exprs: exprs.into(),
        id: 1, // TODO: assign module_id at parse time and send it through the whole way
    };

    context.type_check_module(&module);

    (module, context)
}

/// In "function mode", the only top-level expression is a function expression
pub fn lower_function(input: &str) -> (Idx<Expr>, Context) {
    let (ast, mut context) = parse(input);

    let ast_function = ast
        .exprs()
        .next()
        .expect("the first expression to be the function to lower");

    let function_idx = context.lower_expr(Some(ast_function));
    let function = context.expr(function_idx);

    assert!(matches!(function, Expr::Function(_)));

    context.type_check(function_idx, context.core_types().top);

    (function_idx, context)
}

/// In "script mode", wrap the list of expressions in the script into a synthetic
/// "main" function
pub fn lower_script(input: &str) -> (Idx<Expr>, Context) {
    let (ast, mut context) = parse(input);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    // FIXME: this doesn't seem to handle an empty script
    let body = Expr::Block(BlockExpr::NonEmpty { exprs });
    let body = context.alloc_expr(body, None);
    let main_symbol = ValueSymbol::synthetic_main();
    let main_name = context.interner.intern("main");

    let params = Box::new([]);
    let return_type_annotation = None;
    let captures = Box::new([]);
    let function = Expr::Function(FunctionExprGroup {
        // FIXME: `args: Array String` for CLI scripts?
        overloads: Box::new([FunctionExpr {
            params,
            body,
            return_type_annotation,
            captures,
        }]),
        name: Some((main_name, main_symbol)),
        entry_point: Some(main_name),
    });
    let function_idx = context.alloc_expr(function, None);

    context.type_check(function_idx, context.core_types().top);

    (function_idx, context)
}

fn parse(input: &str) -> (Root, Context) {
    let parsed = parser::parse(input);
    assert_eq!(parsed.errors(), &[]);

    let root = ast::Root::cast(parsed.syntax()).expect("valid Root node");

    let interner = Interner::default();
    let context = Context::new(interner);

    (root, context)
}

#[derive(Clone)]
enum Color {
    Red,
    Green,
    Blue,
}

fn test() {
    let r = Color::Red;
    let Color = Color::Green;
    let Color = Color::Red;
    let Color = Color.clone();
}
