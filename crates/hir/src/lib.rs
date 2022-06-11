mod database;

use database::Context;
use la_arena::Idx;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VariableDef { name: String, value: Expr },
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Missing,
    IntLiteral(i64),
    Binary {
        op: BinaryOp,
        lhs: Idx<Expr>,
        rhs: Idx<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Idx<Expr>,
    },
    Block {
        stmts: Vec<Idx<Stmt>>,
        // TODO: should we just use the last index of stmts?
        // last_expr: Idx<Expr>,
    },
    VariableRef {
        name: String,
    },
    Function {
        // TODO:
        body: Box<Expr>, // Expr::Block
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
}

pub fn lower(ast: ast::Root) -> (Context, Vec<Stmt>) {
    let mut db = Context::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}
