mod check;
mod context;
mod database;
mod infer;
mod types;

use std::fmt;

use context::{Context, Diagnostic};
pub use database::Database;
use la_arena::Idx;
use text_size::TextRange;
use types::Type;

struct TypeDiagnostic {
    pub variant: TypeDiagnosticVariant,
    pub range: TextRange,
}

enum TypeDiagnosticVariant {
    TypeMismatch { expected: Type, actual: Type },
    Undefined { name: String },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Name(String);

/// Fully-Qualified name of a identifier
/// TODO: handle nested scopes in "name"?
pub struct Fqn {
    pub module: Name,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    VariableDef(Idx<LocalDef>),
    Expr(Idx<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocalDef {
    /// Expression value assigned to the variable
    pub value: Idx<Expr>,

    /// Original AST parsed for the variable definition
    ast: ast::VariableDef,
}

// TODO: borrow the string from the AST or put it into an interner?
impl LocalDef {
    pub fn name(&self) -> String {
        self.ast
            .name()
            .expect("VariableDef to have a name")
            .text()
            .to_string()
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Empty,
    BoolLiteral(bool),
    FloatLiteral(f64),
    IntLiteral(i64),
    StringLiteral(String),
    Binary {
        op: BinaryOp,
        lhs: Idx<Expr>,
        lhs_type: Type,
        rhs: Idx<Expr>,
        rhs_type: Type,
    },
    Unary {
        op: UnaryOp,
        expr: Idx<Expr>,
        typ: Type,
    },
    Block {
        stmts: Vec<Idx<Stmt>>,
        // TODO: should we instead use the last index of stmts?
        // last_expr: Idx<Expr>,
        typ: Type,
    },
    // TODO: make this a Path instead with Vec<Segment> (Vec<String> or w/e)
    // so that it can handle `a`, `a.b`, `a.b.c`, etc.
    VariableRef {
        name: String,
        typ: Type,
    },
    Function {
        params: Vec<Idx<Expr>>, // names (or empty?)
        body: Idx<Expr>,        // Expr::Block ?

        return_type_annotation: Option<Idx<Expr>>, // type name
    },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    Path,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Exp => write!(f, "**"),
            BinaryOp::Path => write!(f, "."),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),

            // TODO: "not" or "!"  ?
            UnaryOp::Not => write!(f, "not "),
        }
    }
}

pub fn lower(ast: ast::Root) -> (Database, Vec<Diagnostic>) {
    let mut context = Context::default();

    for stmt in ast.stmts() {
        context.lower_stmt(stmt);
    }

    (context.database, context.diagnostics)
}
