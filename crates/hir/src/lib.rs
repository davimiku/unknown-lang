mod check;
mod database;
mod infer;
mod types;

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
    VariableDef(Idx<VariableDef>),
    Expr(Idx<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableDef {
    value: Idx<Expr>,
    ast: ast::VariableDef,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Missing,
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
    VariableRef {
        name: String,
        typ: Type,
    },
    Function {
        params: Vec<Expr>, // names (or missing)
        body: Box<Expr>,   // Expr::Block ?

        return_type_annotation: Option<Box<Expr>>, // type name
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


#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

pub fn lower(ast: ast::Root) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}
