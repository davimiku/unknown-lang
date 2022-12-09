mod context;
mod database;
mod scope;
mod typecheck;

use std::fmt;

pub use context::{Context, Diagnostic};
use database::Database;
use la_arena::Idx;
pub use typecheck::Type;

pub fn lower(ast: ast::Root) -> (Vec<Idx<Stmt>>, Context) {
    let mut context = Context::new();

    let stmts: Vec<_> = ast
        .stmts()
        .filter_map(|stmt| context.lower_stmt(stmt))
        .collect();

    (stmts, context)
}

// TODO: interned string?
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
    // VariableDef(Idx<LetBinding>),
    Expr(Idx<Expr>),
}

/// Local definition (let binding)
///
/// ```txt
/// let x = a + 2
/// let y: Int = x * 7
/// ```
///
#[derive(Debug, PartialEq, Eq)]
pub struct LetBinding {
    /// Expression value assigned to the variable
    pub value: Idx<Expr>,

    /// Optional type annotation
    type_annotation: Option<Idx<Expr>>,

    /// Original AST parsed for the variable definition
    /// TODO: why? Create a hir::Pattern instead?
    ast: ast::LetBinding,
}

// TODO: borrow the string from the AST or put it into an interner?
impl LetBinding {
    pub fn name(&self) -> String {
        self.ast
            .name()
            .expect("LetBinding to have a name")
            .text()
            .to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
/// Binary expression
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Idx<Expr>,
    pub rhs: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockExpr {
    pub stmts: Vec<Idx<Stmt>>,
    // last_expr: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpr {
    // TODO: make this a Path instead with Vec<Segment> (Vec<String> or w/e)
    // so that it can handle `a`, `a.b`, `a.b.c`, etc.
    pub path: String,
    pub args: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A missing expression from the parse tree
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64),

    /// 64-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i64),

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(String),

    /// Binary expression, ex. `a + b`, `c ** d`
    Binary(BinaryExpr),

    /// Unary expression, ex. `-a`, `not b`
    Unary(UnaryExpr),

    /// Block expression. Contains other expressions, and the
    /// result is the evaluation of the final expression.
    ///
    /// {
    ///     let z = x + y
    ///     z / 2
    /// }
    Block(BlockExpr),

    Call(CallExpr),

    VariableRef {
        // TODO: make this a Path instead with Vec<Segment> (Vec<String> or w/e)
        // so that it can handle `a`, `a.b`, `a.b.c`, etc.
        name: String,
    },

    Function {
        params: Vec<Idx<Expr>>, // names (or empty?)
        body: Idx<Expr>,        // Expr::Block ?

        return_type_annotation: Option<Idx<Expr>>, // type name
    },

    // rename: ValueBinding?
    LetBinding(LetBinding),
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
