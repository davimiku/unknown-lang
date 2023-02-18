mod context;
mod database;
mod fmt_expr;
mod interner;
mod name_res;
mod scope;
mod typecheck;

#[cfg(test)]
mod tests;

use std::fmt;

pub use context::{Context, Diagnostic};
use database::Database;
use interner::{Interner, Key};
use la_arena::Idx;
pub use typecheck::Type;

pub fn lower(ast: ast::Root) -> (Idx<Expr>, Context) {
    let mut context = Context::default();

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr(Some(expr)))
        .collect();

    // wrap everything in a block
    // TODO: instead wrap in a pseudo `main` function?
    // or wrap in a "Module" kind of structure?
    let root = Expr::Block(BlockExpr { exprs });
    let root = context.alloc_expr(root, None);

    let typecheck_results = typecheck::check(root, &context.database, &mut context.interner);
    context.typecheck_results = typecheck_results;

    (root, context)
}

pub fn lower_from_input(input: &str) -> (Idx<Expr>, Context) {
    let parsed = parser::parse(input).syntax();
    let root = ast::Root::cast(parsed).expect("valid Root node");

    lower(root)
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A missing expression from the parse tree
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64), // TODO: shared definition of Float

    /// 32-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i32), // TODO: shared definition of Int

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(Key),

    /// Binary expression, ex. `a + b`, `c ^ d`
    Binary(BinaryExpr),

    /// Unary expression, ex. `-a`, `not b`
    Unary(UnaryExpr),

    /// Block expression. Contains other expressions, and the
    /// result is the evaluation of the final expression.
    Block(BlockExpr),

    Call(CallExpr),

    LocalRef(LocalRef),

    Function(FunctionExpr),

    LocalDef(LocalDef),

    // TODO: should If be a special case of Match?
    If(IfExpr),
}

impl Default for Expr {
    fn default() -> Self {
        Expr::Empty
    }
}

/// Local definition
///
/// Defines a new variable in a given scope.
#[derive(Debug, PartialEq, Eq)]
pub struct LocalDef {
    pub key: LocalDefKey,

    /// Expression value assigned to the variable
    pub value: Idx<Expr>,

    /// Optional type annotation
    // TODO: TypeExpr not Expr
    type_annotation: Option<Idx<Expr>>,
}

impl LocalDef {
    pub(crate) fn name(&self) -> Key {
        self.key.name
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalDefKey {
    name: Key,

    /// Unique number for this Name within this Context
    idx: u32,
}

impl LocalDefKey {
    fn display(&self, interner: &Interner) -> String {
        format!("{}{}", interner.lookup(self.name), self.idx)
    }
}

impl From<(Key, u32)> for LocalDefKey {
    fn from(value: (Key, u32)) -> Self {
        Self {
            name: value.0,
            idx: value.1,
        }
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
    pub exprs: Vec<Idx<Expr>>,
    // tail_expr: Option<Idx<Expr>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpr {
    // TODO: make this a Path instead with Vec<Segment> (Vec<String> or w/e)
    // so that it can handle `a`, `a.b`, `a.b.c`, etc.
    /// Qualified path that the function is bound to
    pub path: String,

    /// Arguments that the function are applied to
    pub args: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpr {
    pub params: Vec<Idx<Expr>>, // names (or empty?)
    pub body: Idx<Expr>,        // Expr::Block ?

    pub return_type_annotation: Option<Idx<Expr>>, // type name
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfExpr {
    /// Condition to check before branching
    pub condition: Idx<Expr>,

    /// Expression that is executed when the condition is true
    pub then_branch: Idx<Expr>,

    /// Expression that is executed when the condition is false
    pub else_branch: Option<Idx<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct LocalRef {
    pub name: LocalRefName,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LocalRefName {
    // TODO: handle `a`, `a.b`, `a.b.c`, etc.
    Resolved(LocalDefKey),
    Unresolved(Key),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Concat,
    Rem,
    Exp,
    Path,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Concat => write!(f, "++"),
            Rem => write!(f, "%"),
            Exp => write!(f, "^"),
            Path => write!(f, "."),
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
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
