use std::fmt;

use la_arena::Idx;

use crate::{
    interner::{Interner, Key},
    type_expr::TypeExpr,
    COMPILER_BRAND,
};

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A missing expression from the parse tree
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64),

    /// 32-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i64),

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(Key),

    /// Binary expression, ex. `a + b`, `c ^ d`
    Binary(BinaryExpr),

    /// Unary expression, ex. `-a`, `!b`
    Unary(UnaryExpr),

    /// Block expression. Contains other expressions, and the
    /// result is the evaluation of the final expression.
    Block(BlockExpr),

    Call(CallExpr),

    LocalRef(LocalRefExpr),

    UnresolvedLocalRef {
        key: Key,
    },

    Function(FunctionExpr),

    LocalDef(LocalDefExpr),

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
pub struct LocalDefExpr {
    pub key: LocalDefKey,

    /// Expression value assigned to the variable
    pub value: Idx<Expr>,

    /// Optional type annotation
    pub type_annotation: Option<Idx<TypeExpr>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalDefKey {
    name: Key,

    /// Unique number for this Name within this Context
    idx: u32,
}

impl LocalDefKey {
    pub fn display(&self, interner: &Interner) -> String {
        let name = interner.lookup(self.name);
        let idx = self.idx;
        format!("{name}{COMPILER_BRAND}{idx}",)
    }

    pub(crate) fn name(&self) -> Key {
        self.name
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

#[derive(Debug, PartialEq)]
pub struct LocalRefExpr {
    pub key: LocalDefKey,
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
    /// Expression that is being called as a function
    pub callee: Idx<Expr>,

    // FIXME: this is temporary until builtins are applied like other functions
    pub callee_path: String,

    /// Arguments that the function are applied to
    pub args: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpr {
    pub params: Vec<FunctionParam>,
    pub body: Idx<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub name: LocalDefKey,

    pub ty: Option<Idx<TypeExpr>>,
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
