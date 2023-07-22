use std::fmt;

use la_arena::Idx;

use crate::interner::{Interner, Key};
use crate::type_expr::TypeExpr;
use crate::COMPILER_BRAND;

#[derive(Default, Debug, PartialEq)]
pub enum Expr {
    /// A missing expression from the parse tree
    #[default]
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64),

    /// 64-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i64),

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(Key),

    /// Array literal value, ex. `[1, 2, 3]`
    ArrayLiteral(ArrayLiteralExpr),

    /// Binary expression, ex. `a + b`, `c ^ d`
    Binary(BinaryExpr),

    /// Unary expression, ex. `-a`, `!b`
    Unary(UnaryExpr),

    /// Block expression. Contains other expressions, and the
    /// result is the evaluation of the final expression.
    Block(BlockExpr),

    /// Empty block expression. Always returns Unit because there
    /// are no expressions contained within.
    EmptyBlock,

    Call(CallExpr),

    LocalRef(LocalRefExpr),

    UnresolvedLocalRef {
        key: Key,
    },

    Path(PathExpr),

    IndexInt(IndexIntExpr),
    // IndexString(IndexStringExpr), // string literals, ex. for named tuples
    // Index(IndexExpr), // arbitrary expressions
    ///
    Function(FunctionExpr),

    LocalDef(LocalDefExpr),

    // TODO: should be Match?
    If(IfExpr),

    /// "Expression statement", an expression that the return value
    /// is unused
    Statement(Idx<Expr>),

    /// "Return statement" doesn't produce a value itself. It mutates
    /// the runtime state (stack, call frame)
    ReturnStatement(Idx<Expr>),
}

// convenience constructors
// TODO: may remove
impl Expr {
    pub(crate) fn local_def(
        key: LocalDefKey,
        value: Idx<Expr>,
        type_annotation: Option<Idx<TypeExpr>>,
    ) -> Self {
        Self::LocalDef(LocalDefExpr {
            key,
            value,
            type_annotation,
        })
    }

    pub(crate) fn call(callee: Idx<Expr>, callee_path: String, args: Vec<Idx<Expr>>) -> Self {
        Self::Call(CallExpr {
            callee,
            callee_path,
            args,
        })
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalDefKey {
    // name: Key,
    pub name: Key,

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
pub enum ArrayLiteralExpr {
    Empty,
    NonEmpty { elements: Vec<Idx<Expr>> },
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
    /// Parameters to the function
    pub params: Vec<FunctionParam>,

    /// Body of the function
    pub body: Idx<Expr>,

    /// Name of the function, if available
    pub name: Option<Key>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub name: LocalDefKey,

    pub ty: Option<Idx<TypeExpr>>,
}

#[derive(Debug, PartialEq)]
pub struct ForInLoopStmt {
    pub array: Idx<Expr>,

    pub item: Idx<Expr>,

    pub block: Idx<Expr>,
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

#[derive(Debug, PartialEq, Eq)]
pub struct PathExpr {
    pub subject: Idx<Expr>,

    pub member: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IndexIntExpr {
    pub subject: Idx<Expr>,

    pub index: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOp {
    /// Addition `+`
    Add,

    /// Subtraction `-`
    Sub,

    /// Multiplication `*`
    Mul,

    /// Division `/`
    Div,

    /// Concatenation `++`
    Concat,

    /// Remainder `%`
    Rem,

    /// Exponent `^`
    Exp,

    /// Path access operator `.`
    Path,

    /// Equality `==`
    Eq,

    /// Inequality `!=`
    Ne,
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
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    IntoString,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::IntoString => write!(f, "~"),
        }
    }
}
