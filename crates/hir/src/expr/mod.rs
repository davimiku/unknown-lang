mod display;

use std::fmt;

use la_arena::Idx;

use crate::interner::Key;
use crate::type_expr::TypeExpr;

#[derive(Default, Debug, PartialEq, Clone)]
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

    VarRef(VarRefExpr),

    UnresolvedVarRef {
        key: Key,
    },

    Path(PathExpr),

    IndexInt(IndexIntExpr),
    // IndexString(IndexStringExpr), // string literals, ex. for named tuples
    // Index(IndexExpr), // arbitrary expressions
    ///
    Function(FunctionExpr),

    /// Variable definition
    VarDef(VarDefExpr),

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
    pub(crate) fn variable_def(
        key: ValueSymbol,
        value: Idx<Expr>,
        type_annotation: Option<Idx<TypeExpr>>,
    ) -> Self {
        Self::VarDef(VarDefExpr {
            symbol: key,
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDefExpr {
    /// Unique symbol for this value in this module
    pub symbol: ValueSymbol,

    /// Expression value of the RHS of the assignment
    pub value: Idx<Expr>,

    /// Optional type annotation
    pub type_annotation: Option<Idx<TypeExpr>>,
}

/// Unique identifier for a symbol that lives in the "value" universe
///
/// Examples of symbols are variable names, function parameter names, etc.
///
/// See also [TypeSymbol] for the analog that lives in the "type" universe
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueSymbol {
    /// Unique id of this symbol within this module
    pub symbol_id: u32,

    /// Unique id of the module where this symbol resides
    pub module_id: u32,
}

impl ValueSymbol {
    pub fn new(module_id: u32, symbol_id: u32) -> Self {
        Self {
            module_id,
            symbol_id,
        }
    }
}

/// Reference to a variable that lives in the "value" universe
#[derive(Debug, PartialEq, Clone)]
pub struct VarRefExpr {
    /// Interned string of the variable name
    pub key: Key,

    /// Unique identifier for the value symbol
    pub symbol: ValueSymbol,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ArrayLiteralExpr {
    Empty,
    NonEmpty { elements: Vec<Idx<Expr>> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Binary expression
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Idx<Expr>,
    pub rhs: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockExpr {
    pub exprs: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpr {
    /// Expression that is being called as a function
    pub callee: Idx<Expr>,

    // FIXME: this is temporary until builtins are applied like other functions
    pub callee_path: String,

    /// Arguments that the function are applied to
    pub args: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpr {
    /// Parameters to the function
    pub params: Vec<FunctionParam>,

    /// Body of the function
    pub body: Idx<Expr>,

    /// Name of the function, if available
    pub name: Option<Key>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub name: ValueSymbol,

    pub annotation: Option<Idx<TypeExpr>>,
}

#[derive(Debug, PartialEq)]
pub struct ForInLoopStmt {
    pub array: Idx<Expr>,

    pub item: Idx<Expr>,

    pub block: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfExpr {
    /// Condition to check before branching
    pub condition: Idx<Expr>,

    /// Expression that is executed when the condition is true
    pub then_branch: Idx<Expr>,

    /// Expression that is executed when the condition is false
    pub else_branch: Option<Idx<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PathExpr {
    pub subject: Idx<Expr>,

    pub member: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
