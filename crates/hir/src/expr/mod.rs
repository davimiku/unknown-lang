mod display;

use std::fmt;

use la_arena::Idx;

use crate::interner::Key;
use crate::lowering_context::CORE_MODULE_ID;
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

    /// Unary expression, ex. `-a`, `!b`
    // TODO: remove and use a Call instead (unary function call)
    Unary(UnaryExpr),

    /// Block expression. Contains other expressions, and the
    /// result is the evaluation of the final expression.
    Block(BlockExpr),

    Call(CallExpr),

    /// Name / reference to a variable that was resolved/found in
    /// the current scope.
    VarRef(VarRefExpr),

    /// Name of a variable that was not resolved/found in the current scope.
    UnresolvedVarRef {
        key: Key,
    },

    Path(PathExpr),

    IndexInt(IndexIntExpr),
    // IndexString(IndexStringExpr), // string literals, ex. for named tuples
    // Index(IndexExpr), // arbitrary expressions
    /// Function definition, including parameters and body.
    ///
    /// A function is inherently anonymous, but if created inside of a VarDef
    /// the variable name is captured.
    Function(FunctionExpr),

    /// Variable definition
    VarDef(VarDefExpr),

    /// Branch based on boolean condition, with possible "else" branch
    If(IfExpr),

    /// "Expression statement", an expression that the return value is unused
    Statement(Idx<Expr>),

    /// Returns the expression from the current function
    ReturnStatement(Idx<Expr>),

    /// Represents the container around a module
    Module(Vec<Idx<Expr>>),

    /// An expression that is only known to by the compiler
    Intrinsic(IntrinsicExpr),
}

// convenience constructors
// rustfmt has a habit of splitting struct initialization across multiple lines,
// even with property shorthand notation. I think often a single line is more readable
// especially when there's a bunch of other code to consider
impl Expr {
    pub(crate) fn variable_def(
        symbol: ValueSymbol,
        value: Idx<Expr>,
        type_annotation: Option<Idx<TypeExpr>>,
    ) -> Self {
        Self::VarDef(VarDefExpr {
            symbol,
            value,
            type_annotation,
        })
    }

    pub(crate) fn call(
        callee: Idx<Expr>,
        args: Box<[Idx<Expr>]>,
        symbol: Option<ValueSymbol>,
    ) -> Self {
        Self::Call(CallExpr {
            callee,
            args,
            symbol,
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
    /// Unique id of the module where this symbol resides
    module_id: u32,

    /// Unique id of this symbol within this module
    symbol_id: u32,
    // TODO: package id? or have a separate map between module_id and package_id ?
}

impl ValueSymbol {
    pub fn new(module_id: u32, symbol_id: u32) -> Self {
        Self {
            module_id,
            symbol_id,
        }
    }

    /// Symbol to represent the "main" function if it is synthetically generated,
    /// such as in "script mode".
    pub fn synthetic_main() -> Self {
        Self {
            module_id: 0,
            symbol_id: u32::MAX,
        }
    }

    pub fn in_core_module(&self) -> bool {
        self.module_id == CORE_MODULE_ID
    }
}

impl From<ValueSymbol> for (u32, u32) {
    fn from(value: ValueSymbol) -> Self {
        (value.module_id, value.symbol_id)
    }
}

/// Reference to a variable that lives in the "value" universe
#[derive(Debug, PartialEq, Clone)]
pub struct VarRefExpr {
    /// Unique identifier for the value symbol
    pub symbol: ValueSymbol,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ArrayLiteralExpr {
    Empty,
    NonEmpty { elements: Vec<Idx<Expr>> },
}

impl ArrayLiteralExpr {
    pub fn elements(&self) -> &[Idx<Expr>] {
        match self {
            Self::Empty => &[],
            Self::NonEmpty { elements } => elements,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockExpr {
    Empty,
    NonEmpty { exprs: Vec<Idx<Expr>> },
}

impl BlockExpr {
    pub fn exprs(&self) -> &[Idx<Expr>] {
        match self {
            BlockExpr::Empty => &[],
            BlockExpr::NonEmpty { exprs } => exprs,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpr {
    /// Expression that is being called as a function
    pub callee: Idx<Expr>,

    /// Arguments that the function are applied to
    pub args: Box<[Idx<Expr>]>,

    /// Symbol being called, if any. This would be None for anonymous function calls
    pub symbol: Option<ValueSymbol>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpr {
    /// Parameters to the function
    pub params: Vec<FunctionParam>,

    /// Body of the function
    pub body: Idx<Expr>,

    /// Name of the function, if available
    pub name: Option<(Key, ValueSymbol)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    /// Original name of the parameter
    pub name: Key,

    /// Unique identifier for this parameter
    pub symbol: ValueSymbol,

    /// Type annotation if provided
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IntrinsicExpr {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
        use BinaryOp as O;
        match self {
            O::Add => write!(f, "+"),
            O::Sub => write!(f, "-"),
            O::Mul => write!(f, "*"),
            O::Div => write!(f, "/"),
            O::Concat => write!(f, "++"),
            O::Rem => write!(f, "%"),
            O::Exp => write!(f, "^"),
            O::Path => write!(f, "."),
            O::Eq => write!(f, "=="),
            O::Ne => write!(f, "!="),
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
