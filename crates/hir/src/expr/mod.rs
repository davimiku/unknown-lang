mod display;

use std::fmt;

use ast::Mutability;
use la_arena::Idx;
use text_size::TextRange;
use util_macros::assert_matches;

use crate::interner::Key;
use crate::lowering_context::CORE_MODULE_ID;
use crate::type_expr::TypeExpr;
use crate::{Context, Type};

/// HIR Expression
#[derive(Default, Debug, PartialEq, Clone)]
pub enum Expr {
    /// A missing expression from the parse tree
    #[default]
    Empty,

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

    /// The value representation of a union type
    ///
    /// ```
    /// type Color = red | green | blue
    /// let g = Color.green
    /// //      ^^^^^
    /// ```
    UnionNamespace(UnionNamespace),

    UnionVariant(UnionVariant),

    UnionUnitVariant(UnionUnitVariant),

    IndexInt(IndexIntExpr),

    /// Function definition, including parameters and body for each overload.
    ///
    /// A function is inherently anonymous, but if created inside of a VarDef
    /// the variable name is captured.
    Function(FunctionExprGroup),

    /// Variable definition
    VarDef(VarDefExpr),

    ReAssignment(ReAssignment),

    /// Branch based on the value of a "scrutinee", usually a union type
    Match(MatchExpr),

    /// Branch based on boolean condition, with possible "else" branch
    If(IfExpr),

    /// Loop expression
    Loop(LoopExpr),

    /// "Expression statement", an expression that the return value is unused
    Statement(Idx<Expr>),

    /// Breaks from the current loop
    BreakStatement(Idx<Expr>),

    /// Returns the expression from the current function
    ReturnStatement(Idx<Expr>),

    /// The whole "statement" at the type level
    ///
    /// i.e. `type Color = red | green | blue`
    TypeStatement(Idx<TypeExpr>),
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
            signature_index: None, // gets set later in type checking
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

impl VarDefExpr {
    pub fn is_mutable(&self, context: &Context) -> bool {
        let mutability = context.mutability_of(&self.symbol);

        mutability == Mutability::Mut
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReAssignment {
    /// Place being reassigned, such as `a`, `a.b`, or `a.b()`
    pub place: Idx<Expr>,

    /// Expression value of the RHS of the reassignment
    pub value: Idx<Expr>,
}

/// Unique identifier for a symbol that lives in the "value" universe
///
/// Examples of symbols are variable names, function parameter names, etc.
///
/// See also [TypeSymbol](crate::type_expr::TypeSymbol) for the analog that lives in the "type" universe
///
/// Get the original string content of this symbol using [Database](crate::database::Database).value_names
/// which gives a `Key` to lookup from the interner.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    // TODO: create a convenience constructor for anonymous functions?
    // pub fn synthetic_anonymous() -> Self { ... }

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
    ///
    /// This could be many different kinds of Expr, such as a variable, path,
    /// function literal, another CallExpr, etc.
    pub callee: Idx<Expr>,

    /// Arguments that the function are applied to
    pub args: Box<[Idx<Expr>]>,

    /// Symbol being called, if any. This would be None for anonymous function calls
    pub symbol: Option<ValueSymbol>,

    pub signature_index: Option<u32>,
}

impl CallExpr {
    pub fn return_ty_idx(&self, context: &Context) -> Idx<Type> {
        let func_ty = context.expr_type(self.callee);
        let func_ty = assert_matches!(func_ty, Type::Function);
        if let Some(sig_index) = self.signature_index {
            let sig = &func_ty.signatures[sig_index as usize];
            sig.return_ty
        } else {
            context.core_types().error
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExprGroup {
    /// Function expression for each overload
    pub overloads: Box<[FunctionExpr]>,

    /// Name of the function, if available
    pub name: Option<(Key, ValueSymbol)>,

    pub entry_point: Option<Key>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpr {
    /// Parameters to the function
    pub params: Box<[FunctionParam]>,

    /// Body of the function
    pub body: Idx<Expr>,

    /// Explicit return type annotation, if provided
    pub return_type_annotation: Option<Idx<TypeExpr>>,

    /// Symbols captured from an outer scopes
    pub captures: Box<[ValueSymbol]>,
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

#[derive(Debug)]
// TODO: temporary / placeholder name as its still being discovered what
// this needs to be
// TODO: this could be done in the MIR, or better here in HIR?
pub enum FunctionKind {
    /// Host function defined in Rust
    ///
    /// Limitations: arguments must be passed by copy?
    /// can arguments be moved once resources are implemented?
    /// what about shared / RC types like String?
    Host,

    /// Function being called is statically known
    ///
    /// ```ignore
    /// let f = fun () -> { ... }
    /// f ()
    /// ```
    ///
    /// The call to `f` can be substituted as a direct call and doesn't
    /// need to be tracked as a runtime variable.
    Direct,

    /// Function being called is statically known, and
    /// closes over values in an outer scope
    ///
    /// ```ignore
    /// let make_adder = fun (a: Int) -> {
    ///     let f = fun (b: Int) -> { a + b }
    ///     f
    /// }
    ///
    /// let add_four = make_adder 4
    /// add_four 8
    /// ```
    ///
    /// Calls to this would need to include the captured environment. The "Direct"
    /// variant can be seen as a specialization of this variant where the captured
    /// environment is zero-sized (no captures).
    DirectClosure,

    /// Function is being called without knowing which function until
    /// runtime.
    ///
    /// ```ignore
    /// let f = if condition { a } else { b }
    /// f ()
    /// ```
    ///
    /// This call can't be done by directly substituting a static call, the function
    /// being called needs to be a variable at runtime.
    ///
    /// TODO: perhaps "dynamic" is better than "indirect" (and "static" vs. "direct")
    Indirect,
    // TODO:
    // IndirectClosure,
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
pub struct LoopExpr {
    /// The body of the loop expression, which is a `BlockExpr`
    pub body: Idx<Expr>,

    /// The `break` expressions within this loop
    pub breaks: Vec<Idx<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchExpr {
    /// The scrutinee of the match expression is the value being tested
    pub scrutinee: Idx<Expr>,

    /// The arms of the match expression
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,

    pub expr: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    /// The wildcard pattern, written as `_`
    Wild(PatternMeta),

    /// Pattern that creates a binding to a variable, such as
    /// a `let` binding with a simple variable name, or (?) matching
    /// a union variant
    Binding {
        meta: PatternMeta,
        binding: PatternBinding,
    },

    // Record/Tuple/Struct
    // PatA | PatB | PatC
    // Or(Vec<Pattern>)
    Path(PatternMeta),

    Literal(PatternMeta),
    // Slice

    // Range
}

impl Pattern {
    pub fn binding(ident: Key, symbol: Option<ValueSymbol>, range: TextRange) -> Self {
        let meta = PatternMeta { range };
        let binding = PatternBinding { ident, symbol };
        Self::Binding { meta, binding }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatternMeta {
    pub range: TextRange,
    // pub id ?? some kind of HirId like rustc?
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatternBinding {
    // `mut` or anything else
    // annotation: BindingAnnotation
    // hir_id ?
    pub ident: Key,
    pub symbol: Option<ValueSymbol>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PathExpr {
    pub subject: Idx<Expr>,

    pub member: Idx<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnionNamespace {
    /// Name of the union in the value namespace
    pub name: ValueSymbol,

    /// Names of the variants (value namespace) with their type annotations
    pub members: Vec<(Key, Idx<TypeExpr>)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnionVariant {
    /// Name of the variant
    pub name: Key,

    /// Index of this variant in the union
    pub index: u32,

    /// Value expression for the union namespace
    pub union_namespace: Idx<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnionUnitVariant {
    /// Name of the variant
    pub name: Key,

    /// Index of this variant in the union
    pub index: u32,

    /// Value expression for the union namespace
    pub union_namespace: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexIntExpr {
    pub subject: Idx<Expr>,

    pub index: Idx<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IntrinsicExpr {
    /// Addition `+`
    Add,

    /// Subtraction `-`
    Sub,

    /// Multiplication `*`
    Mul,

    /// Division `/`
    Div,

    /// Remainder `%`
    Rem,

    /// Concatenation `++`
    Concat,

    /// Equality `==`
    Eq,

    /// Not Equality `!=`
    Ne,

    /// Less Than `<`
    Lt,

    /// Less Than Or Equal `<=`
    Le,

    /// Greater Than `>`
    Gt,

    /// Greater Than Or Equal `>=`
    Ge,
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
