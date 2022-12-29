mod context;
mod database;
mod scope;
mod typecheck;

use std::fmt;

pub use context::{Context, Diagnostic};
use database::Database;
use la_arena::Idx;
pub use typecheck::Type;

pub fn lower(ast: ast::Root) -> (Idx<Expr>, Context) {
    let mut context = Context::new();

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr(Some(expr)))
        .collect();

    // wrap everything in a block
    let root = Expr::Block(BlockExpr { exprs });
    let root = context.alloc_expr(root, None);

    let typecheck_results = typecheck::check(root, &context);
    context.typecheck_results = typecheck_results;

    (root, context)
}

pub fn lower_from_input(input: &str) -> (Idx<Expr>, Context) {
    let parsed = parser::parse(input).syntax();
    let root = ast::Root::cast(parsed).expect("valid Root node");

    lower(root)
}

// TODO: interned string?
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(String);

/// Fully-Qualified name of a identifier
/// TODO: handle nested scopes in "name"?
pub struct Fqn {
    pub module: Name,
    pub name: Name,
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
    pub exprs: Vec<Idx<Expr>>,
    // last_expr: Idx<Expr>,
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
