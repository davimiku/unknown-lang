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
    // TODO: instead wrap in a pseudo `main` function?
    // or wrap in a "Module" kind of structure?
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

    LetBinding(LetBinding),

    If(IfExpr),
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

/// Asserts that the provided `Result` is `Ok`
/// and returns the unwrapped value.
macro_rules! assert_ok {
    ($value:expr) => {{
        assert!($value.is_ok());
        $value.unwrap()
    }};
}
pub(crate) use assert_ok;

/// Asserts that the provided `Result` is `Err`
/// and returns the unwrapped error.
macro_rules! assert_err {
    ($value:expr) => {{
        assert!($value.is_err());
        $value.unwrap_err()
    }};
}
pub(crate) use assert_err;

#[cfg(test)]
mod tests {
    use la_arena::{Arena, ArenaMap};

    use crate::{lower_from_input, typecheck::TypeCheckResults, BlockExpr, Expr, Type};

    // TODO: would be better if this checked the type of the tail expression
    // So input of `123` checks IntLiteral(123) -- the only expression
    // input of
    // ```
    // let a = "Hello"
    // a
    // ```
    // checks the tail expression `a` which is StringLiteral("Hello")
    fn check_typecheck_results(input: &str, expected_expr: Expr, expected_type: Type) {
        let mut arena = Arena::default();
        let mut expr_types = ArenaMap::default();
        let expected = arena.alloc(expected_expr);
        expr_types.insert(expected, expected_type.clone());

        // root gets wrapped in a Block (the implicit 'main' module)
        // TODO: return type won't always be Unit?
        let expected = arena.alloc(Expr::Block(BlockExpr {
            exprs: vec![expected],
        }));
        expr_types.insert(expected, expected_type);
        let expected = TypeCheckResults::with_expr_types(expr_types);

        let (_, context) = lower_from_input(input);
        let actual = context.typecheck_results;

        assert_eq!(expected, actual);
    }

    #[test]
    fn typecheck_int_literal() {
        let input = "1";

        check_typecheck_results(input, Expr::IntLiteral(1), Type::IntLiteral(1));
    }

    #[test]
    fn typecheck_string_literal() {
        let input = r#""Hello""#;

        check_typecheck_results(
            input,
            Expr::StringLiteral("Hello".to_string()),
            Type::StringLiteral("Hello".to_string()),
        )
    }
}
