mod context;
mod database;
mod fmt_expr;
mod interner;
mod name_res;
mod scope;
mod typecheck;

use std::fmt;

pub use context::{Context, Diagnostic};
use database::Database;
use interner::{Interner, Name};
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

    let typecheck_results = typecheck::check(root, &context);
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
    StringLiteral(String),

    /// Binary expression, ex. `a + b`, `c ** d`
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

    If(IfExpr),
}

/// Local definition
///
/// Defines a new variable in a given scope.
#[derive(Debug, PartialEq, Eq)]
pub struct LocalDef {
    key: LocalDefKey,

    /// Expression value assigned to the variable
    pub value: Idx<Expr>,

    /// Optional type annotation
    // TODO: TypeExpr not Expr
    type_annotation: Option<Idx<Expr>>,
}

impl LocalDef {
    pub(crate) fn name(&self) -> Name {
        self.key.name
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalDefKey {
    name: Name,

    /// Unique number for this Name within this Context
    idx: u32,
}

impl LocalDefKey {
    fn display(&self, interner: &Interner) -> String {
        format!("{}{}", interner.lookup(self.name), self.idx)
    }
}

impl From<(Name, u32)> for LocalDefKey {
    fn from(value: (Name, u32)) -> Self {
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
    Unresolved(Name),
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
    use ast::Root;
    use expect_test::expect;
    use la_arena::{Arena, ArenaMap, Idx};

    use crate::{fmt_expr::fmt_expr, typecheck::TypeCheckResults};

    use super::*;

    // TODO: probably make this the only check function and remove everything else
    // or have this function and make another function for lowering errors
    // TODO: use expect! macro for better test results?
    fn check_fmt(input: &str, expected: expect_test::Expect) {
        let root: Root = parser::parse(input).into();

        let (root_expr, context) = lower(root);

        let mut actual = String::new();
        fmt_expr(&mut actual, root_expr, &context, 0);

        expected.assert_eq(&actual);
    }

    #[test]
    fn fmt_int_literal() {
        let input = "1";
        let expected = expect![["
{
    1
}"]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_string_literal() {
        let input = r#""Hello""#;
        let expected = expect![[r#"
{
    "Hello"
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_local_def() {
        let input = "let a = 10";
        let expected = expect![[r#"
{
    a~0 : 10 = 10
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_local_ref() {
        let input = r#"
        let a = 10
        a
"#;
        let expected = expect![[r#"
{
    a~0 : 10 = 10
    a~0
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_multiple_local_def() {
        let input = r#"
        let a = 1
        let b = 2
"#;
        let expected = expect![[r#"
{
    a~0 : 1 = 1
    b~0 : 2 = 2
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_multiple_local_ref() {
        let input = r#"
        let a = 1
        let b = 2
        a
        b
"#;
        let expected = expect![[r#"
{
    a~0 : 1 = 1
    b~0 : 2 = 2
    a~0
    b~0
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_one_level_nested_scope() {
        let input = r#"
        let a = 0
        {
            let a = 10
            a
        }
        a
"#;
        let expected = expect![[r#"
{
    a~0 : 0 = 0
    {
        a~1 : 10 = 10
        a~1
    }
    a~0
}"#]];

        check_fmt(input, expected);
    }

    #[test]
    fn fmt_two_level_nested_scope() {
        let input = r#"
        let a = 0
        {
            a
            let a = 10
            {
                a
                let a = 20
                a
            }
            a
        }
        a
"#;
        let expected = expect![[r#"
{
    a~0 : 0 = 0
    {
        a~0
        a~1 : 10 = 10
        {
            a~1
            a~2 : 20 = 20
            a~2
        }
        a~1
    }
    a~0
}"#]];

        check_fmt(input, expected);
    }

    fn debug(input: &str) -> (Idx<Expr>, Context) {
        let parsed = parser::parse(input).syntax();
        let root = ast::Root::cast(parsed).expect("valid Root node");
        let mut context = Context::default();

        let exprs: Vec<Idx<Expr>> = root
            .exprs()
            .map(|expr| context.lower_expr(Some(expr)))
            .collect();

        // wrap everything in a block
        // TODO: instead wrap in a pseudo `main` function?
        // or wrap in a "Module" kind of structure?
        let root = Expr::Block(BlockExpr { exprs });
        let root = context.alloc_expr(root, None);

        dbg!(&context);

        (root, context)
    }

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

    #[test]
    #[ignore = "Not a real test"]
    fn debug_let_binding() {
        let input = "let x = 1";

        debug(input);
    }

    #[test]
    fn typecheck_let_binding() {
        let input = "let x = 1";

        check_typecheck_results(input, Expr::IntLiteral(1), Type::IntLiteral(1))
    }
}
