mod validation;

use parser::{Parse, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextRange;

impl From<Parse> for Root {
    fn from(parse_tree: Parse) -> Self {
        Root::cast(parse_tree.syntax()).expect("a Root node")
    }
}

// TODO: investigate macros to reduce boilerplate
//
// Each node probably has a "cast" function
// Each node should have a "range" function for reporting diagnostics

// TODO: Make a function / trait / macro for this pattern
// .children_with_tokens()
// .filter_map(SyntaxElement::into_token)
// .find(|token| matches!(token.kind(), Foo | Bar | Baz))

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug)]
pub enum Stmt {
    Import(()),

    /// Expression
    Expr(Expr),
}

impl Stmt {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            _ => {
                let expr = node.children().find_map(Expr::cast)?;
                Self::Expr(expr)
            }
        };

        Some(result)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetBinding(SyntaxNode);

impl LetBinding {
    // TODO: Pattern rather than name (Ident)
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn type_annotation(&self) -> Option<TypeExpr> {
        self.0.children().find_map(TypeExpr::cast)
    }

    pub fn value(&self) -> Option<Expr> {
        // TODO: check this doesn't pick up pattern / destructuring
        self.0.children().find_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Block(Block),
    Binary(Binary),
    BoolLiteral(BoolLiteral),
    Call(Call),
    FloatLiteral(FloatLiteral),
    Function(Function),
    Ident(Ident),
    IntLiteral(IntLiteral),
    LetBinding(LetBinding),
    Loop(LoopExpr),
    Paren(ParenExpr),
    StringLiteral(StringLiteral),
    // TypeBinding(TypeBinding), // the full `type A = struct { ... }`
    TypeExpr(TypeExpr),
    Unary(Unary),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        Some(match node.kind() {
            SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::BoolExpr => Self::BoolLiteral(BoolLiteral(node)),
            SyntaxKind::Call => Self::Call(Call(node)),
            SyntaxKind::FloatExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::InfixExpr => Self::cast_binary(node),
            SyntaxKind::IntExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::LoopExpr => Self::Loop(LoopExpr(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::Path => Self::Ident(Ident(node)), // ???
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::StringExpr => Self::StringLiteral(StringLiteral(node)),
            _ => return None,
        })
    }

    pub fn range(self) -> TextRange {
        use Expr::*;
        match self {
            Block(e) => e.range(),
            Binary(e) => e.range(),
            BoolLiteral(e) => e.range(),
            Call(e) => e.range(),
            FloatLiteral(e) => e.range(),
            Function(e) => e.range(),
            Ident(e) => e.range(),
            IntLiteral(e) => e.range(),
            LetBinding(e) => e.range(),
            Loop(e) => e.range(),
            Paren(e) => e.range(),
            StringLiteral(e) => e.range(),
            TypeExpr(e) => e.range(),
            Unary(e) => e.range(),
        }
    }

    fn cast_binary(node: SyntaxNode) -> Self {
        use SyntaxKind::*;
        let is_function = node
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .any(|token| token.kind() == Arrow);

        if is_function {
            Self::Function(Function(node))
        } else {
            Self::Binary(Binary(node))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary(SyntaxNode);

impl Binary {
    /// Returns the left-hand side of the expression
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    /// Returns the right-hand side of the expression
    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        use SyntaxKind::*;
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), Plus | Dash | Star | Slash | Dot | Caret))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Block(SyntaxNode);

impl Block {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::BlockExpr).then_some(Self(node))
    }

    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }

    // TODO: Iterator ?
    pub fn stmts(&self) -> Vec<Stmt> {
        todo!()
    }

    pub fn last_expr(&self) -> Option<Expr> {
        let children = self.0.children();

        for child in children {
            println!("{child}")
        }

        None
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct BoolLiteral(SyntaxNode);

impl BoolLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::BoolExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Call(SyntaxNode);

impl Call {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Call).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn path(&self) -> Option<Path> {
        self.0.children().find_map(Path::cast)
    }

    pub fn call_args(&self) -> Option<CallArgs> {
        self.0.children().find_map(CallArgs::cast)
    }
}

#[derive(Debug, Clone)]
pub struct CallArgs(SyntaxNode);

impl CallArgs {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::CallArgs).then_some(Self(node))
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }
}

#[derive(Debug, Clone)]
pub struct FloatLiteral(SyntaxNode);

impl FloatLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FloatExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Function(SyntaxNode);

// TODO: what of these need to be Option ?
// prefer not to panic if possible, and create a proper diagnostic
impl Function {
    pub fn param_list(&self) -> FunParamList {
        self.0
            .children()
            .find_map(FunParamList::cast)
            .expect("function to have parameter list")
    }

    pub fn return_type(&self) -> Option<TypeExpr> {
        self.0.children().find_map(TypeExpr::cast)
    }

    pub fn body(&self) -> Block {
        self.0
            .children()
            .find_map(Block::cast)
            .expect("function to have a body")
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct FunParam(SyntaxNode);

impl FunParam {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FunParam).then_some(Self(node))
    }

    // descendants() is depth-first traversal
    // TODO: any cleanup here? We only want the Ident that's within the Call
    pub fn ident(&self) -> Option<Ident> {
        self.0
            .descendants()
            .find_map(Call::cast)
            .and_then(|node| node.0.descendants().find_map(Ident::cast))
    }

    pub fn type_expr(&self) -> Option<TypeExpr> {
        self.0.descendants().find_map(TypeExpr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct FunParamList(SyntaxNode);

impl FunParamList {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        matches!(node.kind(), ParenExpr | Ident).then_some(Self(node))
    }

    pub fn params(&self) -> impl Iterator<Item = FunParam> {
        self.0.children().filter_map(FunParam::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Ident(SyntaxNode);

impl Ident {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Ident).then_some(Self(node))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct IntLiteral(SyntaxNode);

impl IntLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::IntExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn value_as_string(&self) -> Option<String> {
        self.value().map(|token| String::from(token.text()))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct LoopExpr(SyntaxNode);

impl LoopExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::LoopExpr).then_some(Self(node))
    }

    pub fn block(&self) -> Option<Block> {
        self.0.children().find_map(Block::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Path(SyntaxNode);

impl Path {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Path).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn idents(&self) -> impl Iterator<Item = Ident> {
        self.0.descendants().filter_map(Ident::cast)
    }

    pub fn ident_tokens(&self) -> impl Iterator<Item = SyntaxToken> {
        self.idents().filter_map(|ident| ident.name())
    }

    pub fn ident_strings(&self) -> impl Iterator<Item = String> {
        self.ident_tokens().map(|token| String::from(token.text()))
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral(SyntaxNode);

impl StringLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::StringLiteral).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExpr(SyntaxNode);

impl TypeExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::TypeExpr).then_some(Self(node))
    }

    pub fn as_path(&self) -> Option<Path> {
        self.0.children().find_map(Path::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Unary(SyntaxNode);

impl Unary {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        use SyntaxKind::*;
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), Dash | Not))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    /// Asserts that the provided `Option` is `Some`
    /// and returns the unwrapped value.
    macro_rules! assert_some {
        ($value:expr) => {{
            assert!($value.is_some());
            $value.unwrap()
        }};
    }

    /// Asserts that the provided enum is the provided variant,
    /// and extracts the inner value.
    macro_rules! assert_matches {
        ($value:expr, $variant:path) => {{
            assert!(matches!($value, $variant(_)));

            if let $variant(x) = $value {
                x
            } else {
                unreachable!()
            }
        }};
    }

    fn parse_node(input: &str) -> SyntaxNode {
        parser::parse_expr(input).syntax()
    }

    fn parse_expr(input: &str) -> Expr {
        let root = Root::cast(parse_node(input)).unwrap();
        root.expr().expect("expected a top-level expression")
    }

    #[test]
    fn call_no_arguments() {
        let input = "my_func";
        let parsed = parse_expr(input);

        let call = assert_matches!(parsed, Expr::Call);

        let call_args = call.call_args();
        assert!(call_args.is_none());
    }

    #[test]
    fn call_empty_arg() {
        let input = "my_func ()";
        let parsed = parse_expr(input);

        let call = assert_matches!(parsed, Expr::Call);
        assert_eq!(
            call.path().unwrap().ident_strings().next().unwrap(),
            "my_func"
        );
        let call_args = assert_some!(call.call_args());

        assert_eq!(0, call_args.args().count());
    }

    #[test]
    fn call_one_arg() {
        let input = "my_func 1";
        let parsed = parse_expr(input);

        let call = assert_matches!(parsed, Expr::Call);
        let call_args = assert_some!(call.call_args());

        assert_eq!(1, call_args.args().count());
    }

    #[test]
    fn call_arguments() {
        let input = "my_func (1, 2)";
        let parsed = parse_expr(input);

        let call = assert_matches!(parsed, Expr::Call);
        let call_args = assert_some!(call.call_args());

        assert_eq!(2, call_args.args().count());
    }

    #[test]
    fn empty_function() {
        let input = "() -> { }";
        let parsed = parse_expr(input);
        let expected_param_list = None;
        let expected_return_type = None;

        let function = assert_matches!(parsed, Expr::Function);

        assert_eq!(function.param_list().params().next(), expected_param_list);
        assert_eq!(function.return_type(), expected_return_type);
    }

    #[test]
    fn nullary_function_with_return_type() {
        let input = "() -> Int { }";
        let parsed = parse_expr(input);
        let expected_param_list = None;
        let expected_return_type = "Int";

        let function = assert_matches!(parsed, Expr::Function);

        assert_eq!(function.param_list().params().next(), expected_param_list);

        let return_type = assert_some!(function
            .return_type()
            .and_then(|type_expr| type_expr.as_path())
            .and_then(|path| path.idents().next())
            .and_then(|ident| ident.name()));
        assert_eq!(return_type.text(), expected_return_type);
    }

    #[test]
    fn unary_function_with_explicit_param_type() {
        let input = "(a: A, b: B) -> { }";
        let parsed = parse_expr(input);

        let function = assert_matches!(parsed, Expr::Function);

        let param_list = function.param_list();
        let params = param_list.params();
        for param in params {
            dbg!(param.ident().unwrap().name().unwrap());
        }
    }

    #[test]
    fn add_int_and_function() {
        // not a valid expression by the type checker, but should still produce an AST
        let input = "1 + (() -> { })";
        let expected_lhs = "1";
        let expected_rhs_param_list = None;
        let expected_rhs_return_type = None;

        let parsed = parse_expr(input);

        let binary = assert_matches!(parsed, Expr::Binary);

        let lhs = assert_some!(binary.lhs());
        let lhs = assert_matches!(lhs, Expr::IntLiteral);
        let lhs = assert_some!(lhs.value_as_string());
        assert_eq!(expected_lhs, lhs);

        let rhs = assert_some!(binary.rhs());
        let rhs = assert_matches!(rhs, Expr::Paren);
        let rhs = assert_matches!(assert_some!(rhs.expr()), Expr::Function);
        assert_eq!(rhs.param_list().params().next(), expected_rhs_param_list);
        assert_eq!(rhs.return_type(), expected_rhs_return_type);
    }

    #[test]
    fn loop_empty_body() {
        let input = "loop { }";

        let parsed = parse_expr(input);

        let loop_expr = assert_matches!(parsed, Expr::Loop);
        let block = assert_some!(loop_expr.block());
        assert!(block.exprs().count() == 0);
    }
}
