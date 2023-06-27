mod type_expr;

use parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextRange;

pub use type_expr::TypeExpr;

#[derive(Debug, Clone)]
pub enum Expr {
    ArrayLiteral(ArrayLiteral),
    Block(Block),
    Binary(Binary),
    BoolLiteral(BoolLiteral),
    Call(CallExpr),
    FloatLiteral(FloatLiteral),
    ForInLoop(ForInLoop),
    Function(Function),
    If(If),
    IntLiteral(IntLiteral),
    LetBinding(LetBinding),
    Loop(Loop),
    Paren(ParenExpr),
    Path(PathExpr),
    Return(ReturnStatement),
    StringLiteral(StringLiteral),
    // TypeBinding(TypeBinding), // the full `type A = struct { ... }`
    Unary(Unary),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        Some(match node.kind() {
            SyntaxKind::ArrayLiteral => Self::ArrayLiteral(ArrayLiteral(node)),
            SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::BoolLiteralExpr => Self::BoolLiteral(BoolLiteral(node)),
            SyntaxKind::Call => Self::Call(CallExpr(node)),
            // TODO: clean up code here
            SyntaxKind::ConditionExpr | SyntaxKind::ThenBranchExpr | SyntaxKind::ElseBranchExpr => {
                Expr::cast(node.first_child().expect("TODO: handle missing case?"))
                    .expect("contained castable expression")
            }
            SyntaxKind::FloatLiteralExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::FunExpr => Self::Function(Function(node)),
            SyntaxKind::IfExpr => Self::If(If(node)),
            SyntaxKind::InfixExpr => Self::Binary(Binary(node)),
            SyntaxKind::IntLiteralExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::LetBinding => Self::LetBinding(LetBinding(node)),
            SyntaxKind::LoopExpr => Self::Loop(Loop(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::IntoStringExpr => Self::Unary(Unary(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::Path => Self::Path(PathExpr(node)),
            SyntaxKind::ReturnStatement => Self::Return(ReturnStatement(node)),
            SyntaxKind::StringLiteralExpr => Self::StringLiteral(StringLiteral(node)),
            _ => return None,
        })
    }

    pub fn range(self) -> TextRange {
        use Expr::*;
        match self {
            ArrayLiteral(e) => e.range(),
            Block(e) => e.range(),
            Binary(e) => e.range(),
            BoolLiteral(e) => e.range(),
            Call(e) => e.range(),
            FloatLiteral(e) => e.range(),
            ForInLoop(e) => e.range(),
            Function(e) => e.range(),
            If(e) => e.range(),
            IntLiteral(e) => e.range(),
            LetBinding(e) => e.range(),
            Loop(e) => e.range(),
            Paren(e) => e.range(),
            Path(e) => e.range(),
            Return(e) => e.range(),
            StringLiteral(e) => e.range(),
            Unary(e) => e.range(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral(SyntaxNode);

impl ArrayLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::BlockExpr).then_some(Self(node))
    }

    pub fn items(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
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
            // TODO: Extract these binary op tokens into a more obvious or shared place
            .find(|token| {
                matches!(
                    token.kind(),
                    Plus | PlusPlus | Dash | Star | Slash | Dot | Caret | EqualsEquals | BangEquals
                )
            })
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
        (node.kind() == SyntaxKind::BoolLiteralExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr(SyntaxNode);

impl CallExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Call).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn callee(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn args(&self) -> Option<CallArgs> {
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

    pub fn type_args(&self) -> impl Iterator<Item = TypeExpr> {
        self.0.children().filter_map(TypeExpr::cast)
    }
}

#[derive(Debug, Clone)]
pub struct FloatLiteral(SyntaxNode);

impl FloatLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FloatLiteralExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ForInLoop(SyntaxNode);

impl ForInLoop {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::ForInLoop).then_some(Self(node))
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

    pub fn body(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .skip_while(|child| match child.as_token() {
                Some(token) => token.kind() != SyntaxKind::Arrow,
                None => true,
            })
            .skip(1) // consume the arrow
            .filter_map(SyntaxElement::into_node)
            .find_map(Expr::cast)
    }

    pub fn name(&self) -> Option<String> {
        self.0
            .parent()
            .and_then(LetBinding::cast)
            .and_then(|let_binding| let_binding.name())
            .map(|token| token.text().to_owned())
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunParam(SyntaxNode);

impl FunParam {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        match node.kind() {
            // (a: A, b: B) ->
            ParenExprItem => Some(Self(node)),

            // a ->
            Path => Some(Self(node)),

            // (a) ->
            Call => node
                .children()
                .find(|child| child.kind() == Path)
                .map(|_| Self(node)),

            _ => None,
        }
    }

    pub fn ident(&self) -> Option<String> {
        self.0
            .descendants()
            .find_map(PathExpr::cast)
            .and_then(|path| path.ident_strings().next())
    }

    pub fn type_expr(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .find(|child| child.kind() == SyntaxKind::TypeExpr)
            .and_then(TypeExpr::cast)
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

        matches!(node.kind(), ParenExpr | Path).then_some(Self(node))
    }

    pub fn params(&self) -> Box<dyn Iterator<Item = FunParam>> {
        if self.0.kind() == SyntaxKind::Path {
            let iter = self
                .0
                .parent()
                .unwrap()
                .children()
                .filter_map(FunParam::cast)
                .take(1);
            Box::new(iter)
        } else {
            let iter = self.0.children().filter_map(FunParam::cast);
            Box::new(iter)
        }
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct If(SyntaxNode);

impl If {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::IfExpr).then_some(Self(node))
    }

    pub fn condition_expr(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| node.kind() == SyntaxKind::ConditionExpr)
            .and_then(Expr::cast)
    }

    pub fn then_branch(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| node.kind() == SyntaxKind::ThenBranchExpr)
            .and_then(Expr::cast)
    }

    pub fn else_branch(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| node.kind() == SyntaxKind::ElseBranchExpr)
            .and_then(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct IntLiteral(SyntaxNode);

impl IntLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::IntLiteralExpr).then_some(Self(node))
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn as_string(&self) -> Option<String> {
        self.value().map(|token| String::from(token.text()))
    }

    pub fn as_i64(&self) -> Option<i64> {
        self.as_string().and_then(|s| s.parse().ok())
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetBinding(SyntaxNode);

impl LetBinding {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::LetBinding).then_some(Self(node))
    }

    // TODO: Pattern rather than name (Ident)
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children()
            .find(|child| child.kind() == SyntaxKind::Ident)
            .and_then(|ident| ident.first_token())
    }

    pub fn type_annotation(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .find(|child| child.kind() == SyntaxKind::TypeExpr)
            .and_then(TypeExpr::cast)
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
pub struct Loop(SyntaxNode);

impl Loop {
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
pub struct PathExpr(SyntaxNode);

impl PathExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Path).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn ident_tokens(&self) -> impl Iterator<Item = SyntaxToken> {
        self.0
            .children()
            .filter(|node| node.kind() == SyntaxKind::Ident)
            .filter_map(|ident| ident.first_token())
    }

    pub fn ident_strings(&self) -> impl Iterator<Item = String> {
        self.ident_tokens().map(|token| String::from(token.text()))
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement(SyntaxNode);

impl ReturnStatement {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::ReturnStatement).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn return_value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
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

    pub fn as_string(&self) -> Option<String> {
        self.value().map(|token| token.text().to_owned())
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
            .find(|token| matches!(token.kind(), Dash | Bang | Tilde))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
