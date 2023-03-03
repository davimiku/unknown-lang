#[cfg(test)]
mod tests;
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

    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
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
    If(IfExpr),
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
            // TODO: clean up code here
            SyntaxKind::ConditionExpr | SyntaxKind::ThenBranchExpr | SyntaxKind::ElseBranchExpr => {
                Expr::cast(node.first_child().expect("TODO: handle missing case?"))
                    .expect("contained castable expression")
            }
            SyntaxKind::FloatExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::IfExpr => Self::If(IfExpr(node)),
            SyntaxKind::InfixExpr => Self::cast_binary(node),
            SyntaxKind::IntExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::LetBinding => Self::LetBinding(LetBinding(node)),
            SyntaxKind::LoopExpr => Self::Loop(LoopExpr(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::Path => Self::Ident(Ident(node)), // is this ever constructed?
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
            If(e) => e.range(),
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
            .find(|token| {
                matches!(
                    token.kind(),
                    Plus | PlusPlus | Dash | Star | Slash | Dot | Caret
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

    // descendants() is depth-first traversal
    // TODO: any cleanup here? We only want the Ident that's within the Call
    pub fn ident(&self) -> Option<Ident> {
        self.0
            .descendants()
            .find_map(Path::cast)
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

        matches!(node.kind(), ParenExpr | Call | Ident).then_some(Self(node))
    }

    pub fn params(&self) -> impl Iterator<Item = FunParam> {
        self.0.children().filter_map(FunParam::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(SyntaxNode);

impl Ident {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::Ident).then_some(Self(node))
    }

    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }

    pub fn name(&self) -> Option<String> {
        self.name_token().map(|token| String::from(token.text()))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr(SyntaxNode);

impl IfExpr {
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
        self.idents().filter_map(|ident| ident.name_token())
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
            .find(|token| matches!(token.kind(), Dash | Bang))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
