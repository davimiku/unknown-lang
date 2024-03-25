mod type_expr;

use std::fmt::{self, Display};

use parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextRange;

pub use type_expr::{PathExpr as TypePathExpr, TypeExpr};

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
    Ident(Ident),
    If(If),
    IntLiteral(IntLiteral),
    LetBinding(LetBinding),
    Loop(Loop),
    Paren(ParenExpr),
    Path(PathExpr),
    ReAssignment(ReAssignment),
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
            SyntaxKind::Ident => Self::Ident(Ident(node)),
            SyntaxKind::IfExpr => Self::If(If(node)),
            SyntaxKind::InfixExpr => cast_infix(node),
            SyntaxKind::IntLiteralExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::LetBinding => Self::LetBinding(LetBinding(node)),
            SyntaxKind::LoopExpr => Self::Loop(Loop(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::IntoStringExpr => Self::Unary(Unary(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::PathExpr => Self::Path(PathExpr(node)),
            SyntaxKind::ReturnStatement => Self::Return(ReturnStatement(node)),
            SyntaxKind::StringLiteralExpr => Self::StringLiteral(StringLiteral(node)),

            _ => return None,
        })
    }

    pub fn range(self) -> TextRange {
        use Expr as E;
        match self {
            E::ArrayLiteral(e) => e.range(),
            E::Block(e) => e.range(),
            E::Binary(e) => e.range(),
            E::BoolLiteral(e) => e.range(),
            E::Call(e) => e.range(),
            E::FloatLiteral(e) => e.range(),
            E::ForInLoop(e) => e.range(),
            E::Function(e) => e.range(),
            E::Ident(e) => e.range(),
            E::If(e) => e.range(),
            E::IntLiteral(e) => e.range(),
            E::LetBinding(e) => e.range(),
            E::Loop(e) => e.range(),
            E::Paren(e) => e.range(),
            E::Path(e) => e.range(),
            E::ReAssignment(e) => e.range(),
            E::Return(e) => e.range(),
            E::StringLiteral(e) => e.range(),
            E::Unary(e) => e.range(),
        }
    }
}

// FIXME: for ReAssign make a new kind of Expr
fn cast_infix(node: SyntaxNode) -> Expr {
    let binary = Binary(node.clone());
    if let Some(token) = binary.op() {
        match token.kind() {
            SyntaxKind::Dot => Expr::Path(PathExpr(node)),
            SyntaxKind::Equals => Expr::ReAssignment(ReAssignment(node)),
            _ => Expr::Binary(binary),
        }
    } else {
        Expr::Binary(binary)
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
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(Self::is_binop_token)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    fn is_binop_token(token: &SyntaxToken) -> bool {
        use SyntaxKind as S;

        matches!(
            token.kind(),
            S::Plus
                | S::PlusPlus
                | S::Dash
                | S::Star
                | S::Slash
                | S::Percent
                | S::Dot
                | S::Caret
                | S::Equals
                | S::EqualsEquals
                | S::BangEquals
                | S::LAngle
                | S::LAngleEquals
                | S::RAngle
                | S::RAngleEquals
        )
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

    pub fn value(&self) -> parser::SyntaxToken {
        self.0.first_token().expect("BoolLiteral to have a token")
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
    pub fn param_list(&self) -> FunctionParamList {
        self.0
            .children()
            .find_map(FunctionParamList::cast)
            .expect("function to have parameter list")
    }

    pub fn return_type(&self) -> Option<TypeExpr> {
        self.0.children().find_map(TypeExpr::cast)
    }

    pub fn body(&self) -> Option<FunctionBody> {
        self.0.children().find_map(FunctionBody::cast)
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
pub struct FunctionParam(SyntaxNode);

impl FunctionParam {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FunParam).then_some(Self(node))
    }

    // TODO: would be incorrect if a param was `: Int` ?
    // needs to be reslient and return None in that case
    // TODO: eventually may support destructuring (patterns!)
    // here and then it's no longer meaningful to get an "ident" here
    pub fn ident(&self) -> Option<Ident> {
        self.0.children().find_map(Ident::cast)
    }

    pub fn type_expr(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .find(|node| node.kind() == SyntaxKind::TypeExpr)
            .and_then(TypeExpr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParamList(SyntaxNode);

impl FunctionParamList {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FunParamList).then_some(Self(node))
    }

    pub fn params(&self) -> impl Iterator<Item = FunctionParam> {
        self.0.children().filter_map(FunctionParam::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBody(SyntaxNode);

impl FunctionBody {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::FunBody).then_some(Self(node))
    }

    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
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

    pub fn as_string(&self) -> String {
        self.0
            .first_token()
            .expect("parsed Ident to have a token")
            .text()
            .to_owned()
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

    pub fn mutability(&self) -> Mutability {
        match self
            .0
            .children_with_tokens()
            .find(|child| child.kind() == SyntaxKind::MutKw)
        {
            Some(_) => Mutability::Mut,
            None => Mutability::Not,
        }
    }

    pub fn type_annotation(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .find(|child| child.kind() == SyntaxKind::TypeExpr)
            .and_then(TypeExpr::cast)
    }

    pub fn value(&self) -> Option<Expr> {
        // TODO: better abstraction for "go until {thing}, then take the next thing"

        self.0
            .children_with_tokens()
            .skip_while(|child| match child.as_token() {
                Some(token) => token.kind() != SyntaxKind::Equals,
                None => true,
            })
            .skip(1) // consume the Equals
            .filter_map(SyntaxElement::into_node)
            .find_map(Expr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ReAssignment(SyntaxNode);

impl ReAssignment {
    /// Assignment "place", which may be an arbitrarily complex expression itself
    /// ```ignore
    ///    a = 4
    /// // ^
    ///    a.b = 4
    /// // ^^^
    ///    a().b = 4
    /// // ^^^^^
    /// ```
    pub fn place(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .take_while(|child| child.kind() != SyntaxKind::Equals)
            .filter_map(SyntaxElement::into_node)
            .find_map(Expr::cast)
    }

    pub fn value(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .skip_while(|child| match child.as_token() {
                Some(token) => token.kind() != SyntaxKind::Equals,
                None => true,
            })
            .skip(1) // consume the Equals
            .filter_map(SyntaxElement::into_node)
            .find_map(Expr::cast)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Not,
    Mut,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mutability::Not => f.write_str(""),
            Mutability::Mut => f.write_str("mut "),
        }
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

// TODO: rename to member expression?
// TODO: parameterize to have Expr/TypeExpr use the same struct?
#[derive(Debug, Clone)]
pub struct PathExpr(SyntaxNode);

impl PathExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::PathExpr).then_some(Self(node))
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    /// The expression to the left of the dot
    ///
    /// The subject is the noun that drives the action of the sentence, ex.
    /// "Karl runs", Karl is the subject. i.e. in `karl.runs`, karl is the subject.
    // FIXME: take only the first child before the Dot
    // user may be in the middle of typing `|.foo` ("|" is their cursor)
    pub fn subject(&self) -> Option<Expr> {
        self.0.first_child().and_then(Expr::cast)
    }

    pub fn subject_as_ident(&self) -> Option<Ident> {
        if let Some(Expr::Ident(ident)) = self.subject() {
            Some(ident)
        } else {
            None
        }
    }

    /// The expression to the right of the dot
    ///
    /// In `point.x`, "x" is the member
    pub fn member(&self) -> Option<Expr> {
        let mut take = false;
        // TODO: better abstraction for "go until {thing}, then take the next thing"
        for child in self.0.children_with_tokens() {
            if take {
                return child.into_node().and_then(Expr::cast);
            }
            if child.kind() == SyntaxKind::Dot {
                take = true;
            }
        }
        None
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
