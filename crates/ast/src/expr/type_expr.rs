use parser::{SyntaxKind, SyntaxNode, SyntaxNodeExt};
use text_size::TextRange;

use super::{BoolLiteral, CallExpr, FloatLiteral, Ident, IntLiteral, StringLiteral};

#[derive(Debug, Clone)]
pub enum TypeExpr {
    // Binary(Binary), // parameterize to work on either Expr | TypeExpr
    BoolLiteral(BoolLiteral),
    FloatLiteral(FloatLiteral),
    Function(Function),
    Path(PathExpr),
    Call(CallExpr),
    Ident(Ident),
    // If(IfExpr), // todo
    IntLiteral(IntLiteral),
    // Paren(ParenExpr), // parameterize to work on either Expr | TypeExpr
    StringLiteral(StringLiteral),
    Union(Union),
    // Unary(Unary), // parameterize to work on either Expr | TypeExpr
}

impl TypeExpr {
    pub fn cast(mut node: SyntaxNode) -> Option<Self> {
        // being forgiving if the caller passed the "wrapper" TypeExpr
        // eventually remove and fix the callsites, but not urgent
        if node.kind() == SyntaxKind::TypeExpr {
            node = node.first_child().unwrap();
        }
        Some(match node.kind() {
            // SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::BoolLiteralExpr => Self::BoolLiteral(BoolLiteral(node)),
            SyntaxKind::Call => Self::Call(CallExpr(node)),
            SyntaxKind::FloatLiteralExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::Ident => Self::Ident(Ident(node)),
            SyntaxKind::InfixExpr => Self::cast_infix(node),
            SyntaxKind::IntLiteralExpr => Self::IntLiteral(IntLiteral(node)),
            // SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            // SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::PathExpr => Self::Path(PathExpr(node)),
            // SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::StringLiteralExpr => Self::StringLiteral(StringLiteral(node)),
            _ => return None,
        })
    }

    pub fn range(&self) -> TextRange {
        use TypeExpr as T;
        match self {
            // Binary(e) => e.range(),
            T::BoolLiteral(e) => e.range(),
            T::Call(e) => e.range(),
            T::FloatLiteral(e) => e.range(),
            T::Function(e) => e.range(),
            T::Ident(e) => e.range(),
            T::IntLiteral(e) => e.range(),
            T::Path(e) => e.range(),
            // If(e) => e.range(),
            // Paren(e) => e.range(),
            T::StringLiteral(e) => e.range(),
            T::Union(e) => e.range(),
            // Unary(e) => e.range(),
        }
    }

    fn cast_infix(node: SyntaxNode) -> Self {
        if node.has_child_of(SyntaxKind::Arrow) {
            Self::Function(Function(node))
        } else if node.has_child_of(SyntaxKind::Bar) {
            Self::Union(Union(node))
        } else {
            todo!()
            // Self::Binary(Binary(node))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function(SyntaxNode);

impl Function {
    pub fn param_list(&self) -> Vec<TypeExpr> {
        self.0.children().filter_map(TypeExpr::cast).collect()
    }

    pub fn return_type(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .skip_while(|child| child.kind() != SyntaxKind::Arrow)
            .skip(1) // consume the arrow
            .find_map(TypeExpr::cast)
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
    /// The expression to the left of the dot
    ///
    /// The subject is the noun that drives the action of the sentence, ex.
    /// "Karl runs", Karl is the subject. i.e. in `karl.runs`, karl is the subject.
    pub fn subject(&self) -> Option<TypeExpr> {
        self.0.first_child().and_then(TypeExpr::cast)
    }

    pub fn member(&self) -> Option<TypeExpr> {
        self.0
            .children()
            .by_ref()
            .take_while(|p| p.kind() == SyntaxKind::Dot)
            .next()
            .and_then(TypeExpr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Union(SyntaxNode);

impl Union {
    // flattens the (possibly) nested InfixExpr with Bar
    // operators into a list - see the parser tests/binding.rs
    // for an example of this structure
    pub fn variants(&self) -> Vec<CompoundTypeItem> {
        // TODO: code needs a real clean-up
        // idea is that the first variant will be Ident | CompoundTypeItem
        // subsequent variants will be after Bar and will be Ident | CompoundTypeItem | InfixExpr
        // if it is InfixExpr, should loop until it ends at Ident | CompoundTypeItem
        let first = self.0.children().find_map(CompoundTypeItem::cast).unwrap();
        let mut v = vec![first];
        let mut next = self
            .0
            .children_with_tokens()
            .skip_while(|child| child.kind() != SyntaxKind::Bar)
            .skip(1) // consume the `|`
            .find_map(|node_or_token| {
                let node = node_or_token.as_node();
                if let Some(node) = node {
                    if Self::is_variant(node) {
                        return Some(node.clone());
                    }
                }
                None
            })
            .unwrap();
        if let Some(item) = CompoundTypeItem::cast(next) {
            v.push(item);
            return v;
        }
        // loop {
        //     match node.children().find_map(CompoundTypeItem::cast) {
        //         Some(item) => v.push(item),
        //         None => break,
        //     }
        // }
        v
    }

    pub fn is_variant(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            SyntaxKind::Ident | SyntaxKind::CompoundTypeItem
        ) || node.has_child_of(SyntaxKind::Bar)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct CompoundTypeItem(SyntaxNode);

impl CompoundTypeItem {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Ident | SyntaxKind::CompoundTypeItem => Some(Self(node)),

            _ => None,
        }
    }

    pub fn ident_as_string(&self) -> String {
        match self.0.kind() {
            SyntaxKind::Ident => self
                .0
                .first_token()
                .expect("parsed Ident to have a token")
                .text()
                .to_owned(),

            SyntaxKind::CompoundTypeItem => self
                .0
                .children()
                .find(|node| node.kind() == SyntaxKind::Ident)
                .expect("CompoundTypeItem to have Ident")
                .first_token()
                .expect("Ident to have token")
                .to_string(),

            _ => unreachable!(),
        }
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
