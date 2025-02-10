use parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodeExt};
use text_size::TextRange;

use crate::expr::{CallExpr, FloatLiteral, Ident, IntLiteral, StringLiteral};

#[derive(Debug, Clone)]
pub enum TypeExpr {
    // Binary(Binary), // parameterize to work on either Expr | TypeExpr
    FloatLiteral(FloatLiteral),
    Function(Function),
    Path(PathExpr),
    Call(CallExpr),
    Ident(Ident),
    // If(IfExpr), // todo
    IntLiteral(IntLiteral),
    Paren(ParenExpr),
    StringLiteral(StringLiteral),
    Union__Old(Union__Old),
    Union(Union),
    // Unary(Unary), // parameterize to work on either Expr | TypeExpr
}

impl TypeExpr {
    pub fn cast(mut node: SyntaxNode) -> Option<Self> {
        // being forgiving if the caller passed the "wrapper" TypeExpr
        // eventually remove and fix the callsites, but not urgent
        if matches!(
            node.kind(),
            SyntaxKind::TypeExpr | SyntaxKind::CompoundTypeItemType
        ) {
            node = node.first_child().unwrap();
        }
        Some(match node.kind() {
            // SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::Call => Self::Call(CallExpr(node)),
            SyntaxKind::FloatLiteralExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::Ident => Self::Ident(Ident(node)),
            SyntaxKind::InfixExpr => Self::cast_infix(node),
            SyntaxKind::IntLiteralExpr => Self::IntLiteral(IntLiteral(node)),
            // SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            // SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::PathExpr => Self::Path(PathExpr(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::StringLiteralExpr => Self::StringLiteral(StringLiteral(node)),
            SyntaxKind::UnionTypeExpr => Self::Union__Old(Union__Old(node)),

            _ => return None,
        })
    }

    pub fn range(&self) -> TextRange {
        use TypeExpr as T;
        match self {
            // Binary(e) => e.range(),
            T::Call(e) => e.range(),
            T::FloatLiteral(e) => e.range(),
            T::Function(e) => e.range(),
            T::Ident(e) => e.range(),
            T::IntLiteral(e) => e.range(),
            T::Path(e) => e.range(),
            // If(e) => e.range(),
            T::Paren(e) => e.range(),
            T::StringLiteral(e) => e.range(),
            T::Union__Old(e) => e.range(),
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

/// Union with the current syntax
///
/// ```ignore
/// type Bool = union ( false, true )
/// type Status = union (
///     pending,
///     active,
///     complete: Int,
///     failed: String,
/// )
/// ```
#[derive(Debug, Clone)]
pub struct Union__Old(SyntaxNode);

impl Union__Old {
    pub fn variants(&self) -> Vec<CompoundTypeItem> {
        self.compound_type_block()
            .map(|block| block.items())
            .unwrap_or_default()
    }

    pub fn compound_type_block(&self) -> Option<CompoundTypeBlock> {
        self.0.children().find_map(CompoundTypeBlock::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    pub fn expr(&self) -> Option<TypeExpr> {
        self.0.children().find_map(TypeExpr::cast)
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

/// Eventually want to switch to the syntax with a vertical bar
///
/// ```ignore
/// type Bool = true | false
/// type Status =
///     | pending
///     | active
///     | complete: Int
///     | failed: String
/// ```
#[derive(Debug, Clone)]
pub struct Union(SyntaxNode);

impl Union {
    // flattens the (possibly) nested InfixExpr with Bar
    // operators into a list
    // type Stooge = larry | moe | curly
    //               ^^^^^^^^^^^^
    //               ^^^^^^^^^^^^^^^^^^^
    // nested InfixExpr here, almost like `(a | b) | c`
    // see the parser tests/binding.rs
    // for a full example of this structure
    pub fn variants(&self) -> Vec<CompoundTypeItem> {
        /*
        [crates/ast/src/type_expr.rs:193:9] self = Union(
            InfixExpr@9..18
              Ident@9..11
                Ident@9..10 "a"
                Emptyspace@10..11 " "
              Bar@11..12 "|"
              Emptyspace@12..13 " "
              InfixExpr@13..18
                Ident@13..15
                  Ident@13..14 "b"
                  Emptyspace@14..15 " "
                Bar@15..16 "|"
                Emptyspace@16..17 " "
                Ident@17..18
                  Ident@17..18 "c"
            ,
        )
                 */
        // this is fairly flakey, if the CST changes slightly this will definitely break
        let mut variants: Vec<CompoundTypeItem> = vec![];
        for child in self.0.children() {
            let kind = child.kind();
            match kind {
                // TODO - first child should be one of these
                SyntaxKind::Ident | SyntaxKind::CompoundTypeItem => {
                    variants.push(CompoundTypeItem(child));
                }
                SyntaxKind::InfixExpr => variants.append(&mut Union(child).variants()),
                _ => {}
            }
        }
        variants
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
pub struct CompoundTypeBlock(SyntaxNode);

impl CompoundTypeBlock {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        (node.kind() == SyntaxKind::CompoundTypeBlock).then_some(Self(node))
    }

    pub fn items(&self) -> Vec<CompoundTypeItem> {
        self.0
            .children()
            .filter_map(CompoundTypeItem::cast)
            .collect()
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

    pub fn type_expr(&self) -> Option<TypeExpr> {
        match self.0.kind() {
            SyntaxKind::Ident => None,
            SyntaxKind::CompoundTypeItem => {
                self.0
                    .children_with_tokens()
                    .skip_while(|child| match child.as_token() {
                        Some(token) => token.kind() != SyntaxKind::Colon,
                        None => true,
                    })
                    .skip(1) // consume the Colon
                    .filter_map(SyntaxElement::into_node)
                    .find_map(TypeExpr::cast)
            }

            _ => unreachable!(),
        }
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
