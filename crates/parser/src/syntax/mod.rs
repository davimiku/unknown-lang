mod syntax_kind;

use num_traits::{FromPrimitive, ToPrimitive};

pub use syntax_kind::SyntaxKind;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum UnknownLanguage {}

impl rowan::Language for UnknownLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<UnknownLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<UnknownLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<UnknownLanguage>;

pub trait SyntaxNodeExt {
    fn has_child_of(&self, kind: SyntaxKind) -> bool;
}

impl SyntaxNodeExt for SyntaxNode {
    fn has_child_of(&self, kind: SyntaxKind) -> bool {
        self.children_with_tokens().any(|node| node.kind() == kind)
    }
}
