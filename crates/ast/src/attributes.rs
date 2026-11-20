use parser::{SyntaxKind, SyntaxNode};

pub const EXPORT_ATTRIBUTE: &'static str = "#export";

pub(super) fn has_export_attribute(node: &SyntaxNode) -> bool {
    let mut sibling = node.prev_sibling();
    while let Some(previous) = sibling {
        match previous.kind() {
            SyntaxKind::Newline | SyntaxKind::Emptyspace => {
                sibling = previous.prev_sibling();
            }
            SyntaxKind::Attribute => {
                return previous
                    .first_token()
                    .is_some_and(|token| token.text() == EXPORT_ATTRIBUTE);
            }
            _ => return false,
        }
    }
    false
}
