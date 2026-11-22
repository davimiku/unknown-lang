use parser::{SyntaxKind, SyntaxNode};

pub const EXPORT_ATTRIBUTE: &'static str = "#export";
pub const BUILTIN_ATTRIBUTE: &'static str = "#builtin";

pub(super) fn has_export_attribute(node: &SyntaxNode) -> bool {
    attribute(node, EXPORT_ATTRIBUTE).is_some()
}

pub(super) fn attribute(node: &SyntaxNode, name: &str) -> Option<SyntaxNode> {
    let mut sibling = node.prev_sibling();
    while let Some(previous) = sibling {
        match previous.kind() {
            SyntaxKind::Newline | SyntaxKind::Emptyspace => {
                sibling = previous.prev_sibling();
            }
            SyntaxKind::Attribute => {
                let matches = previous
                    .first_token()
                    .is_some_and(|token| token.text() == name);
                if matches {
                    return Some(previous);
                }
                sibling = previous.prev_sibling();
            }
            _ => return None,
        }
    }
    None
}

pub(super) fn string_argument(node: &SyntaxNode, name: &str) -> Option<String> {
    let attribute = attribute(node, name)?;
    let token = attribute
        .descendants()
        .find(|child| child.kind() == SyntaxKind::StringLiteralExpr)?
        .first_token()?;
    token
        .text()
        .strip_prefix('"')
        .and_then(|value| value.strip_suffix('"'))
        .map(str::to_owned)
}

pub(super) fn attribute_ranges(node: &SyntaxNode, name: &str) -> Vec<text_size::TextRange> {
    node.descendants()
        .filter(|child| child.kind() == SyntaxKind::Attribute)
        .filter(|attribute| {
            attribute
                .first_token()
                .is_some_and(|token| token.text() == name)
        })
        .map(|attribute| attribute.text_range())
        .collect()
}
