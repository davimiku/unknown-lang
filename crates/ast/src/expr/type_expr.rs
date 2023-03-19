use parser::{SyntaxKind, SyntaxNode};
use text_size::TextRange;

use super::{BoolLiteral, Call, FloatLiteral, IntLiteral, Path, StringLiteral};

#[derive(Debug, Clone)]
pub enum TypeExpr {
    // Binary(Binary), // parameterize to work on either Expr | TypeExpr
    BoolLiteral(BoolLiteral),
    FloatLiteral(FloatLiteral),
    Function(Function),
    Path(Path),
    Call(Call),
    // If(IfExpr), // todo
    IntLiteral(IntLiteral),
    // Paren(ParenExpr), // parameterize to work on either Expr | TypeExpr
    StringLiteral(StringLiteral),
    // TypeBinding(TypeBinding), // the full `type A = struct { ... }`
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
            SyntaxKind::Call => Self::Call(Call(node)),
            SyntaxKind::FloatLiteralExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::InfixExpr => Self::cast_binary(node),
            SyntaxKind::IntLiteralExpr => Self::IntLiteral(IntLiteral(node)),
            // SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            // SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::Path => Self::Path(Path(node)),
            // SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::StringLiteralExpr => Self::StringLiteral(StringLiteral(node)),
            _ => return None,
        })
    }

    pub fn range(&self) -> TextRange {
        use TypeExpr::*;
        match self {
            // Binary(e) => e.range(),
            BoolLiteral(e) => e.range(),
            Call(e) => e.range(),
            FloatLiteral(e) => e.range(),
            Function(e) => e.range(),
            Path(e) => e.range(),
            // If(e) => e.range(),
            IntLiteral(e) => e.range(),
            // Paren(e) => e.range(),
            StringLiteral(e) => e.range(),
            // Unary(e) => e.range(),
        }
    }

    fn cast_binary(node: SyntaxNode) -> Self {
        let is_function = node.children().any(|node| node.kind() == SyntaxKind::Arrow);

        if is_function {
            Self::Function(Function(node))
        } else {
            todo!()
            // Self::Binary(Binary(node))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function(SyntaxNode);

// TODO: what of these need to be Option ?
// prefer not to panic if possible, and create a proper diagnostic
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
