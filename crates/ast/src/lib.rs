mod validation;

use parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextRange;

// TODO: investigate macros to reduce boilerplate
//
// Each node probably has a "cast" function
// Each node should have a "range" function for reporting diagnostics

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
    VariableDef(VariableDef),
    Expr(Expr),
}

impl Stmt {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef(node)),
            _ => {
                let expr = node.children().find_map(Expr::cast)?;
                Self::Expr(expr)
            }
        };

        Some(result)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableDef(SyntaxNode);

impl VariableDef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn value(&self) -> Option<Expr> {
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
    Function(Function),
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    Loop(Loop),
    Paren(ParenExpr),
    StringLiteral(StringLiteral),
    Unary(Unary),
    Ident(Ident),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::BoolExpr => Self::BoolLiteral(BoolLiteral(node)),
            SyntaxKind::FunExpr => Self::Function(Function(node)),
            SyntaxKind::NameRef => Self::Ident(Ident(node)),
            SyntaxKind::InfixExpr => Self::Binary(Binary(node)),
            SyntaxKind::IntExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::FloatExpr => Self::FloatLiteral(FloatLiteral(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::NotExpr => Self::Unary(Unary(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::StringExpr => Self::StringLiteral(StringLiteral(node)),
            _ => return None,
        };

        Some(result)
    }

    pub fn range(self) -> TextRange {
        match self {
            Expr::Block(e) => e.range(),
            Expr::Binary(e) => e.range(),
            Expr::BoolLiteral(e) => e.range(),
            Expr::Function(e) => e.range(),
            Expr::IntLiteral(e) => e.range(),
            Expr::FloatLiteral(e) => e.range(),
            Expr::Loop(e) => e.range(),
            Expr::Paren(e) => e.range(),
            Expr::StringLiteral(e) => e.range(),
            Expr::Unary(e) => e.range(),
            Expr::Ident(e) => e.range(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block(SyntaxNode);

impl Block {
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
pub struct Binary(SyntaxNode);

impl Binary {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    // todo: return a BinarySyntaxToken enum or something like that
    // to make working with this function easier/exhaustive
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
pub struct Function(SyntaxNode);

impl Function {
    // TODO: what of these need to be Option ?
    pub fn param_list(&self) -> Option<ParamList> {
        self.0.children().find_map(ParamList::cast)
    }

    pub fn return_ty(&self) -> ReturnType {
        todo!()
    }

    pub fn body(&self) -> Expr {
        todo!()
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

    pub fn range(&self) -> TextRange {
        self.0.text_range()
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
pub struct BoolLiteral(SyntaxNode);

impl BoolLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::BoolExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Loop(SyntaxNode);

impl Loop {
    pub fn block(&self) -> Block {
        todo!()
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
pub struct StringLiteral(SyntaxNode);

impl StringLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::StringExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
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

#[derive(Debug, Clone)]
pub struct Ident(SyntaxNode);

impl Ident {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ParamList(SyntaxNode);

impl ParamList {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IntExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    // Iterator ?
    pub fn params(&self) -> Vec<Param> {
        todo!()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Param(SyntaxNode);

impl Param {
    // Option<Ident> ?
    pub fn name(&self) -> Ident {
        todo!()
    }

    pub fn typ(&self) -> TypeDef {
        todo!()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnType(SyntaxNode);

impl ReturnType {
    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef(SyntaxNode);

impl TypeDef {
    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}
