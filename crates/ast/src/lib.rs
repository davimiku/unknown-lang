mod validation;

use parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

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
            _ => Self::Expr(Expr::cast(node)?),
        };

        Some(result)
    }
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum Expr {
    Block(Block),
    Binary(Binary),
    Function(Function),
    IntLiteral(IntLiteral),
    Loop(Loop),
    Paren(ParenExpr),
    Unary(Unary),
    VariableRef(VariableRef),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::FunExpr => Self::Function(Function(node)),
            SyntaxKind::InfixExpr => Self::Binary(Binary(node)),
            SyntaxKind::IntExpr => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::NegationExpr => Self::Unary(Unary(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef(node)),
            _ => return None,
        };

        Some(result)
    }
}

#[derive(Debug)]
pub struct Block(SyntaxNode);

impl Block {
    // TODO: Iterator ?
    pub fn stmts(&self) -> Vec<Stmt> {
        todo!()
    }

    pub fn last_expr(&self) -> Option<Expr> {
        let children = self.0.children();

        children.inspect(|c| println!("{c}"));

        None
    }
}

#[derive(Debug)]
pub struct Binary(SyntaxNode);

impl Binary {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        use SyntaxKind::*;
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), Plus | Dash | Star | Slash))
    }
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct IntLiteral(SyntaxNode);

impl IntLiteral {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IntExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<parser::SyntaxToken> {
        self.0.first_token()
    }
}

#[derive(Debug)]
pub struct Loop(SyntaxNode);

impl Loop {
    pub fn block(&self) -> Block {
        todo!()
    }
}

#[derive(Debug)]
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct Unary(SyntaxNode);

impl Unary {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Dash)
    }
}

#[derive(Debug)]
pub struct VariableRef(SyntaxNode);

impl VariableRef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct Param(SyntaxNode);

impl Param {
    // Option<Ident> ?
    pub fn name(&self) -> Ident {
        todo!()
    }

    pub fn r#type(&self) -> Type {
        todo!()
    }
}

#[derive(Debug)]
pub struct Ident(SyntaxNode);

#[derive(Debug)]
pub struct ReturnType(SyntaxNode);

#[derive(Debug)]
pub struct Type(SyntaxNode);
