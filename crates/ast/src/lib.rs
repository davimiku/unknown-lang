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
    /// A local (let) binding
    LocalDef(LocalDef),

    /// Expression
    Expr(Expr),
}

impl Stmt {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::VariableDef => Self::LocalDef(LocalDef(node)),
            _ => {
                let expr = node.children().find_map(Expr::cast)?;
                Self::Expr(expr)
            }
        };

        Some(result)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocalDef(SyntaxNode);

impl LocalDef {
    // TODO: Pattern rather than name (Ident)
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn type_annotation(&self) -> Option<TypeExpr> {
        self.0.children().find_map(TypeExpr::cast)
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
    Call(Call),
    FloatLiteral(FloatLiteral),
    Function(Function),
    Ident(Ident),
    IntLiteral(IntLiteral),
    Loop(Loop),
    Paren(ParenExpr),
    StringLiteral(StringLiteral),
    TypeExpr(TypeExpr),
    Unary(Unary),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::BlockExpr => Self::Block(Block(node)),
            SyntaxKind::BoolExpr => Self::BoolLiteral(BoolLiteral(node)),
            SyntaxKind::Call => Self::Call(Call(node)),
            SyntaxKind::FunExpr => Self::Function(Function(node)),
            SyntaxKind::Path => Self::Ident(Ident(node)),
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
        use Expr::*;
        match self {
            Block(e) => e.range(),
            Binary(e) => e.range(),
            BoolLiteral(e) => e.range(),
            Call(e) => e.range(),
            FloatLiteral(e) => e.range(),
            Function(e) => e.range(),
            Ident(e) => e.range(),
            IntLiteral(e) => e.range(),
            Loop(e) => e.range(),
            Paren(e) => e.range(),
            StringLiteral(e) => e.range(),
            TypeExpr(e) => e.range(),
            Unary(e) => e.range(),
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
pub struct Call(SyntaxNode);

impl Call {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Call {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn path(&self) -> Option<Path> {
        self.0.children().find_map(Path::cast)
    }

    // TODO: how to get the call args...
    pub fn call_args(&self) -> Option<CallArgs> {
        self.0.children().find_map(CallArgs::cast)
    }
}

#[derive(Debug, Clone)]
pub struct CallArgs(SyntaxNode);

impl CallArgs {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::CallArgs {
            Some(Self(node))
        } else {
            None
        }
    }

    // TODO: perhaps implement Iterator (Item = Expr) for CallArgs so the caller can collect
    pub fn as_vec(&self) -> Vec<Expr> {
        self.0.children().filter_map(Expr::cast).collect()
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
pub struct TypeExpr(SyntaxNode);

impl TypeExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeExpr {
            Some(Self(node))
        } else {
            None
        }
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
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Ident {
            Some(Self(node))
        } else {
            None
        }
    }
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

#[derive(Debug, Clone)]
pub struct Path(SyntaxNode);

impl Path {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Path {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }

    pub fn idents(&self) -> Vec<Ident> {
        self.0.descendants().filter_map(Ident::cast).collect()
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

#[cfg(test)]
mod tests {

    use super::*;

    fn parse_node(input: &str) -> SyntaxNode {
        parser::parse_expr(input).syntax()
    }

    fn parse_expr(input: &str) -> Expr {
        let root = Root::cast(parse_node(input)).unwrap();
        root.expr().expect("expected a top-level expression")
    }

    #[test]
    fn call_no_arguments() {
        let input = "my_func";
        let parsed = parse_expr(input);

        assert!(matches!(parsed, Expr::Call(_)));

        if let Expr::Call(call) = parsed {
            let call_args = call.call_args();
            assert!(matches!(call_args, None));
        } else {
            unreachable!()
        }
    }

    #[test]
    fn call_empty_arg() {
        let input = "my_func ()";
        let parsed = parse_expr(input);

        assert!(matches!(parsed, Expr::Call(_)));

        if let Expr::Call(call) = parsed {
            let call_args = call.call_args();
            assert!(matches!(call_args, Some(_)));

            let args = call_args.unwrap().as_vec();
            assert_eq!(0, args.len());
        } else {
            unreachable!()
        }
    }

    #[test]
    fn call_one_arg() {
        let input = "my_func 1";
        let parsed = parse_expr(input);

        assert!(matches!(parsed, Expr::Call(_)));

        if let Expr::Call(call) = parsed {
            let call_args = call.call_args();
            assert!(matches!(call_args, Some(_)));

            let args = call_args.unwrap().as_vec();
            assert_eq!(1, args.len());
        } else {
            unreachable!()
        }
    }

    #[test]
    fn call_arguments() {
        let input = "my_func (1, 2)";
        let parsed = parse_expr(input);

        assert!(matches!(parsed, Expr::Call(_)));

        if let Expr::Call(call) = parsed {
            let call_args = call.call_args();
            assert!(matches!(call_args, Some(_)));

            let args = call_args.unwrap().as_vec();
            assert_eq!(2, args.len());
        } else {
            unreachable!()
        }
    }
}
