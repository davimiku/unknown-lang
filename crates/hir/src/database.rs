use la_arena::Arena;
use parser::SyntaxKind;

use crate::{BinaryOp, Expr, Stmt, UnaryOp};

#[derive(Debug, PartialEq, Default)]
pub struct Context {
    exprs: Arena<Expr>,
}

impl Context {
    pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
                name: ast.name()?.text().into(),
                value: self.lower_expr(ast.value()),
            },
            ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
        };

        Some(result)
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        dbg!(&ast);
        if let Some(ast) = ast {
            match ast {
                ast::Expr::IntLiteral(ast) => self.lower_int_literal(ast),
                ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
                ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
                ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
                ast::Expr::Block(ast) => self.lower_block(ast),
                ast::Expr::VariableRef(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_int_literal(&mut self, ast: ast::IntLiteral) -> Expr {
        let value: Option<i64> = ast.value().and_then(|token| token.text().parse().ok());

        if let Some(value) = value {
            Expr::IntLiteral(value)
        } else {
            Expr::Missing
        }
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Dash => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            _ => unreachable!(),
        };

        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        Expr::Binary {
            op,
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
        }
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Dash => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            expr: self.exprs.alloc(expr),
        }
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        dbg!(&ast);
        // create a new scope

        let stmts = Vec::new();

        Expr::Block {
            stmts,
            last_expr: None,
        }
    }

    fn lower_variable_ref(&mut self, ast: ast::VariableRef) -> Expr {
        Expr::VariableRef {
            var: ast.name().unwrap().text().into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();
        let hir = Context::default().lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
    }

    fn check_expr(input: &str, expected_hir: Expr, expected_database: Context) {
        let root = parse(input);
        // TODO: why is root.stmts() empty in failing tests
        let first_stmt = root.stmts().next().unwrap();
        dbg!(&first_stmt);
        let ast = match first_stmt {
            ast::Stmt::Expr(ast) => ast,
            _ => unreachable!(),
        };
        let mut database = Context::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database);
    }

    #[test]
    fn lower_variable_def() {
        check_stmt(
            "let foo = bar",
            Stmt::VariableDef {
                name: "foo".into(),
                value: Expr::VariableRef { var: "bar".into() },
            },
        )
    }

    #[test]
    fn lower_variable_def_without_name() {
        let root = parse("let = 10");
        let ast = root.stmts().next().unwrap();
        assert!(Context::default().lower_stmt(ast).is_none());
    }

    #[test]
    fn lower_variable_def_without_value() {
        check_stmt(
            "let a =",
            Stmt::VariableDef {
                name: "a".into(),
                value: Expr::Missing,
            },
        )
    }

    #[test]
    fn lower_expr_stmt() {
        check_stmt("123", Stmt::Expr(Expr::IntLiteral(123)))
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::IntLiteral(1));
        let rhs = exprs.alloc(Expr::IntLiteral(2));

        check_expr(
            "1 + 2",
            Expr::Binary {
                op: BinaryOp::Add,
                lhs,
                rhs,
            },
            Context { exprs },
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::IntLiteral(10));
        let rhs = exprs.alloc(Expr::Missing);

        check_expr(
            "10 -",
            Expr::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
            },
            Context { exprs },
        );
    }

    #[test]
    fn lower_literal() {
        check_expr("999", Expr::IntLiteral(999), Context::default());
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "((((((abc))))))",
            Expr::VariableRef { var: "abc".into() },
            Context::default(),
        )
    }

    #[test]
    fn lower_unary_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(Expr::IntLiteral(10));

        check_expr(
            "-10",
            Expr::Unary {
                op: UnaryOp::Neg,
                expr: ten,
            },
            Context { exprs },
        );
    }

    #[test]
    fn lower_variable_ref() {
        check_expr(
            "foo",
            Expr::VariableRef { var: "foo".into() },
            Context::default(),
        )
    }

    #[test]
    fn lower_empty_block() {
        check_expr(
            "{}",
            Expr::Block {
                stmts: vec![],
                last_expr: None,
            },
            Context::default(),
        )
    }

    #[test]
    fn lower_block_with_one_statement() {
        check_expr(
            r#"{ 
    let a = 4 
}"#,
            Expr::Block {
                stmts: vec![],
                last_expr: None,
            },
            Context::default(),
        )
    }
}
