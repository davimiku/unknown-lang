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
        if let Some(ast) = ast {
            match ast {
                ast::Expr::Binary(ast) => self.lower_binary(ast),
                ast::Expr::Block(ast) => self.lower_block(ast),
                ast::Expr::BoolLiteral(ast) => self.lower_bool_literal(ast),
                ast::Expr::Function(_ast) => todo!(),
                ast::Expr::Loop(ast) => self.lower_loop(ast),
                ast::Expr::IntLiteral(ast) => self.lower_int_literal(ast),
                ast::Expr::Paren(ast) => self.lower_expr(ast.expr()),
                ast::Expr::StringLiteral(ast) => self.lower_string_literal(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Ident(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let value: Option<bool> = ast.value().and_then(|token| token.text().parse().ok());

        if let Some(value) = value {
            Expr::BoolLiteral(value)
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

    fn lower_string_literal(&mut self, ast: ast::StringLiteral) -> Expr {
        let value: Option<String> = ast.value().map(|token| token.text().to_owned());

        if let Some(s) = value {
            let s = &s[1..s.len() - 1]; // remove leading and trailing quotes
            Expr::StringLiteral(s.to_string())
        } else {
            Expr::Missing
        }
    }

    fn lower_binary(&mut self, ast: ast::Binary) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Dash => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::Dot => BinaryOp::Path,
            SyntaxKind::Caret => BinaryOp::Exp,
            SyntaxKind::Percent => BinaryOp::Rem,
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

    fn lower_unary(&mut self, ast: ast::Unary) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Dash => UnaryOp::Neg,
            SyntaxKind::Not => UnaryOp::Not,
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

        // temp: dummy value
        let stmts = Vec::new();

        Expr::Block {
            stmts,
            // TODO: should we just use the last index of stmts?
            // last_expr: None,
        }
    }

    fn lower_loop(&mut self, ast: ast::Loop) -> Expr {
        dbg!(&ast);

        // create a new scope
        // identify break expressions
        // lower statements/expressions
        todo!()
    }

    fn lower_variable_ref(&mut self, ast: ast::Ident) -> Expr {
        Expr::VariableRef {
            name: ast.name().unwrap().text().into(),
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

        let ast = root.expr();

        if ast.is_none() {
            dbg!(root);
            panic!("expected a top-level expression")
        };
        let ast = ast.unwrap();

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
                value: Expr::VariableRef { name: "bar".into() },
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
    fn lower_int_literal() {
        check_expr("999", Expr::IntLiteral(999), Context::default());
    }

    #[test]
    fn lower_string_literal() {
        check_expr(
            r#""hello""#,
            Expr::StringLiteral("hello".to_string()),
            Context::default(),
        )
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "((((((abc))))))",
            Expr::VariableRef { name: "abc".into() },
            Context::default(),
        )
    }

    #[test]
    fn lower_negation_expr() {
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
            Expr::VariableRef { name: "foo".into() },
            Context::default(),
        )
    }

    #[test]
    fn lower_bool_expr() {
        check_expr("true", Expr::BoolLiteral(true), Context::default())
    }

    #[test]
    fn lower_empty_block() {
        check_expr(
            "{}",
            Expr::Block {
                stmts: vec![],
                // last_expr: None,
            },
            Context::default(),
        )
    }

    // TODO: need to implement
    #[test]
    fn lower_not_expr() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::BoolLiteral(true));

        check_expr(
            "not true",
            Expr::Unary {
                op: UnaryOp::Not,
                expr,
            },
            Context { exprs },
        )
    }

    #[ignore = "not implemented!"]
    #[test]
    fn lower_block_with_one_statement() {
        let mut stmts = Arena::new();
        let a_4 = stmts.alloc(Stmt::VariableDef {
            name: "a".into(),
            value: Expr::IntLiteral(4),
        });
        check_expr(
            r#"{ 
    let a = 4 
}"#,
            Expr::Block {
                stmts: vec![a_4],
                // last_expr: None,
            },
            Context::default(),
        )
    }
}
