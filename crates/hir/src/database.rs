use std::str::FromStr;

use la_arena::{Arena, ArenaMap, Idx};
use parser::SyntaxKind;
use text_size::TextRange;

use crate::types::Type;
use crate::{BinaryOp, Expr, Stmt, UnaryOp, VariableDef};

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    statements: Arena<Stmt>,
    exprs: Arena<Expr>,
    expr_ranges: ArenaMap<Idx<Expr>, TextRange>,
    variable_defs: Arena<VariableDef>,
}

impl Database {
    pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(ast) => self.lower_variable_def(ast),
            ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
        };

        Some(result)
    }

    fn lower_variable_def(&mut self, ast: ast::VariableDef) -> Stmt {
        let value = self.lower_expr(ast.value());
        let idx = self.variable_defs.alloc(VariableDef { value, ast });

        // insert into scopes .insert(name, idx)

        Stmt::VariableDef(idx)
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Idx<Expr> {
        let expr = if let Some(ast) = ast.clone() {
            match ast {
                ast::Expr::Binary(ast) => self.lower_binary(ast),
                ast::Expr::Block(ast) => self.lower_block(ast),
                ast::Expr::BoolLiteral(ast) => self.lower_bool_literal(ast),
                ast::Expr::FloatLiteral(ast) => self.lower_float_literal(ast),
                ast::Expr::Function(ast) => todo!(),
                ast::Expr::Loop(ast) => self.lower_loop(ast),
                ast::Expr::IntLiteral(ast) => self.lower_int_literal(ast),
                ast::Expr::Paren(ast) => return self.lower_expr(ast.expr()),
                ast::Expr::StringLiteral(ast) => self.lower_string_literal(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Ident(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        };

        let idx = self.exprs.alloc(expr);
        let range = ast.map_or(Default::default(), |ast| ast.range());
        self.expr_ranges.insert(idx, range);

        idx
    }

    fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let value: Option<bool> = ast.value().and_then(|token| token.text().parse().ok());

        if let Some(value) = value {
            Expr::BoolLiteral(value)
        } else {
            Expr::Missing
        }
    }

    fn lower_float_literal(&mut self, ast: ast::FloatLiteral) -> Expr {
        let value: Option<f64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or_else(|| Expr::Missing, Expr::FloatLiteral)
    }

    fn lower_int_literal(&mut self, ast: ast::IntLiteral) -> Expr {
        let value: Option<i64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or_else(|| Expr::Missing, Expr::IntLiteral)
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
        let op = match ast.op().expect("valid binary op token").kind() {
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
            lhs,
            rhs,
            lhs_type: Type::Undetermined, // TODO: Default::default()
            rhs_type: Type::Undetermined, // TODO: Default::default()
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
            expr,
            typ: Type::Undetermined, // TODO: Default::default()
        }
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        // create a new scope

        // temp: dummy value
        let stmts = Vec::new();

        Expr::Block {
            stmts,
            // TODO: should we instead use the last index of stmts?
            // last_expr: None,
            typ: Type::Undetermined, // TODO: Default::default()
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
            typ: Type::Undetermined, // TODO: Default::default()
        }
    }
}

fn parse_ignore_underscore<T: FromStr>(s: &str) -> Option<T> {
    let mut s = s.to_string();
    s.retain(|c| c != '_');

    s.parse().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn parse_expr(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse_expr(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt, expected_database: Database) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();

        let mut database = Database::default();
        let hir = database.lower_stmt(ast).unwrap();

        assert_eq!(database, expected_database);
    }

    fn check_expr(input: &str, expected_hir: Expr) {
        let root = parse_expr(input);
        let ast = root.expr().expect("expected a top-level expression");

        let mut database = Database::default();
        let actual = database.lower_expr(Some(ast));

        let mut expected_database = Database::default();
        let expected = expected_database.exprs.alloc(expected_hir);

        assert_eq!(database.exprs[actual], expected_database.exprs[expected]);
    }

    fn init_arenas() -> (Arena<Stmt>, Arena<Expr>, ArenaMap<Idx<Expr>, TextRange>) {
        let statements = Arena::new();
        let exprs = Arena::new();
        let expr_ranges = ArenaMap::default();

        (statements, exprs, expr_ranges)
    }

    #[test]
    fn lower_variable_def() {
        let mut expected_database = Database::default();
        let bar_expr = Expr::VariableRef {
            name: "bar".into(),
            typ: Type::Undetermined, // TODO: Default::default()
        };
        let bar_expr = expected_database.exprs.alloc(bar_expr);

        let variable_def = VariableDef {
            value: bar_expr,
            ast: todo!(),
        };
        let variable_def = expected_database.variable_defs.alloc(variable_def);
        let expected_hir = Stmt::VariableDef(variable_def);

        check_stmt("let foo = bar", expected_hir, expected_database)
    }

    #[test]
    fn lower_variable_def_without_name() {
        let root = parse("let = 10");
        let ast = root.stmts().next().unwrap();
        assert!(Database::default().lower_stmt(ast).is_none());
    }

    #[test]
    fn lower_variable_def_without_value() {
        let mut expected_database = Database::default();
        let missing_expr = Expr::Missing;
        let missing_expr = expected_database.exprs.alloc(missing_expr);

        let variable_def = VariableDef {
            value: missing_expr,
            ast: todo!(),
        };
        let variable_def = expected_database.variable_defs.alloc(variable_def);
        let expected_hir = Stmt::VariableDef(variable_def);

        check_stmt("let a =", expected_hir, expected_database)
    }

    #[test]
    fn lower_expr_stmt() {
        let input = "123";

        let mut expected_database = Database::default();
        let int_expr = Expr::IntLiteral(123);
        let int_expr = expected_database.exprs.alloc(int_expr);

        check_stmt(input, Stmt::Expr(int_expr), expected_database)
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
                lhs_type: Type::Undetermined, // TODO: Default::default()
                rhs_type: Type::Undetermined, // TODO: Default::default()
            },
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
                lhs_type: Type::Undetermined, // TODO: Default::default()
                rhs_type: Type::Undetermined, // TODO: Default::default()
            },
        );
    }

    #[test]
    fn lower_int_literal() {
        let input = "999";
        let expected_hir = Expr::IntLiteral(999);

        check_expr(input, expected_hir);
    }

    #[test]
    fn lower_int_literal_with_separators() {
        let input = "123_456_789";
        let expected_hir = Expr::IntLiteral(123_456_789);

        check_expr(input, expected_hir);
    }

    #[test]
    fn lower_string_literal() {
        let input = r#""hello""#;
        let expected_hir = Expr::StringLiteral("hello".to_string());

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_paren_expr() {
        let input = &"((((((abc))))))";
        let expected_hir = Expr::VariableRef {
            name: "abc".into(),
            typ: Type::Undetermined, // TODO: Default::default()
        };

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_negation_expr() {
        let mut exprs = Arena::new();

        let ten = exprs.alloc(Expr::IntLiteral(10));

        let input = "-10";
        let expected_hir = Expr::Unary {
            op: UnaryOp::Neg,
            expr: ten,
            typ: Type::Undetermined, // TODO: Default::default()
        };

        check_expr(input, expected_hir);
    }

    #[test]
    fn lower_variable_ref() {
        let input = "foo";
        let expected_hir = Expr::VariableRef {
            name: "foo".into(),
            typ: Type::Undetermined, // TODO: Default::default()
        };

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_bool_expr() {
        let input = "true";
        let expected_hir = Expr::BoolLiteral(true);

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_empty_block() {
        let input = "{}";

        let expected_hir = Expr::Block {
            stmts: vec![],
            typ: Type::Undetermined, // TODO: Default::default()
        };

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_not_expr() {
        let mut exprs = Arena::new();

        let expr = exprs.alloc(Expr::BoolLiteral(true));

        let input = "not true";
        let expected_hir = Expr::Unary {
            op: UnaryOp::Not,
            expr,
            typ: Type::Undetermined, // TODO: Default::default()
        };

        check_expr(input, expected_hir)
    }

    // TODO: lower block tests
}
