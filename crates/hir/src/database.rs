use crate::interner::Name;
use crate::Expr;
use la_arena::{Arena, ArenaMap, Idx};
use std::collections::HashMap;
use text_size::TextRange;

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) expr_ranges: ArenaMap<Idx<Expr>, TextRange>,
}

impl Database {
    pub(crate) fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        let idx = self.exprs.alloc(expr);

        let range = ast.map_or(Default::default(), |ast| ast.range());
        self.expr_ranges.insert(idx, range);

        idx
    }
}

#[cfg(test)]
mod tests {

    use crate::context::Context;
    use crate::{BinaryExpr, BinaryOp, BlockExpr, IfExpr, UnaryExpr, UnaryOp};

    use super::*;

    fn parse_expr(input: &str) -> ast::Expr {
        ast::Expr::cast(parser::test_parse_expr(input).syntax()).unwrap()
    }

    fn check_expr(input: &str, expected_hir: Expr) {
        let ast = parse_expr(input);

        let mut context = Context::default();
        let actual = context.lower_expr(Some(ast));

        let mut expected_database = Database::default();
        let expected = expected_database.exprs.alloc(expected_hir);

        assert_eq!(
            context.database.exprs[actual],
            expected_database.exprs[expected]
        );
    }

    #[test]
    #[ignore = "TODO: revisit after finishing LetBinding"]
    fn experiment_lower_variable_def() {
        // let input = "let a = b";
        // let root = parse(input);
        // let ast = root.stmts().next().unwrap();

        // let mut context = Context::new();
        // let hir = context.lower_stmt(ast).unwrap();

        // let s = context.database.debug_string();

        // dbg!(s);
    }

    #[test]
    #[ignore = "TODO: Figure out how to test with interned strings"]
    fn lower_let_binding() {
        // let input = "let a = 1";
        // let mut exprs = Arena::new();

        // let expected_let_binding = LocalDef {
        //     key
        //     name: "a".to_owned(),
        //     type_annotation: None,
        //     value: exprs.alloc(Expr::IntLiteral(1)),
        //     local_idx: 0,
        // };
        // let expected_hir = Expr::LocalDef(expected_let_binding);

        // check_expr(input, expected_hir);
    }

    #[test]
    #[ignore = "TODO: Figure out how to test with interned strings"]
    fn lower_variable_def_without_name() {
        // let root = parse("let = 10");
        // let ast = root.stmts().next().unwrap();

        // let mut context = Context::new();
        // let stmt = context.lower_stmt(ast);
        // dbg!(stmt);
        // let x = &context.database.stmts[stmt.unwrap()];
        // match x {
        //     Stmt::VariableDef(d) => {
        //         let v = &context.database.local_defs[*d];
        //         dbg!(&context.database.exprs[v.value]);
        //     }
        //     Stmt::Expr(_) => unreachable!(),
        // }
        // assert!(context.lower_stmt(ast).is_none());
    }

    #[test]
    #[ignore = "TODO: Figure out how to test with interned strings"]
    fn lower_variable_def_without_value() {
        // let input = "let a =";
        // // let mut exprs = Arena::new();

        // let expected_let_binding = LocalDef {
        //     name: todo!(),
        //     type_annotation: None,
        //     value: todo!(),
        //     local_idx: todo!(),
        // };
        // let expected_hir = Expr::LocalDef(expected_let_binding);

        // check_expr(input, expected_hir);
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();

        let lhs = exprs.alloc(Expr::IntLiteral(1));
        let rhs = exprs.alloc(Expr::IntLiteral(2));
        let op = BinaryOp::Add;
        let expected_hir = Expr::Binary(BinaryExpr { op, lhs, rhs });

        check_expr("1 + 2", expected_hir);
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();

        let lhs = exprs.alloc(Expr::IntLiteral(10));
        let rhs = exprs.alloc(Expr::Empty);
        let op = BinaryOp::Sub;

        check_expr("10 -", Expr::Binary(BinaryExpr { op, lhs, rhs }));
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
    fn lower_negation_expr() {
        let mut exprs = Arena::new();

        let expr = exprs.alloc(Expr::IntLiteral(10));
        let op = UnaryOp::Neg;

        let input = "-10";
        let expected_hir = Expr::Unary(UnaryExpr { op, expr });

        check_expr(input, expected_hir);
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

        let expected_hir = Expr::Block(BlockExpr { exprs: vec![] });

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_not_expr() {
        let mut exprs = Arena::new();

        let expr = exprs.alloc(Expr::BoolLiteral(true));
        let op = UnaryOp::Not;

        let input = "not true";
        let expected_hir = Expr::Unary(UnaryExpr { op, expr });

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_if_expression() {
        let mut exprs = Arena::new();

        let condition = exprs.alloc(Expr::BoolLiteral(true));
        let then_branch = exprs.alloc(Expr::Block(BlockExpr { exprs: vec![] }));

        let input = "if true {}";
        let expected_hir = Expr::If(IfExpr {
            condition,
            then_branch,
            else_branch: None,
        });

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_if_else_expression() {
        let mut exprs = Arena::new();

        let condition = exprs.alloc(Expr::BoolLiteral(true));
        let then_branch = exprs.alloc(Expr::Block(BlockExpr { exprs: vec![] }));
        let else_branch = exprs.alloc(Expr::Block(BlockExpr { exprs: vec![] }));

        let input = "if true {} else {}";
        let expected_hir = Expr::If(IfExpr {
            condition,
            then_branch,
            else_branch: Some(else_branch),
        });

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_if_else_if_expression() {
        let mut exprs = Arena::new();

        let condition = exprs.alloc(Expr::BoolLiteral(true));
        let then_branch = exprs.alloc(Expr::Block(BlockExpr { exprs: vec![] }));

        let condition2 = exprs.alloc(Expr::BoolLiteral(false));
        let then_branch2 = exprs.alloc(Expr::Block(BlockExpr { exprs: vec![] }));

        let else_branch = exprs.alloc(Expr::If(IfExpr {
            condition: condition2,
            then_branch: then_branch2,
            else_branch: None,
        }));

        let input = "if true {} else if false {}";
        let expected_hir = Expr::If(IfExpr {
            condition,
            then_branch,
            else_branch: Some(else_branch),
        });

        check_expr(input, expected_hir)
    }

    // TODO: lower block tests
}
