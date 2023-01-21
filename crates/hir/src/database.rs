use la_arena::{Arena, ArenaMap, Idx};
use std::fmt::{self, Write as FmtWrite};
use text_size::TextRange;

use crate::{BinaryExpr, BlockExpr, CallExpr, Expr, FunctionExpr, LetBinding, UnaryExpr};

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) expr_ranges: ArenaMap<Idx<Expr>, TextRange>,

    /// Local (let) bindings of variable definitions
    pub(crate) let_bindings: Arena<LetBinding>,
}

impl Database {
    // TODO: reduce visibility after updating tests in vm::exec
    pub fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        let idx = self.exprs.alloc(expr);

        let range = ast.map_or(Default::default(), |ast| ast.range());
        self.expr_ranges.insert(idx, range);

        idx
    }

    pub(super) fn alloc_let_binding(&mut self, let_binding: LetBinding) -> Idx<LetBinding> {
        self.let_bindings.alloc(let_binding)
    }

    fn fmt_expr(&self, s: &mut String, idx: Idx<Expr>, mut indent: usize) -> fmt::Result {
        match &self.exprs[idx] {
            Expr::Empty => write!(s, "{{empty}}"),

            Expr::BoolLiteral(b) => write!(s, "{b}"),
            Expr::FloatLiteral(f) => write!(s, "{f}"),
            Expr::IntLiteral(i) => write!(s, "{i}"),
            Expr::StringLiteral(sl) => write!(s, "{sl}"),

            Expr::Call(CallExpr { path, args }) => {
                write!(s, "{path} ")?;
                if args.len() == 1 {
                    self.fmt_expr(s, args[0], indent)
                } else {
                    write!(s, "(")?;
                    for arg in args {
                        self.fmt_expr(s, *arg, indent)?;
                        write!(s, ",")?;
                    }
                    write!(s, ")")
                }
            }

            Expr::Binary(BinaryExpr { op, lhs, rhs, .. }) => {
                self.fmt_expr(s, *lhs, indent)?;

                write!(s, " {} ", op)?;

                self.fmt_expr(s, *rhs, indent)
            }

            Expr::Unary(UnaryExpr { op, expr, .. }) => {
                write!(s, "{}", op)?;

                self.fmt_expr(s, *expr, indent)
            }

            Expr::Block(BlockExpr { exprs, .. }) => {
                writeln!(s, "{{")?;
                indent += 4;

                for idx in exprs {
                    self.fmt_expr(s, *idx, indent)?;
                }

                indent -= 4;

                write!(s, "{}}}", " ".repeat(indent))
            }

            Expr::VariableRef { name, .. } => write!(s, "{}", name),

            Expr::Function(FunctionExpr {
                params,
                body,
                return_type_annotation,
            }) => {
                write!(s, "fun (")?;

                for param in params {
                    self.fmt_expr(s, *param, indent)?;
                }

                write!(s, ") -> ")?;

                if return_type_annotation.is_some() {
                    let return_type = return_type_annotation.unwrap();
                    self.fmt_expr(s, return_type, indent)?;
                }

                self.fmt_expr(s, *body, indent)
            }

            Expr::LetBinding(local_def) => {
                // let def = &self.local_defs[*local_def];

                // write!(s, "let _{:?} = ", local_def.into_raw())?;
                // self.write_expr(s, def.value, indent);
                todo!()
            }

            Expr::If(if_expr) => {
                todo!()
            }
        }
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

        let mut context = Context::new();
        let actual = context.lower_expr(Some(ast));

        let mut expected_database = Database::default();
        let expected = expected_database.exprs.alloc(expected_hir);

        assert_eq!(
            context.database.exprs[actual],
            expected_database.exprs[expected]
        );
    }

    fn init_arenas() -> (Arena<Expr>, ArenaMap<Idx<Expr>, TextRange>) {
        let exprs = Arena::new();
        let expr_ranges = ArenaMap::default();

        (exprs, expr_ranges)
    }

    #[test]
    #[ignore = "TODO: revisit after finishing Stmt -> Expr for LetBinding"]
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
    #[ignore = "TODO: revisit after finishing Stmt -> Expr for LetBinding"]
    fn lower_let_binding() {
        let input = "let a = 1";
        let mut exprs = Arena::new();

        let expected_let_binding = LetBinding {
            ast: todo!(),
            type_annotation: None,
            value: exprs.alloc(Expr::IntLiteral(1)),
        };
        let expected_hir = Expr::LetBinding(expected_let_binding);

        check_expr(input, expected_hir);
    }

    #[test]
    #[ignore = "not being parsed correctly"]
    // currently being parsed as Stmt::VariableDef.value == Empty
    // should parse the expression still (IntLiteral 10)
    // but the identifier (pattern) is missing
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
    #[ignore = "TODO: revisit after finishing Stmt -> Expr for LetBinding"]

    fn lower_variable_def_without_value() {
        let input = "let a =";
        // let mut exprs = Arena::new();

        let expected_let_binding = LetBinding {
            ast: todo!(),
            type_annotation: None,
            value: todo!(),
        };
        let expected_hir = Expr::LetBinding(expected_let_binding);

        check_expr(input, expected_hir);
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
    fn lower_variable_ref() {
        let input = "foo";
        let expected_hir = Expr::VariableRef { name: "foo".into() };

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_paren_expr() {
        let input = "((((((abc))))))";
        let expected_hir = Expr::VariableRef { name: "abc".into() };

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
