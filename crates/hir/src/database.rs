use la_arena::{Arena, ArenaMap, Idx};
use std::fmt::{self, Write as FmtWrite};
use text_size::TextRange;

use crate::{Expr, LocalDef, Stmt};

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated statements
    stmts: Arena<Stmt>,

    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    expr_ranges: ArenaMap<Idx<Expr>, TextRange>,

    /// Local (let) bindings of variable definitions
    local_defs: Arena<LocalDef>,
}

impl Database {
    // TODO: reduce visibility after updating tests in vm::exec
    pub fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        let idx = self.exprs.alloc(expr);

        let range = ast.map_or(Default::default(), |ast| ast.range());
        self.expr_ranges.insert(idx, range);

        idx
    }

    pub(super) fn alloc_stmt(&mut self, stmt: Stmt) -> Idx<Stmt> {
        self.stmts.alloc(stmt)
    }

    pub(super) fn alloc_local(&mut self, local: LocalDef) -> Idx<LocalDef> {
        self.local_defs.alloc(local)
    }

    pub fn debug_string(&self) -> String {
        let mut s = String::new();

        let indent: usize = 0;

        for (stmt, ..) in self.stmts.iter() {
            self.write_stmt(&mut s, stmt, indent).unwrap();
        }

        s
    }

    fn write_stmt(&self, s: &mut String, idx: Idx<Stmt>, indent: usize) -> fmt::Result {
        write!(s, "{}", " ".repeat(indent))?;

        match &self.stmts[idx] {
            Stmt::VariableDef(stmt) => {
                let def = &self.local_defs[*stmt];

                write!(s, "let _{:?} = ", stmt.into_raw())?;
                self.write_expr(s, def.value, indent)
            }
            Stmt::Expr(expr) => self.write_expr(s, *expr, indent),
        }?;

        writeln!(s)
    }

    fn write_expr(&self, s: &mut String, idx: Idx<Expr>, mut indent: usize) -> fmt::Result {
        match &self.exprs[idx] {
            Expr::Empty => write!(s, "{{empty}}"),

            Expr::BoolLiteral(b) => write!(s, "{}", b),
            Expr::FloatLiteral(f) => write!(s, "{}", f),
            Expr::IntLiteral(i) => write!(s, "{}", i),
            Expr::StringLiteral(sl) => write!(s, "{}", sl),

            Expr::Call { path, args } => todo!(),

            Expr::Binary { op, lhs, rhs, .. } => {
                self.write_expr(s, *lhs, indent)?;

                write!(s, " {} ", op)?;

                self.write_expr(s, *rhs, indent)
            }

            Expr::Unary { op, expr, .. } => {
                write!(s, "{}", op)?;

                self.write_expr(s, *expr, indent)
            }

            Expr::Block { stmts, .. } => {
                writeln!(s, "{{")?;
                indent += 4;

                for idx in stmts {
                    self.write_stmt(s, *idx, indent)?;
                }

                indent -= 4;

                write!(s, "{}}}", " ".repeat(indent))
            }

            Expr::VariableRef { name, .. } => write!(s, "{}", name),

            Expr::Function {
                params,
                body,
                return_type_annotation,
            } => {
                write!(s, "fun (")?;

                for param in params {
                    self.write_expr(s, *param, indent)?;
                }

                write!(s, ") -> ")?;

                if return_type_annotation.is_some() {
                    let return_type = return_type_annotation.unwrap();
                    self.write_expr(s, return_type, indent)?;
                }

                self.write_expr(s, *body, indent)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::{BinaryOp, UnaryOp};

    use super::*;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn parse_expr(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse_expr(input).syntax()).unwrap()
    }

    fn check_stmt<S: Into<String>>(input: &str, expected_string: S) {
        let expected_string = expected_string.into();

        let root = parse(input);
        let ast = root.stmts().next().unwrap();

        let mut context = Context::default();
        context.lower_stmt(ast).unwrap();

        let actual_string = context.database.debug_string();
        assert_eq!(expected_string, actual_string);

        // TODO: check diagnostics
    }

    // fn check_stmt(input: &str, expected_hir: Stmt, expected_database: Database) {
    //     let root = parse(input);
    //     let ast = root.stmts().next().unwrap();

    //     let mut context = Context::default();
    //     let hir_idx = context.lower_stmt(ast).unwrap();
    //     let hir = context.database.stmts[hir_idx].clone();

    //     context.database.debug_string();

    //     assert_eq!(context.database, expected_database);
    //     assert_eq!(expected_hir, hir);
    // }

    fn check_expr(input: &str, expected_hir: Expr) {
        let root = parse_expr(input);
        let ast = root.expr().expect("expected a top-level expression");

        let mut context = Context::default();
        let actual = context.lower_expr(Some(ast));

        let mut expected_database = Database::default();
        let expected = expected_database.exprs.alloc(expected_hir);

        assert_eq!(
            context.database.exprs[actual],
            expected_database.exprs[expected]
        );
    }

    fn init_arenas() -> (Arena<Stmt>, Arena<Expr>, ArenaMap<Idx<Expr>, TextRange>) {
        let statements = Arena::new();
        let exprs = Arena::new();
        let expr_ranges = ArenaMap::default();

        (statements, exprs, expr_ranges)
    }

    #[test]
    fn test_lower_variable_def() {
        let input = "let a = b";
        let root = parse(input);
        let ast = root.stmts().next().unwrap();

        let mut context = Context::default();
        let hir = context.lower_stmt(ast).unwrap();

        let s = context.database.debug_string();

        dbg!(s);
    }

    #[test]
    fn lower_variable_def() {
        let input = "let a = b";
        let expected_string = "let i0 = b\n";

        check_stmt(input, expected_string)
    }

    #[test]
    #[ignore = "not being parsed correctly"]
    // currently being parsed as Stmt::VariableDef.value == Empty
    // should parse the expression still (IntLiteral 10)
    // but the identifier (pattern) is missing
    fn lower_variable_def_without_name() {
        let root = parse("let = 10");
        let ast = root.stmts().next().unwrap();

        let mut context = Context::default();
        let stmt = context.lower_stmt(ast);
        dbg!(stmt);
        let x = &context.database.stmts[stmt.unwrap()];
        match x {
            Stmt::VariableDef(d) => {
                let v = (&context.database.local_defs[*d]);
                dbg!(&context.database.exprs[v.value]);
            }
            Stmt::Expr(_) => unreachable!(),
        }
        // assert!(context.lower_stmt(ast).is_none());
    }

    #[test]
    fn lower_variable_def_without_value() {
        let input = "let a =";
        let expected_string = "let i0 = {empty}\n";

        check_stmt(input, expected_string);
    }

    #[test]
    fn lower_expr_stmt_int_literal() {
        let input = "123";
        let expected_string = "123\n";

        check_stmt(input, expected_string);
    }

    #[test]
    fn lower_expr_stmt_call() {
        let input = "print a";
        let expected_string = "print i0\n";

        check_stmt(input, expected_string);
    }

    #[test]
    fn lower_expr_stmt_empty_block() {
        let input = "{}";
        let expected_string = r#"{
}
"#;

        check_stmt(input, expected_string);
    }

    #[test]
    #[ignore = "lowering not implemented for blocks"]
    fn lower_expr_stmt_block_stmts() {
        let input = r#"{
    let a = 1
    let b = 2
    let c = 3
        }"#;
        let expected_string = r#"{
    let i0 = 1
    let i1 = 2
    let i2 = 3
}
"#;

        check_stmt(input, expected_string);
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();

        let lhs = exprs.alloc(Expr::IntLiteral(1));
        let rhs = exprs.alloc(Expr::IntLiteral(2));
        let op = BinaryOp::Add;

        check_expr("1 + 2", Expr::Binary { op, lhs, rhs });
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();

        let lhs = exprs.alloc(Expr::IntLiteral(10));
        let rhs = exprs.alloc(Expr::Empty);
        let op = BinaryOp::Sub;

        check_expr("10 -", Expr::Binary { op, lhs, rhs });
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
        let expected_hir = Expr::Unary { op, expr };

        check_expr(input, expected_hir);
    }

    #[test]
    fn lower_variable_ref() {
        let input = "foo";
        let expected_hir = Expr::VariableRef { name: "foo".into() };

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

        let expected_hir = Expr::Block { stmts: vec![] };

        check_expr(input, expected_hir)
    }

    #[test]
    fn lower_not_expr() {
        let mut exprs = Arena::new();

        let expr = exprs.alloc(Expr::BoolLiteral(true));
        let op = UnaryOp::Not;

        let input = "not true";
        let expected_hir = Expr::Unary { op, expr };

        check_expr(input, expected_hir)
    }

    // TODO: lower block tests
}
