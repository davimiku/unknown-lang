use std::str::FromStr;

use la_arena::Idx;
use parser::SyntaxKind;
use text_size::TextRange;

use crate::expr::{
    BinaryExpr, CallExpr, FunctionExpr, FunctionParam, IfExpr, LocalDefExpr, LocalRefExpr,
    LocalRefName, UnaryExpr,
};
use crate::interner::{Interner, Key};
use crate::scope::Scopes;
use crate::type_expr::{self, LocalTypeRefExpr, LocalTypeRefName, TypeExpr};
use crate::typecheck::{self, TypeDatabase, TypeDiagnostic};
use crate::{BinaryOp, BlockExpr, Database, Expr, Type, UnaryOp};

/// Character appended to names/keys to indicate it was modified or generated by the compiler
pub const COMPILER_BRAND: char = '~';

#[derive(Debug, PartialEq)]
pub enum Diagnostic {
    // Lowering(),
    Type(TypeDiagnostic),
}

#[derive(Debug)]
pub struct Context<'a> {
    /// Database holding the lowered expressions and associated data
    pub(crate) database: Database,

    /// Results of type checking and inferring expressions
    pub(crate) type_database: TypeDatabase,

    /// Diagnostics found while lowering
    pub(crate) diagnostics: Vec<Diagnostic>,

    pub(crate) scopes: Scopes,

    pub interner: &'a mut Interner,
}

impl<'a> Context<'a> {
    pub(crate) fn new(interner: &'a mut Interner) -> Self {
        let bool_key = interner.intern("Bool");
        let int_key = interner.intern("Int");
        let float_key = interner.intern("Float");
        let string_key = interner.intern("String");

        let print_key = interner.intern("print");

        let mut scopes = Scopes::new();

        let bool_key = scopes.insert_local_type(bool_key);
        let int_key = scopes.insert_local_type(int_key);
        let float_key = scopes.insert_local_type(float_key);
        let string_key = scopes.insert_local_type(string_key);

        let _print_key = scopes.insert_local(print_key);

        let mut typecheck_results = TypeDatabase::default();
        typecheck_results.set_local_type(bool_key, Type::Bool);
        typecheck_results.set_local_type(int_key, Type::Int);
        typecheck_results.set_local_type(float_key, Type::Float);
        typecheck_results.set_local_type(string_key, Type::String);

        Self {
            database: Default::default(),
            type_database: typecheck_results,
            diagnostics: Default::default(),
            scopes,
            interner,
        }
    }
}

// Public functions
impl<'a> Context<'a> {
    pub(crate) fn type_check(&mut self, root: Idx<Expr>, expected_type: &Type) {
        let inferred_result = typecheck::infer_expr(root, &mut self.type_database, &self.database);

        match inferred_result {
            Ok(_) => {
                let check_result = typecheck::check_expr(
                    root,
                    expected_type,
                    &mut self.type_database,
                    &self.database,
                );
                if let Err(err) = check_result {
                    self.diagnostics.push(err.into())
                }
            }
            Err(err) => self.diagnostics.push(err.into()),
        }
    }

    /// Returns the expression at the given index
    pub fn expr(&self, idx: Idx<Expr>) -> &Expr {
        &self.database.exprs[idx]
    }

    /// Returns the type expression at the given index
    pub fn type_expr(&self, idx: Idx<TypeExpr>) -> &TypeExpr {
        &self.database.type_exprs[idx]
    }

    pub fn lookup(&self, key: Key) -> &str {
        self.interner.lookup(key)
    }

    pub fn type_of_expr(&self, idx: Idx<Expr>) -> &Type {
        self.type_database
            .get_expr_type(idx)
            .expect("type checking to be complete")
    }

    pub fn range_of(&self, idx: Idx<Expr>) -> TextRange {
        self.database.expr_ranges[idx]
    }
}

impl<'a> Context<'a> {
    pub(crate) fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        self.database.alloc_expr(expr, ast)
    }

    pub(crate) fn alloc_type_expr(&mut self, expr: TypeExpr, range: TextRange) -> Idx<TypeExpr> {
        self.database.alloc_type_expr(expr, range)
    }

    pub(crate) fn push_scope(&mut self) {
        self.scopes.push();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

// Lowering functions - Expr
impl<'a> Context<'a> {
    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Idx<Expr> {
        use ast::Expr::*;
        let expr = if let Some(ast) = ast.clone() {
            match ast {
                Binary(ast) => self.lower_binary(ast),
                Block(ast) => self.lower_block(ast),
                BoolLiteral(ast) => self.lower_bool_literal(ast),
                Call(ast) => self.lower_call(ast),
                FloatLiteral(ast) => self.lower_float_literal(ast),
                Function(ast) => self.lower_function_expr(ast),
                If(ast) => self.lower_if_expr(ast),
                IntLiteral(ast) => self.lower_int_literal(ast),
                LetBinding(ast) => self.lower_let_binding(ast),
                Loop(ast) => self.lower_loop(ast),
                Paren(ast) => return self.lower_expr(ast.expr()),
                StringLiteral(ast) => self.lower_string_literal(ast),
                Unary(ast) => self.lower_unary(ast),
            }
        } else {
            Expr::Empty
        };

        self.alloc_expr(expr, ast)
    }

    fn lower_let_binding(&mut self, ast: ast::LetBinding) -> Expr {
        // TODO: desugar patterns into separate LocalDef
        let name = ast.name().unwrap().text().to_string();
        let name = self.interner.intern(&name);

        let key = self.scopes.insert_local(name);

        let value = self.lower_expr(ast.value());

        let type_annotation = ast
            .type_annotation()
            .map(|type_expr| self.lower_type_expr(type_expr.into()));

        Expr::LocalDef(LocalDefExpr {
            key,
            value,
            type_annotation,
        })
    }

    fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let value: Option<bool> = ast.value().and_then(|token| token.text().parse().ok());

        value.map_or(Expr::Empty, Expr::BoolLiteral)
    }

    fn lower_float_literal(&mut self, ast: ast::FloatLiteral) -> Expr {
        let value: Option<f64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(Expr::Empty, Expr::FloatLiteral)
    }

    fn lower_int_literal(&mut self, ast: ast::IntLiteral) -> Expr {
        let value: Option<i32> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(Expr::Empty, Expr::IntLiteral)
    }

    fn lower_string_literal(&mut self, ast: ast::StringLiteral) -> Expr {
        let value: Option<String> = ast.value().map(|token| token.text().to_owned());

        if let Some(s) = value {
            let s = &s[1..s.len() - 1]; // remove leading and trailing quotes

            let key = self.interner.intern(s);
            Expr::StringLiteral(key)
        } else {
            Expr::Empty
        }
    }

    fn lower_binary(&mut self, ast: ast::Binary) -> Expr {
        let op = match ast.op().expect("valid binary op token").kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Dash => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::PlusPlus => BinaryOp::Concat,
            SyntaxKind::Dot => BinaryOp::Path,
            SyntaxKind::Caret => BinaryOp::Exp,
            SyntaxKind::Percent => BinaryOp::Rem,
            _ => unreachable!(),
        };

        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        Expr::Binary(BinaryExpr { op, lhs, rhs })
    }

    fn lower_unary(&mut self, ast: ast::Unary) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Dash => UnaryOp::Neg,
            SyntaxKind::Bang => UnaryOp::Not,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary(UnaryExpr { op, expr })
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        self.push_scope();

        let exprs = ast
            .exprs()
            .map(|expr_ast| self.lower_expr(Some(expr_ast)))
            .collect();

        self.pop_scope();

        Expr::Block(BlockExpr { exprs })
    }

    fn lower_loop(&mut self, _ast: ast::Loop) -> Expr {
        self.push_scope();
        // identify break expressions
        // lower statements/expressions
        self.pop_scope();

        // break value?
        todo!()
    }

    fn lower_call(&mut self, ast: ast::Call) -> Expr {
        let path = ast.path().unwrap();
        let mut idents = path.ident_strings();

        // TODO: handle multiple idents in a Path
        let name = idents.next().unwrap();
        let key = self.interner.intern(&name);

        if let Some(call_args) = ast.args() {
            let args = call_args
                .args()
                .map(|expr| self.lower_expr(Some(expr)))
                .collect();

            let local_key = self.scopes.find_local(key);
            // FIXME: handle None scenario
            let local_key = local_key.unwrap();

            // TODO: check for mismatched arg count?
            Expr::Call(CallExpr {
                callee: local_key,
                callee_path: name,
                args,
            })
        } else {
            self.lower_name_ref(key)
        }
    }

    fn lower_name_ref(&mut self, name: Key) -> Expr {
        let local_key = self.scopes.find_local(name);

        let local_ref = if let Some(local_key) = local_key {
            LocalRefExpr {
                name: LocalRefName::Resolved(local_key),
            }
        } else {
            LocalRefExpr {
                name: LocalRefName::Unresolved(name),
            }
        };

        Expr::LocalRef(local_ref)
    }

    fn lower_function_expr(&mut self, function_ast: ast::Function) -> Expr {
        self.push_scope();
        let params = function_ast
            .param_list()
            .params()
            .map(|param| {
                let ident = param.ident().expect("function parameter to have ident");

                let key = self.interner.intern(&ident);
                let key = self.scopes.insert_local(key);

                let ty = param
                    .type_expr()
                    .map(|type_expr| self.lower_type_expr(Some(type_expr)));

                FunctionParam { name: key, ty }
            })
            .collect();

        let body = function_ast.body().map(|body| self.lower_expr(Some(body)));
        let body = body.expect("TODO: handle missing function body");
        self.pop_scope();

        Expr::Function(FunctionExpr { params, body })
    }

    fn lower_if_expr(&mut self, ast: ast::If) -> Expr {
        let condition = self.lower_expr(ast.condition_expr());

        self.push_scope();
        let then_branch = self.lower_expr(ast.then_branch());
        self.pop_scope();

        let else_branch = ast.else_branch().map(|else_branch| {
            self.push_scope();
            let lowered = self.lower_expr(Some(else_branch));
            self.pop_scope();
            lowered
        });

        Expr::If(IfExpr {
            condition,
            then_branch,
            else_branch,
        })
    }
}

// Lowering functions - TypeExpr
impl<'a> Context<'a> {
    fn lower_type_expr(&mut self, ast: Option<ast::TypeExpr>) -> Idx<TypeExpr> {
        use ast::TypeExpr::*;
        if let Some(ast) = ast {
            let range = ast.range();
            let type_expr = match ast {
                BoolLiteral(ast) => self.lower_type_bool_literal(ast),
                FloatLiteral(ast) => self.lower_type_float_literal(ast),
                IntLiteral(ast) => self.lower_type_int_literal(ast),
                StringLiteral(ast) => self.lower_type_string_literal(ast),

                Function(_) => todo!(),
                Path(_) => {
                    panic!("TODO: maybe Path should never be lowered directly, always via Call")
                }
                Call(ast) => self.lower_type_call(ast),
            };
            self.alloc_type_expr(type_expr, range)
        } else {
            self.alloc_type_expr(TypeExpr::Empty, TextRange::default())
        }
    }

    fn lower_type_bool_literal(&mut self, ast: ast::BoolLiteral) -> TypeExpr {
        let value: Option<bool> = ast.value().and_then(|token| token.text().parse().ok());

        value.map_or(TypeExpr::Empty, TypeExpr::BoolLiteral)
    }

    fn lower_type_float_literal(&mut self, ast: ast::FloatLiteral) -> TypeExpr {
        let value: Option<f64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(TypeExpr::Empty, TypeExpr::FloatLiteral)
    }

    fn lower_type_int_literal(&mut self, ast: ast::IntLiteral) -> TypeExpr {
        let value: Option<i32> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(TypeExpr::Empty, TypeExpr::IntLiteral)
    }

    fn lower_type_string_literal(&mut self, ast: ast::StringLiteral) -> TypeExpr {
        let value: Option<String> = ast.value().map(|token| token.text().to_owned());

        if let Some(s) = value {
            let s = &s[1..s.len() - 1]; // remove leading and trailing quotes

            let key = self.interner.intern(s);
            TypeExpr::StringLiteral(key)
        } else {
            TypeExpr::Empty
        }
    }

    fn lower_type_call(&mut self, ast: ast::Call) -> TypeExpr {
        let path = ast.path().unwrap();
        let mut idents = path.ident_strings();

        // TODO: handle multiple idents in a Path
        let name = idents.next().unwrap();

        if let Some(call_args) = ast.args() {
            let args = call_args
                .type_args()
                .map(|expr| self.lower_type_expr(Some(expr)))
                .collect();

            TypeExpr::Call(type_expr::CallExpr { path: name, args })
        } else {
            self.lower_type_name_ref(name)
        }
    }

    fn lower_type_name_ref(&mut self, name: String) -> TypeExpr {
        let name = self.interner.intern(&name);

        let local_key = self.scopes.find_local_type(name);

        let name = match local_key {
            Some(local_key) => LocalTypeRefName::Resolved(local_key),
            None => LocalTypeRefName::Unresolved(name),
        };

        TypeExpr::LocalRef(LocalTypeRefExpr { name })
    }
}

fn parse_ignore_underscore<T: FromStr>(s: &str) -> Option<T> {
    let mut s = s.to_string();
    s.retain(|c| c != '_');

    s.parse().ok()
}
