use std::{collections::HashMap, str::FromStr};

use la_arena::Idx;
use parser::SyntaxKind;

use crate::{BinaryOp, Database, Expr, LocalDef, Stmt, UnaryOp};

// temp
pub(crate) type Diagnostic = ();

#[derive(Debug)]
pub(crate) struct Scope {
    // TODO: module scope has no parent. Represent as -1, 0, or change to Option<usize> ?
    // or make "above module" as 0, and module scope starts at 1
    parent_idx: usize,

    local_defs: HashMap<String, Idx<LocalDef>>,
}

impl Scope {
    pub(crate) fn get(&self, key: &str) -> Option<Idx<LocalDef>> {
        self.local_defs.get(key).copied()
    }
}

#[derive(Debug, Default)]
pub(crate) struct Context {
    /// Database holding the lowered Stmt/Expr and associated data
    pub(crate) database: Database,

    /// Diagnostics found while lowering
    pub(crate) diagnostics: Vec<Diagnostic>,

    current_scope_idx: usize,

    pub(crate) scopes: Vec<Scope>,
}

// Scope-related functions
impl Context {
    pub(crate) fn insert_local_def(&mut self, name: String, idx: Idx<LocalDef>) {
        let current_scope = &mut self.scopes[self.current_scope_idx];

        current_scope.local_defs.insert(name, idx);
    }

    pub(crate) fn lookup_name(&self, name: &str) -> Option<Idx<LocalDef>> {
        // TODO: could newtype Vec<Scope> as Scopes and implement Iter
        let mut idx = self.current_scope_idx;
        loop {
            if idx == 0 {
                break None;
            }

            let scope = &self.scopes[idx];

            if let Some(value) = scope.get(name) {
                break Some(value);
            }

            idx = scope.parent_idx;
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.scopes.push(Scope {
            parent_idx: self.current_scope_idx,
            local_defs: Default::default(),
        });

        self.current_scope_idx = self.scopes.len();
    }

    pub(crate) fn pop_scope(&mut self) {
        let current_scope = &mut self.scopes[self.current_scope_idx];

        self.current_scope_idx = current_scope.parent_idx;
    }
}

// Lowering functions
impl Context {
    pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Idx<Stmt>> {
        let stmt = match ast {
            ast::Stmt::VariableDef(ast) => self.lower_variable_def(ast),
            ast::Stmt::Expr(ast) => {
                let idx = self.lower_expr(Some(ast));

                Stmt::Expr(idx)
            }
        };

        let idx = self.database.alloc_stmt(stmt);

        Some(idx)
    }

    fn lower_variable_def(&mut self, ast: ast::VariableDef) -> Stmt {
        let name = ast.name();
        let value = self.lower_expr(ast.value());
        let local = LocalDef { value, ast };
        let idx = self.database.alloc_local(local);

        if let Some(name) = name {
            let name = name.text().to_string();
            self.insert_local_def(name, idx);
        }
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
                ast::Expr::Function(ast) => self.lower_function_def(ast),
                ast::Expr::Loop(ast) => self.lower_loop(ast),
                ast::Expr::IntLiteral(ast) => self.lower_int_literal(ast),
                ast::Expr::Paren(ast) => return self.lower_expr(ast.expr()),
                ast::Expr::StringLiteral(ast) => self.lower_string_literal(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Ident(ast) => self.lower_ident(ast),
            }
        } else {
            Expr::Empty
        };

        self.database.alloc_expr(expr, ast)
    }

    fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let value: Option<bool> = ast.value().and_then(|token| token.text().parse().ok());

        if let Some(value) = value {
            Expr::BoolLiteral(value)
        } else {
            Expr::Empty
        }
    }

    fn lower_float_literal(&mut self, ast: ast::FloatLiteral) -> Expr {
        let value: Option<f64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(Expr::Empty, Expr::FloatLiteral)
    }

    fn lower_int_literal(&mut self, ast: ast::IntLiteral) -> Expr {
        let value: Option<i64> = ast
            .value()
            .and_then(|token| parse_ignore_underscore(token.text()));

        value.map_or(Expr::Empty, Expr::IntLiteral)
    }

    fn lower_string_literal(&mut self, ast: ast::StringLiteral) -> Expr {
        let value: Option<String> = ast.value().map(|token| token.text().to_owned());

        if let Some(s) = value {
            let s = &s[1..s.len() - 1]; // remove leading and trailing quotes
            Expr::StringLiteral(s.to_string())
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
            lhs_type: Default::default(),
            rhs_type: Default::default(),
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
            typ: Default::default(),
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
            typ: Default::default(),
        }
    }

    fn lower_loop(&mut self, ast: ast::Loop) -> Expr {
        dbg!(&ast);

        // create a new scope
        // identify break expressions
        // lower statements/expressions
        todo!()
    }

    // TODO: rather than Ident, Local? Call? local_or_call?
    fn lower_ident(&mut self, ast: ast::Ident) -> Expr {
        // if ast::Call doesn't have ast::Path, return Empty
        // if ast::Path doesn't have top-level-name, return Empty

        // if it has a nested name (?)  (it uses "module.function" for paths, like "io.write")
        // // Make a FullyQualifiedName (FQN) with module name + function name
        // // Look up function in world index
        // // OK (return lower_call) => update context state with other module reference, update symbol map, lower call
        // // Err (return Expr::Empty) => update symbol map, add diagnostic

        // if it is in current scope (as a non-function variable - DIFFERENT THAN MINE)
        // // check if it's trying to call a non-function as a function, add diagnostic
        // // update symbol map
        // // (return Expr::Local)

        // if it is a param of the current function
        // // update symbol map
        // // (return Expr::Param)

        // if it is a "definition" (a function or record)
        // // update symbol map
        // // (return lower_call)

        Expr::VariableRef {
            name: ast.name().unwrap().text().into(),
            typ: Default::default(),
        }
    }

    fn lower_call() {
        // check for mismatched arg count (no auto-currying it seems)
        // lower_expr each of the args
        // (return Expr::Call)
        todo!()
    }

    fn lower_function_def(&mut self, ast: ast::Function) -> Expr {
        let params = todo!();
        let body = todo!();
        let return_type_annotation = todo!();

        Expr::Function {
            params,
            body,
            return_type_annotation,
        }
    }
}

fn parse_ignore_underscore<T: FromStr>(s: &str) -> Option<T> {
    let mut s = s.to_string();
    s.retain(|c| c != '_');

    s.parse().ok()
}