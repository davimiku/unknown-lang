use std::str::FromStr;

use la_arena::Idx;
use parser::SyntaxKind;

use crate::scope::Scopes;
use crate::typecheck::TypeCheckResults;
use crate::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Database, Expr, IfExpr, LetBinding, Type, UnaryExpr,
    UnaryOp,
};

// temp
pub type Diagnostic = ();

#[derive(Debug, Default)]
pub struct Context {
    /// Database holding the lowered expressions and associated data
    pub(crate) database: Database,

    /// Results of type checking and inferring expressions
    pub(crate) typecheck_results: TypeCheckResults,

    /// Diagnostics found while lowering
    pub(crate) diagnostics: Vec<Diagnostic>,

    current_scope_idx: usize,

    pub(crate) scopes: Scopes,
}

// Constructors
impl Context {
    pub fn new() -> Self {
        Self::default()
    }
}

// Scope-related functions
impl Context {
    /// Returns the expression at the given index
    pub fn expr(&self, idx: Idx<Expr>) -> &Expr {
        &self.database.exprs[idx]
    }

    pub fn local_def(&self, idx: Idx<LetBinding>) -> &LetBinding {
        &self.database.let_bindings[idx]
    }

    pub fn type_of(&self, idx: Idx<Expr>) -> &Type {
        &self.typecheck_results[idx]
    }

    // TODO: probably remove this, it's only used for temporary tests in the vm crate
    // that should be removed later.
    pub fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        self.database.alloc_expr(expr, ast)
    }

    pub(crate) fn insert_local_def(&mut self, name: String, idx: Idx<LetBinding>) {
        self.scopes
            .insert_local_def(self.current_scope_idx, name, idx)
    }

    pub(crate) fn lookup_name(&self, name: &str) -> Option<Idx<LetBinding>> {
        self.scopes
            .iter_from(self.current_scope_idx)
            .find_map(|scope| scope.get_local(name))
    }

    pub(crate) fn push_scope(&mut self) {
        self.current_scope_idx = self.scopes.push(self.current_scope_idx);
    }

    pub(crate) fn pop_scope(&mut self) {
        self.current_scope_idx = self.scopes.get_parent_idx(self.current_scope_idx);
    }
}

// Lowering functions
impl Context {
    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Idx<Expr> {
        use ast::Expr::*;
        let expr = if let Some(ast) = ast.clone() {
            match ast {
                Binary(ast) => self.lower_binary(ast),
                Block(ast) => self.lower_block(ast),
                BoolLiteral(ast) => self.lower_bool_literal(ast),
                Call(ast) => self.lower_call(ast),
                FloatLiteral(ast) => self.lower_float_literal(ast),
                Function(ast) => self.lower_function_def(ast),
                Ident(ast) => self.lower_ident(ast),
                If(ast) => self.lower_if_expr(ast),
                IntLiteral(ast) => self.lower_int_literal(ast),
                LetBinding(ast) => self.lower_let_binding(ast),
                Loop(ast) => self.lower_loop(ast),
                Paren(ast) => return self.lower_expr(ast.expr()),
                StringLiteral(ast) => self.lower_string_literal(ast),
                TypeExpr(ast) => self.lower_type_expr(ast),
                Unary(ast) => self.lower_unary(ast),
            }
        } else {
            Expr::Empty
        };

        self.database.alloc_expr(expr, ast)
    }

    fn lower_let_binding(&mut self, ast: ast::LetBinding) -> Expr {
        let name = ast.name();
        let value = self.lower_expr(ast.value());
        let type_annotation = None;
        let let_binding = LetBinding {
            value,
            type_annotation,
            ast,
        };
        let idx = self.database.alloc_let_binding(let_binding);

        if let Some(name) = name {
            let name = name.text().to_string();
            self.insert_local_def(name, idx);
        }
        // insert into scopes .insert(name, idx)

        // Expr::LetBinding(idx)
        todo!();
    }

    fn lower_type_expr(&mut self, ast: ast::TypeExpr) -> Expr {
        todo!()
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

        Expr::Binary(BinaryExpr { op, lhs, rhs })
    }

    fn lower_unary(&mut self, ast: ast::Unary) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Dash => UnaryOp::Neg,
            SyntaxKind::Not => UnaryOp::Not,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary(UnaryExpr { op, expr })
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        // create a new scope?
        let exprs = ast
            .exprs()
            .map(|expr_ast| self.lower_expr(Some(expr_ast)))
            .collect();

        Expr::Block(BlockExpr { exprs })
    }

    fn lower_loop(&mut self, ast: ast::LoopExpr) -> Expr {
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
            name: ast.name_token().unwrap().text().into(),
        }
    }

    fn lower_call(&mut self, ast: ast::Call) -> Expr {
        let path = ast.path().unwrap();
        let mut idents = path.ident_strings();

        // TODO: handle multiple idents in a Path :)
        let name = idents.next().unwrap();
        let call_args = ast.args();

        if let Some(call_args) = call_args {
            let args = call_args
                .args()
                .map(|expr| self.lower_expr(Some(expr.clone())))
                .collect();

            // TODO: check for mismatched arg count?
            Expr::Call(CallExpr { path: name, args })
        } else {
            Expr::VariableRef { name }
        }
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

    fn lower_if_expr(&mut self, ast: ast::IfExpr) -> Expr {
        let condition = self.lower_expr(ast.condition_expr());
        let then_branch = self.lower_expr(ast.then_branch());
        let else_branch = ast
            .else_branch()
            .map(|else_branch| self.lower_expr(Some(else_branch)));

        Expr::If(IfExpr {
            condition,
            then_branch,
            else_branch,
        })
    }
}

fn parse_ignore_underscore<T: FromStr>(s: &str) -> Option<T> {
    let mut s = s.to_string();
    s.retain(|c| c != '_');

    s.parse().ok()
}
