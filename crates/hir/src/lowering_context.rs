use std::fmt;
use std::str::FromStr;

use itertools::Itertools;
use la_arena::Idx;
use parser::SyntaxKind;
use text_size::TextRange;
use util_macros::assert_matches;

use crate::diagnostic::Diagnostic;
use crate::expr::{
    ArrayLiteralExpr, FunctionExpr, FunctionParam, IfExpr, IndexIntExpr, IntrinsicExpr, UnaryExpr,
    VarRefExpr,
};
use crate::interner::{Interner, Key};
use crate::scope::ModuleScopes;
use crate::type_expr::{CallExpr, TypeExpr, TypeRefExpr, TypeSymbol};
use crate::typecheck::{check_expr, infer_expr, CoreTypes, TypeDatabase};
use crate::{BlockExpr, ContextDisplay, Database, Expr, Type, UnaryOp, ValueSymbol};

/// Character appended to names/keys to indicate it was modified or generated by the compiler
pub const COMPILER_BRAND: char = '~';

pub const CORE_MODULE_ID: u32 = 0;

// TODO: rename to LoweringContext? and have a TypecheckContext?
#[derive(Debug)]
pub struct Context {
    /// Database holding the lowered expressions and associated data
    pub(crate) database: Database,

    /// Results of type checking and inferring expressions
    pub(crate) type_database: TypeDatabase,

    /// Diagnostics found while lowering
    pub diagnostics: Vec<Diagnostic>,

    pub(crate) module_scopes: Vec<ModuleScopes>,

    pub(crate) current_module_id: u32,

    pub interner: Interner,

    // HACK: when lowering a let binding for a function like this:
    // `let identity = fun (i: Int) -> { i }`
    // It's useful to store the Key/Symbol of "identity" with the FuncExpr for later use
    // This is reset to None at the start of lowering each "statement"
    // TODO: doesn't feel great to introduce mutable state like this to track
    // Do we already have this information available in the AST?
    current_let_binding_symbol: Option<(Key, ValueSymbol)>,
}

impl Context {
    pub fn new(interner: Interner) -> Self {
        let mut database = Database::default();
        let mut current_module_id = 0;
        let mut scopes = ModuleScopes::new(current_module_id);
        let mut type_database = TypeDatabase::default();

        // Populate "module 0" with builtins
        {
            let t = type_database.core.clone();
            let keys = interner.core_keys();

            macro_rules! core_type {
                ($key:expr, $ty:expr) => {
                    let symbol = scopes.insert_type($key);
                    database.type_names.insert(symbol, $key);
                    type_database.insert_type_symbol(symbol, $ty);
                };
            }

            core_type!(keys.bool, t.bool);
            core_type!(keys.int, t.int);
            core_type!(keys.float, t.float);
            core_type!(keys.string, t.string);

            macro_rules! core_value {
                ($key:expr, $fn_type:expr) => {{
                    let symbol = scopes.insert_value($key);
                    database.value_names.insert(symbol, $key);
                    let ty = type_database.alloc_type($fn_type);
                    type_database.insert_value_symbol(symbol, ty);

                    symbol
                }};
                ($key:expr, $fn_type:expr, $intrinsic:ident) => {{
                    let symbol = scopes.insert_value($key);
                    database.value_names.insert(symbol, $key);
                    let ty = type_database.alloc_type($fn_type);
                    type_database.insert_value_symbol(symbol, ty);

                    let intrinsic_expr =
                        database.alloc_expr(Expr::Intrinsic(IntrinsicExpr::$intrinsic), None);
                    database.function_defs.insert(symbol, intrinsic_expr);
                }};
            }

            core_value!(keys.print, Type::func(vec![t.string], t.unit));
            core_value!(keys.args, Type::array_of(t.string));
            core_value!(keys.add, Type::func(vec![t.int, t.int], t.int), Add);
            core_value!(keys.sub, Type::func(vec![t.int, t.int], t.int), Sub);
            core_value!(keys.mul, Type::func(vec![t.int, t.int], t.int), Mul);
            core_value!(keys.div, Type::func(vec![t.int, t.int], t.int), Div);
            core_value!(keys.concat, Type::func(vec![t.string, t.string], t.string));

            core_value!(keys.eq, Type::func(vec![t.int, t.int], t.bool), Eq);
            core_value!(keys.ne, Type::func(vec![t.int, t.int], t.bool), Ne);
            core_value!(keys.lt, Type::func(vec![t.int, t.int], t.bool), Lt);
            core_value!(keys.le, Type::func(vec![t.int, t.int], t.bool), Le);
            core_value!(keys.gt, Type::func(vec![t.int, t.int], t.bool), Gt);
            core_value!(keys.ge, Type::func(vec![t.int, t.int], t.bool), Ge);
        }

        current_module_id += 1;

        Self {
            database,
            type_database,
            diagnostics: Default::default(),
            current_module_id,
            module_scopes: vec![scopes, ModuleScopes::new(current_module_id)],
            interner,
            current_let_binding_symbol: None,
        }
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let database = self.database.display(self);
        f.write_str(&database)?;

        let type_database = self.type_database.display(self);
        f.write_str(&type_database)?;

        let scopes = self.current_scopes().display(self);
        f.write_str(&scopes)
    }
}

impl Context {
    pub(crate) fn type_check(&mut self, root: Idx<Expr>, expected_type: Idx<Type>) {
        let mut result = infer_expr(root, self);

        if result.is_ok() {
            result.push_result(check_expr(root, expected_type, self));
        }

        self.diagnostics.append(&mut result.diagnostics())
    }
}

// Public functions
// TODO: standardize on naming conventions and input/output types / signature
// Current thought is "foo" means get a &Foo and "foo_idx" means get a Idx<Foo>
impl Context {
    /// Returns the expression at the given index
    pub fn expr(&self, idx: Idx<Expr>) -> &Expr {
        &self.database.exprs[idx]
    }

    /// Returns the type expression at the given index
    pub fn type_expr(&self, idx: Idx<TypeExpr>) -> &TypeExpr {
        &self.database.type_exprs[idx]
    }

    /// Gets an interned key by its string
    //
    // TODO: keep this public? or offer more specific getters only
    pub fn lookup(&self, key: Key) -> &str {
        self.interner.lookup(key)
    }

    /// Gets a reference to the type for an expression
    pub fn expr_type(&self, idx: Idx<Expr>) -> &Type {
        self.type_database.type_(self.expr_type_idx(idx))
    }

    /// Gets the index of a type for an expression
    pub fn expr_type_idx(&self, idx: Idx<Expr>) -> Idx<Type> {
        self.type_database.get_expr_type(idx)
    }

    pub fn type_(&self, idx: Idx<Type>) -> &Type {
        self.type_database.type_(idx)
    }

    pub fn type_of_value(&self, value: &ValueSymbol) -> &Type {
        self.type_database.type_(self.type_idx_of_value(value))
    }

    pub fn type_idx_of_value(&self, value: &ValueSymbol) -> Idx<Type> {
        self.type_database.get_value_symbol(value)
    }

    pub fn range_of_expr(&self, idx: Idx<Expr>) -> TextRange {
        self.database.expr_ranges[idx]
    }

    pub fn range_of_type_expr(&self, idx: Idx<TypeExpr>) -> TextRange {
        self.database.type_expr_ranges[idx]
    }

    pub fn find_local(&mut self, name: &str) -> Option<ValueSymbol> {
        let key = self.interner.intern(name);

        self.find_value(key)
    }

    /// Gets the interned string for a value symbol
    pub fn value_symbol_str(&self, symbol: ValueSymbol) -> &str {
        let key = self.database.value_names[&symbol];
        self.lookup(key)
    }

    /// Gets the interned string for a type symbol
    pub fn type_symbol_str(&self, symbol: TypeSymbol) -> &str {
        let key = self.database.type_names[&symbol];
        self.lookup(key)
    }

    pub fn lookup_function_symbol(&self, symbol: ValueSymbol) -> Option<Idx<Expr>> {
        self.database.function_def(symbol)
    }

    pub fn core_types(&self) -> &CoreTypes {
        &self.type_database.core
    }
}

impl Context {
    fn find_value(&self, name: Key) -> Option<ValueSymbol> {
        self.current_scopes()
            .find_value(name)
            .or_else(|| self.core_scopes().find_value(name))
    }

    fn find_type(&self, name: Key) -> Option<TypeSymbol> {
        self.current_scopes()
            .find_type(name)
            .or_else(|| self.core_scopes().find_type(name))
    }

    fn current_scopes(&self) -> &ModuleScopes {
        &self.module_scopes[self.current_module_id as usize]
    }

    fn core_scopes(&self) -> &ModuleScopes {
        &self.module_scopes[CORE_MODULE_ID as usize]
    }

    fn current_scopes_mut(&mut self) -> &mut ModuleScopes {
        &mut self.module_scopes[self.current_module_id as usize]
    }
}

impl Context {
    pub(crate) fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        self.database.alloc_expr(expr, ast)
    }

    pub(crate) fn alloc_type_expr(&mut self, expr: TypeExpr, range: TextRange) -> Idx<TypeExpr> {
        self.database.alloc_type_expr(expr, range)
    }

    pub(crate) fn push_scope(&mut self) {
        self.current_scopes_mut().push();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.current_scopes_mut().pop();
    }
}

// Lowering functions - Expr
impl Context {
    pub(crate) fn lower_expr_statement(&mut self, ast: Option<ast::Expr>) -> Idx<Expr> {
        self.current_let_binding_symbol = None;
        let inner = self.lower_expr(ast.clone());

        match self.expr(inner) {
            Expr::VarDef(_) => inner,
            _ => {
                let statement = Expr::Statement(inner);
                self.alloc_expr(statement, ast)
            }
        }
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Idx<Expr> {
        use ast::Expr as E;
        let expr = if let Some(ast) = ast.clone() {
            match ast {
                E::ArrayLiteral(ast) => self.lower_array_literal(ast),
                E::Binary(ast) => self.lower_binary(ast),
                E::Block(ast) => self.lower_block(ast),
                E::BoolLiteral(ast) => self.lower_bool_literal(ast),
                E::Call(ast) => self.lower_call(ast),
                E::FloatLiteral(ast) => self.lower_float_literal(ast),
                E::Function(ast) => self.lower_function_expr(ast),
                E::ForInLoop(ast) => self.lower_for_in_loop(ast),
                E::Ident(ast) => self.lower_name_ref(&ast.as_string()),
                E::If(ast) => self.lower_if_expr(ast),
                E::IntLiteral(ast) => self.lower_int_literal(ast),
                E::LetBinding(ast) => self.lower_let_binding(ast),
                E::Loop(ast) => self.lower_loop(ast),
                E::Paren(ast) => return self.lower_expr(ast.expr()),
                E::Path(ast) => self.lower_path(ast),
                E::Return(ast) => self.lower_return_statement(ast),
                E::StringLiteral(ast) => self.lower_string_literal(ast),
                E::Unary(ast) => self.lower_unary(ast),
            }
        } else {
            Expr::Empty
        };

        self.alloc_expr(expr, ast)
    }

    fn lower_let_binding(&mut self, ast: ast::LetBinding) -> Expr {
        // TODO: desugar patterns into separate definitions
        let name = ast.name().unwrap().text().to_string();
        let key = self.interner.intern(&name);

        let symbol = self.current_scopes_mut().insert_value(key);
        self.database.value_names.insert(symbol, key);
        self.current_let_binding_symbol = Some((key, symbol));

        let value = self.lower_expr(ast.value());

        let type_annotation = ast
            .type_annotation()
            .map(|type_expr| self.lower_type_expr(type_expr.into()));

        if let Expr::Function(_) = self.expr(value) {
            self.database.function_defs.insert(symbol, value);
        }

        Expr::variable_def(symbol, value, type_annotation)
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
        let value: Option<i64> = ast
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

    fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> Expr {
        let elements = ast
            .items()
            .map(|item| self.lower_expr(Some(item)))
            .collect_vec();

        Expr::ArrayLiteral(match elements.len() {
            0 => ArrayLiteralExpr::Empty,
            _ => ArrayLiteralExpr::NonEmpty { elements },
        })
    }

    fn lower_binary(&mut self, ast: ast::Binary) -> Expr {
        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        let op = match ast.op().expect("valid binary op token").kind() {
            SyntaxKind::Plus => "+",
            SyntaxKind::Dash => "-",
            SyntaxKind::Star => "*",
            SyntaxKind::Slash => "/",
            SyntaxKind::PlusPlus => "++",
            SyntaxKind::Dot => ".",
            SyntaxKind::Caret => "^",
            SyntaxKind::Percent => "%",
            SyntaxKind::EqualsEquals => "==",
            SyntaxKind::BangEquals => "!=",
            SyntaxKind::LAngle => "<",
            SyntaxKind::LAngleEquals => "<=",
            SyntaxKind::RAngle => ">",
            SyntaxKind::RAngleEquals => ">=",
            _ => unreachable!(),
        };
        let callee = self.lower_name_ref(op);
        let symbol = assert_matches!(&callee, Expr::VarRef).symbol;
        let callee = self.alloc_expr(callee, Some(ast::Expr::Binary(ast)));

        Expr::call(callee, Box::new([lhs, rhs]), Some(symbol))
    }

    fn lower_unary(&mut self, ast: ast::Unary) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Dash => UnaryOp::Neg,
            SyntaxKind::Bang => UnaryOp::Not,
            SyntaxKind::Tilde => UnaryOp::IntoString,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary(UnaryExpr { op, expr })
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        self.push_scope();

        let exprs: Vec<Idx<Expr>> = ast
            .exprs()
            .map(|expr_ast| self.lower_expr_statement(Some(expr_ast)))
            .collect();

        self.pop_scope();

        if exprs.is_empty() {
            Expr::Block(BlockExpr::Empty)
        } else {
            Expr::Block(BlockExpr::NonEmpty { exprs })
        }
    }

    /// ```txt
    /// for el in arr {
    ///     //
    /// }
    /// ```
    ///
    /// translates to:
    ///
    /// ```txt
    /// {
    ///     let mutable _index = 0
    ///     let _len = arr.len
    ///     loop {
    ///         if _index >= _len { break }
    ///         let el = arr _index
    ///         //
    ///         _index += 1
    ///     }
    /// }
    /// ```
    fn lower_for_in_loop(&mut self, ast: ast::ForInLoop) -> Expr {
        self.push_scope();

        // inject `let mutable _index = 0`
        // let index_key = self
        //     .current_scopes()
        //     .insert_value(self.interner.intern("_index"));
        // let index_value = self.alloc_expr(Expr::IntLiteral(0), None);
        // let index_def: Idx<Expr> =
        //     self.alloc_expr(Expr::variable_def(index_key, index_value, None), None);

        // // inject `let _len = arr.len`
        // let len_key = self
        //     .current_scopes()
        //     .insert_value(self.interner.intern("_len"));
        // let len_value = self.alloc_expr(
        //     Expr::call(
        //         todo!(), // `Array.len` from global|builtin|primordial scope
        //         todo!(), // array path|literal from ast
        //         vec![],
        //     ),
        //     None,
        // );
        // let len_def = self.alloc_expr(Expr::variable_def(len_key, len_value, None), None);

        todo!()
    }

    fn lower_loop(&mut self, _ast: ast::Loop) -> Expr {
        self.push_scope();
        // identify break expressions
        // lower statements/expressions
        self.pop_scope();

        // break value?
        todo!()
    }

    fn lower_call(&mut self, ast: ast::CallExpr) -> Expr {
        let callee = ast.callee().unwrap();
        let symbol = match &callee {
            ast::Expr::Ident(ident) => {
                let key = self.interner.intern(&ident.as_string());
                self.find_value(key)
            }
            ast::Expr::Path(path) => None,

            _ => None,
        };
        let callee = self.lower_expr(Some(callee));

        // TODO: confirm new definition of ast::CallExpr always has args
        let call_args = ast.args().unwrap();
        let args = call_args
            .args()
            .map(|expr| self.lower_expr(Some(expr)))
            .collect();

        // TODO: check for mismatched arg count here, or during typecheck?
        Expr::call(callee, args, symbol)
    }

    fn lower_path(&mut self, path: ast::PathExpr) -> Expr {
        // `a`
        // -> subject: `a`
        // -> member: None

        // `a.b.c` (MemberExpr)
        // -> subject: `a.b`
        // -> member: `.c`

        // `a.b.1` (IndexExpr)
        // -> subject: `a.b`
        // -> index: `1` (Int)

        // `loc.point."x"` (IndexExpr)
        // -> subject: `loc.point`
        // -> index: `"x"` (String)

        // `arr.(mid)` (IndexExpr)
        // -> subject: `arr`
        // -> index: `mid` (variable)

        // `arr.(point.x)` (IndexExpr)
        // -> subject: `arr`
        // -> index: `point.x` (path)

        if let Some(member) = path.member() {
            let subject = self.lower_expr(path.subject());
            let member = self.lower_expr(Some(member));
            //

            match self.expr(member) {
                // Index
                Expr::IntLiteral(_) => Expr::IndexInt(IndexIntExpr {
                    subject,
                    index: member,
                }),
                Expr::StringLiteral(_) => todo!(),
                Expr::Path(_) => todo!(),

                Expr::Unary(_) => todo!(), // arr.(-x)     // ??

                Expr::Call(_) => todo!(), // arr.(max arr2) // ??

                // How do we distinguish between `a.b` (member) and `a.(b)` (index) ?
                Expr::VarRef(_) => todo!(),

                Expr::Block(_) => todo!(), // technically allowed? Or prevent in AST
                Expr::If(_) => todo!(),    // technically allowed? Or prevent in AST

                _ => todo!(),
            }
        } else {
            let subject = path
                .subject_as_ident()
                .expect("left side of path to be an ident");

            self.lower_name_ref(&subject.as_string())
        }
    }

    fn lower_index_expr(&mut self, subject: Expr, index: Expr) -> Expr {
        todo!("didn't implement index expressions yet")
    }

    fn lower_name_ref(&mut self, name: &str) -> Expr {
        let key = self.interner.intern(name);

        let value_symbol = self.find_value(key);

        if let Some(symbol) = value_symbol {
            Expr::VarRef(VarRefExpr { symbol })
        } else {
            Expr::UnresolvedVarRef { key }
        }
    }

    fn lower_function_expr(&mut self, function_ast: ast::Function) -> Expr {
        let func_name = self.current_let_binding_symbol;
        self.push_scope();
        let params = function_ast
            .param_list()
            .params()
            .map(|param| {
                let ident = param.ident().expect("function parameter to have ident");

                let name = self.interner.intern(&ident.as_string());
                let symbol = self.current_scopes_mut().insert_value(name);
                self.database.value_names.insert(symbol, name);

                let annotation = param
                    .type_expr()
                    .map(|type_expr| self.lower_type_expr(Some(type_expr)));

                FunctionParam {
                    name,
                    symbol,
                    annotation,
                }
            })
            .collect_vec();

        let body = function_ast
            .body()
            .and_then(|body| body.expr())
            .map(|body| self.lower_expr(Some(body)));
        let body = body.expect("TODO: handle missing function body");
        self.pop_scope();

        // let name = function_ast.name().map(|ast_name| {
        //     let key = self.interner.intern(&ast_name);
        //     let symbol = self.current_scopes_mut().insert_value(key);
        //     self.database.value_names.insert(symbol, key);
        //     (key, symbol)
        // });

        Expr::Function(FunctionExpr {
            params,
            body,
            name: func_name,
        })
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

    fn lower_return_statement(&mut self, ast: ast::ReturnStatement) -> Expr {
        let return_value = ast.return_value();
        // TODO: None variant should be substituted with Unit value?
        let return_value = self.lower_expr(return_value);

        Expr::ReturnStatement(return_value)
    }
}

// Lowering functions - TypeExpr
impl Context {
    fn lower_type_expr(&mut self, ast: Option<ast::TypeExpr>) -> Idx<TypeExpr> {
        use ast::TypeExpr as TE;
        if let Some(ast) = ast {
            let range = ast.range();
            let type_expr = match ast {
                TE::BoolLiteral(ast) => self.lower_type_bool_literal(ast),
                TE::FloatLiteral(ast) => self.lower_type_float_literal(ast),
                TE::IntLiteral(ast) => self.lower_type_int_literal(ast),
                TE::StringLiteral(ast) => self.lower_type_string_literal(ast),
                TE::Ident(ast) => self.lower_type_ident(ast),
                TE::Function(_) => todo!(),
                TE::Path(ast) => self.lower_type_path(ast),
                TE::Call(ast) => self.lower_type_call(ast),
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
        let value: Option<i64> = ast
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

    fn lower_type_call(&mut self, ast: ast::CallExpr) -> TypeExpr {
        // let path = ast.path().unwrap();
        // let mut idents = path.ident_strings();

        // TODO: handle multiple idents in a Path
        // let name = idents.next().unwrap();

        let name = String::new();

        if let Some(call_args) = ast.args() {
            let args = call_args
                .type_args()
                .map(|expr| self.lower_type_expr(Some(expr)))
                .collect();

            TypeExpr::Call(CallExpr { path: name, args })
        } else {
            let key = self.interner.intern(&name);
            self.lower_type_name_ref(key)
        }
    }

    fn lower_type_path(&mut self, path: ast::TypePathExpr) -> TypeExpr {
        todo!()
        // TODO: handling proper paths, i.e. `a.b.1`
        // let name = path.ident_strings().next().unwrap();
        // let key = self.interner.intern(&name);

        // self.lower_type_name_ref(key)
    }

    fn lower_type_ident(&mut self, ident: ast::Ident) -> TypeExpr {
        let key = self.interner.intern(&ident.as_string());

        self.lower_type_name_ref(key)
    }

    fn lower_type_name_ref(&mut self, key: Key) -> TypeExpr {
        match self.find_type(key) {
            Some(symbol) => TypeExpr::VarRef(TypeRefExpr { key, symbol }),
            None => TypeExpr::UnresolvedVarRef { key },
        }
    }
}

fn parse_ignore_underscore<T: FromStr>(s: &str) -> Option<T> {
    let mut s = s.to_string();
    s.retain(|c| c != '_');

    s.parse().ok()
}
