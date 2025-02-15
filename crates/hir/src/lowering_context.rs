use std::fmt;
use std::str::FromStr;

use ast::Mutability;
use itertools::Itertools;
use la_arena::Idx;
use parser::SyntaxKind;
use text_size::TextRange;
use util_macros::assert_matches;

use crate::diagnostic::{Diagnostic, LoweringDiagnostic};
use crate::expr::{
    ArrayLiteralExpr, FunctionExpr, FunctionExprGroup, FunctionParam, IfExpr, IndexIntExpr,
    IntrinsicExpr, LoopExpr, MatchArm, MatchExpr, Pattern, ReAssignment, UnaryExpr, UnionNamespace,
    VarRefExpr,
};
use crate::interner::{Interner, Key};
use crate::intrinsics::insert_core_values;
use crate::scope::ModuleScopes;
use crate::type_expr::{CallExpr, TypeExpr, TypeRefExpr, TypeSymbol, UnionTypeExpr};
use crate::typecheck::{check_expr, infer_expr, infer_module, CoreTypes, TypeDatabase};
use crate::{BlockExpr, ContextDisplay, Database, Expr, Module, Type, UnaryOp, ValueSymbol};

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

    breaks_stack: BreaksStack,

    // HACK: when lowering a let binding for a function like this:
    // `let identity = fun (i: Int) -> { i }`
    // It's useful to store the Key/Symbol of "identity" with the FuncExpr for later use
    // This is reset to None at the start of lowering each "statement"
    // TODO: doesn't feel great to introduce mutable state like this to track
    // Do we already have this information available in the AST?
    // TODO: would this need to capture a pattern for `let (a, b) = ...` and
    // other such things?
    current_let_binding_symbol: Option<(Key, ValueSymbol)>,
}

impl Context {
    pub fn new(interner: Interner) -> Self {
        let mut database = Database::default();
        let mut current_module_id = 0;
        let mut scopes = ModuleScopes::new(current_module_id);
        let mut type_database = TypeDatabase::new(&interner);

        insert_core_values(&mut database, &mut type_database, &mut scopes, &interner);

        current_module_id += 1;

        Self {
            database,
            type_database,
            current_module_id,
            module_scopes: vec![scopes, ModuleScopes::new(current_module_id)],
            interner,
            diagnostics: Default::default(),
            breaks_stack: Default::default(),
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
    pub(crate) fn type_check_module(&mut self, module: &Module) {
        // TODO: raise a diagnostic if `main` returns `bottom`?
        let result = infer_module(module, self);

        self.diagnostics.append(&mut result.diagnostics());
    }

    pub(crate) fn type_check(&mut self, root: Idx<Expr>, expected_type: Idx<Type>) {
        let mut result = infer_expr(root, self);

        if result.is_ok() {
            result.push_result(check_expr(root, expected_type, self));
        }

        self.diagnostics.append(&mut result.diagnostics());
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

    /// Returns a mutable expression at the given index
    pub fn expr_mut(&mut self, idx: Idx<Expr>) -> &mut Expr {
        &mut self.database.exprs[idx]
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

    pub fn type_is_function(&self, idx: Idx<Type>) -> bool {
        matches!(self.type_(idx), Type::Function(_))
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

    pub fn type_of_type_expr(&self, idx: Idx<TypeExpr>) -> Idx<Type> {
        self.type_database.get_type_expr_type(idx)
    }

    pub fn range_of_type_expr(&self, idx: Idx<TypeExpr>) -> TextRange {
        self.database.type_expr_ranges[idx]
    }

    fn find_local(&mut self, name: &str) -> Option<ValueSymbol> {
        let key = self.interner.intern(name);

        self.find_value(key)
    }

    /// Gets the interned Key for a value symbol
    pub fn value_symbol_key(&self, symbol: ValueSymbol) -> Key {
        self.database.value_names[&symbol]
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

    pub fn lookup_operator(&self, symbol: ValueSymbol) -> Option<IntrinsicExpr> {
        self.database.lookup_operator(symbol)
    }

    pub fn core_types(&self) -> &CoreTypes {
        &self.type_database.core
    }

    pub fn mutability_of(&self, symbol: &ValueSymbol) -> Mutability {
        *self
            .database
            .mutabilities
            .get(symbol)
            .expect("symbol to exist")
    }
}

impl Context {
    pub fn find_value(&self, name: Key) -> Option<ValueSymbol> {
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
        // self.type_database.insert_type_symbol(key, ty);
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
            Expr::TypeStatement(_) => inner,
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
                E::Break(ast) => self.lower_break_statement(ast),
                E::Call(ast) => self.lower_call(ast),
                E::FloatLiteral(ast) => self.lower_float_literal(ast),
                E::Function(ast) => self.lower_function_expr(ast),
                E::ForInLoop(ast) => self.lower_for_in_loop(ast),
                E::Ident(ast) => self.lower_name_ref(&ast.as_string()),
                E::If(ast) => self.lower_if_expr(ast),
                E::IntLiteral(ast) => self.lower_int_literal(ast),
                E::LetBinding(ast) => self.lower_let_binding(ast),
                E::Loop(ast) => self.lower_loop(ast),
                E::Match(ast) => self.lower_match(ast),
                E::Paren(ast) => return self.lower_expr(ast.expr()),
                E::Path(ast) => self.lower_path(ast),
                E::ReAssignment(ast) => self.lower_reassignment(ast),
                E::Return(ast) => self.lower_return_statement(ast),
                E::StringLiteral(ast) => self.lower_string_literal(ast),
                E::TypeBinding(ast) => self.lower_type_binding(ast),
                E::Unary(ast) => self.lower_unary(ast),
            }
        } else {
            Expr::Empty
        };

        self.alloc_expr(expr, ast)
    }

    fn lower_value_name(&mut self, name: String) -> (Key, ValueSymbol) {
        let key = self.interner.intern(&name);
        let symbol = self.lower_value_key(key);

        (key, symbol)
    }

    fn lower_value_key(&mut self, name: Key) -> ValueSymbol {
        let symbol = self.current_scopes_mut().insert_value(name);
        self.database.value_names.insert(symbol, name);
        symbol
    }

    fn lower_type_name(&mut self, name: String) -> (Key, TypeSymbol) {
        let key = self.interner.intern(&name);

        let symbol = self.current_scopes_mut().insert_type(key);
        self.database.type_names.insert(symbol, key);
        (key, symbol)
    }

    fn lower_let_binding(&mut self, ast: ast::LetBinding) -> Expr {
        // TODO: desugar patterns into separate definitions
        // or not until MIR?
        let name = ast.name().unwrap().text().to_string();
        let (key, symbol) = self.lower_value_name(name);
        self.current_let_binding_symbol = Some((key, symbol));

        let mutability = ast.mutability();
        self.database.mutabilities.insert(symbol, mutability);

        let value = self.lower_expr(ast.value());

        let type_annotation = ast
            .type_annotation()
            .map(|type_expr| self.lower_type_expr(type_expr.into(), None));

        Expr::variable_def(symbol, value, type_annotation)
    }

    fn lower_reassignment(&mut self, ast: ast::ReAssignment) -> Expr {
        let place = self.lower_expr(ast.place());
        let value = self.lower_expr(ast.value());

        Expr::ReAssignment(ReAssignment { place, value })
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
        let s: String = ast
            .value()
            .map(|token| token.text().to_owned())
            .expect("StringLiteral to have a valid token");

        let s = &s[1..s.len() - 1]; // remove leading and trailing quotes

        let key = self.interner.intern(s);
        Expr::StringLiteral(key)
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

    /// ```ignore
    /// for el in arr {
    ///     // exprs
    /// }
    /// ```
    ///
    /// translates to:
    ///
    /// ```ignore
    /// {
    ///     let mutable _index = 0
    ///     let _len = arr.len
    ///     loop {
    ///         if _index >= _len { break }
    ///         let el = intrinsic { arr[_index] }
    ///         // exprs
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

    fn lower_loop(&mut self, ast: ast::Loop) -> Expr {
        if let Some(block) = ast.block() {
            self.breaks_stack.push_loop();
            let block = Some(ast::Expr::Block(block));
            let body = self.lower_expr(block);

            let breaks = self.breaks_stack.pop();
            Expr::Loop(LoopExpr { body, breaks })
        } else {
            Expr::Empty
        }
    }

    fn lower_match(&mut self, ast: ast::expr::Match) -> Expr {
        let scrutinee = ast.scrutinee().and_then(|s| s.expr());
        let scrutinee = self.lower_expr(scrutinee);

        let arms = ast
            .arms()
            .into_iter()
            .filter_map(|arm| self.lower_match_arm(arm))
            .collect();

        Expr::Match(MatchExpr { scrutinee, arms })
    }

    fn lower_match_arm(&mut self, ast: ast::expr::MatchArm) -> Option<MatchArm> {
        ast.pattern().map(|pattern| {
            let pattern = self.lower_pattern(pattern);
            let expr = self.lower_expr(ast.expr());
            MatchArm { pattern, expr }
        })
    }

    fn lower_pattern(&mut self, ast: ast::expr::Pattern) -> Pattern {
        let range = ast.range();
        match ast {
            ast::expr::Pattern::Identifier(node) => todo!(),
            ast::expr::Pattern::DotIdentifier(dot_ident) => {
                let name = dot_ident.name();
                let key = self.interner.intern(&name);

                // TODO: capture the bound variable
                // should use `self.lower_name(name)` to ensure it's added to scope
                // between push_scope() / pop_scope()
                // .some(example) -> { ... }
                //       ^^^^^^^
                Pattern::binding(key, None, range)
            }
            ast::expr::Pattern::StringLiteral(node) => todo!(),
            ast::expr::Pattern::IntLiteral(node) => todo!(),
        }
    }

    fn lower_call(&mut self, ast: ast::expr::CallExpr) -> Expr {
        let callee = ast.callee().unwrap();
        let symbol = match &callee {
            ast::Expr::Ident(ident) => {
                let key = self.interner.intern(&ident.as_string());
                self.find_value(key)
            }
            ast::Expr::Path(path) => path
                .subject_as_ident()
                .map(|ident| self.interner.intern(&ident.as_string()))
                .and_then(|key| self.find_value(key)),

            _ => None,
        };
        let callee = self.lower_expr(Some(callee));

        // TODO: confirm new definition of ast::CallExpr always has args
        let call_args = ast.args().unwrap();
        let args = call_args
            .args()
            .map(|expr| self.lower_expr(Some(expr)))
            .collect();

        Expr::call(callee, args, symbol)
    }

    fn lower_path(&mut self, path: ast::PathExpr) -> Expr {
        // `a`
        // -> subject: `a`  (Ident)
        // -> member: None

        // `Color.green` (VariantExpr)
        // -> subject: `Color`  (UnionNamespace)
        // -> member: `green`   (UnionUnitVariant)

        // `Status.pending start_time`
        // -> subject: `Status` (UnionNamespace)
        // -> member: `pending` (UnionVariant)

        // `a.b.c` (MemberExpr)
        // -> subject: `a.b` (PathExpr)
        // -> member: `c`    (RecordField)

        // Need to recognize "Color" as a namespace / union namespace kind of thing, and
        // recognize "green" as a UnitVariant

        // or is Color more of a "record"?
        // pseudo `let Color = ( red={...}, green={...}, blue={...} )`

        if let Some(member) = path.member() {
            dbg!(&path.subject());
            let subject = self.lower_expr(path.subject());
            dbg!(self.expr(subject));

            // if subject is UnionNamespace
            // and if member Key matches any UnionNamespace.members
            // make a UnionUnitVariant or UnionVariant accordingly

            // FIXME - this just produces UnresolvedVarRef, because "green" doesn't
            // exists as a value in the current namespace, it only exists as a value in
            // the "Color" namespace
            // Pass in a namespace to self.lower_expr?
            // change some kind of state on self?
            let member = self.lower_expr(Some(member));
            //

            // if subject is UnionNamespace, member should be UnionVariant/UnionUnitVariant

            match self.expr(member) {
                // Index - TODO: maybe remove indexing completely
                Expr::IntLiteral(_) => Expr::IndexInt(IndexIntExpr {
                    subject,
                    index: member,
                }),
                Expr::StringLiteral(_) => todo!("allowed? `rec.\"field\"`"),
                Expr::Path(path) => todo!(),

                Expr::Unary(_) => todo!(), // arr.(-x)     // allowed??

                Expr::Call(_) => todo!(), // arr.(max arr2) // allowed??

                // How do we distinguish between `a.b` (member) and `a.(b)` (index) ?
                Expr::VarRef(_) => todo!(),

                Expr::Block(_) => todo!(), // technically allowed? Or prevent in AST
                Expr::If(_) => todo!(),    // technically allowed? Or prevent in AST

                Expr::UnresolvedVarRef { key } => panic!(
                    "Internal Compiler Error: Unresolved variable '{}'",
                    self.lookup(*key)
                ),

                // Expr::
                e => {
                    dbg!(e);
                    todo!("{}", e.display(self))
                }
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
        dbg!(name);
        dbg!(value_symbol);

        // TODO - figure out if its unionnamespace
        // Expr::UnionNamespace(UnionNamespace { name, members });

        if let Some(symbol) = value_symbol {
            // if this ValueSymbol was originally defined by virtue of a type binding (ex. unions)
            if let Some(type_symbol) = self.database.type_value_symbols.get(&symbol) {
                let type_expr = self.database.named_type_exprs
                    .get(type_symbol)
                    .expect("value symbol with a corresponding type symbol has a corresponding type expression");
                let (type_expr, ..) = self.database.type_expr(*type_expr);

                match type_expr {
                    TypeExpr::Union(union_type_expr) => {
                        return Expr::UnionNamespace(UnionNamespace {
                            name: symbol,
                            members: union_type_expr.variants.clone(),
                        });
                    }
                    _ => unreachable!(),
                };
            }
            Expr::VarRef(VarRefExpr { symbol })
        } else {
            Expr::UnresolvedVarRef { key }
        }
    }

    fn lower_function_expr(&mut self, function_ast: ast::Function) -> Expr {
        let func_name = self.current_let_binding_symbol;

        // TODO: parse and lower @entry_point annotations
        // TODO: once there are programs with multiple modules, only the "main" in the
        // main module gets this special status. Or make the user use an annotation still
        let mut entry_point = None;
        if let Some((key, ..)) = func_name {
            if self.interner.core_keys().main == key {
                entry_point = Some(key);
            }
        }
        let entry_point = entry_point;

        self.push_scope();
        let params: Box<[FunctionParam]> = function_ast
            .param_list()
            .params()
            .map(|param| {
                let ident = param.ident().expect("function parameter to have ident");

                let name = self.interner.intern(&ident.as_string());
                let symbol = self.current_scopes_mut().insert_value(name);
                self.database.value_names.insert(symbol, name);

                let annotation = param
                    .type_expr()
                    .map(|type_expr| self.lower_type_expr(Some(type_expr), None));

                FunctionParam {
                    name,
                    symbol,
                    annotation,
                }
            })
            .collect();

        // TODO: not sure how... but getting all the ValueSymbols
        // used by this body but defined outside of the body would
        // be great
        let captures = Vec::new();

        let body = function_ast
            .body()
            .and_then(|body| body.expr())
            .map(|body| self.lower_expr(Some(body)));
        let body = body.expect("TODO: handle missing function body");
        self.pop_scope();

        let return_ty = self.lower_type_expr(function_ast.return_type(), None);
        let return_type_annotation = if matches!(self.type_expr(return_ty), TypeExpr::Empty) {
            None
        } else {
            Some(return_ty)
        };

        let overloads = Box::new([FunctionExpr {
            params,
            body,
            return_type_annotation,
            captures: captures.into(),
        }]);

        Expr::Function(FunctionExprGroup {
            overloads,
            name: func_name,
            entry_point,
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

    fn lower_break_statement(&mut self, ast: ast::BreakStatement) -> Expr {
        // TODO: Confirm it is OK to create Expr::Empty here for the None case
        let break_value = self.lower_expr(ast.value());

        if self.breaks_stack.is_empty() {
            let diagnostic = LoweringDiagnostic::break_outside_loop(ast.range());
            self.diagnostics.push(diagnostic.into());
        } else {
            self.breaks_stack.add(break_value);
        }

        Expr::BreakStatement(break_value)
    }

    fn lower_return_statement(&mut self, ast: ast::ReturnStatement) -> Expr {
        let return_value = ast.return_value();
        // TODO: Confirm it is OK to create Expr::Empty here for the None case
        let return_value = self.lower_expr(return_value);

        Expr::ReturnStatement(return_value)
    }
}

// Lowering functions - TypeExpr
impl Context {
    fn lower_type_expr(
        &mut self,
        ast: Option<ast::TypeExpr>,
        name: Option<TypeSymbol>,
    ) -> Idx<TypeExpr> {
        use ast::TypeExpr as TE;
        let type_expr = if let Some(ast) = ast {
            let range = ast.range();
            let type_expr = match ast {
                TE::Call(ast) => self.lower_type_call(ast),
                TE::FloatLiteral(ast) => self.lower_type_float_literal(ast),
                TE::Ident(ast) => self.lower_type_ident(ast),
                TE::IntLiteral(ast) => self.lower_type_int_literal(ast),
                TE::Function(_) => todo!(),
                TE::Paren(ast) => return self.lower_type_expr(ast.expr(), name),
                TE::Path(ast) => self.lower_type_path(ast),
                TE::StringLiteral(ast) => self.lower_type_string_literal(ast),
                TE::Union__Old(ast) => self.lower_union__old(ast),
                TE::Union(ast) => self.lower_union(ast, name),
            };
            self.alloc_type_expr(type_expr, range)
        } else {
            self.alloc_type_expr(TypeExpr::Empty, TextRange::default())
        };

        if let Some(type_symbol) = name {
            self.database
                .named_type_exprs
                .insert(type_symbol, type_expr);
        }
        type_expr
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

    fn lower_union(&mut self, ast: ast::Union, name: Option<TypeSymbol>) -> TypeExpr {
        let variants = ast
            .variants()
            .iter()
            .map(|item| {
                let ident_str = &item.ident_as_string();
                let key = self.interner.intern(ident_str);
                let type_expr = item
                    .type_expr()
                    .map(|t| self.lower_type_expr(Some(t), None))
                    .unwrap_or_else(|| self.alloc_type_expr(TypeExpr::Unit, item.range()));

                (key, type_expr)
            })
            .collect_vec();

        // Union names also exist in the value namespace
        // `type Color = red | green | blue`
        // `let myColor = Color.green`
        //                ^^^^^ ^^^^^
        if let Some(type_symbol) = name {
            let key = self.database.type_names[&type_symbol];
            let value_symbol = self.lower_value_key(key);
            self.database
                .mutabilities
                .insert(value_symbol, Mutability::Not);
            self.database
                .type_value_symbols
                .insert(value_symbol, type_symbol);
        }

        TypeExpr::Union(UnionTypeExpr { name, variants })
    }

    fn lower_union__old(&mut self, ast: ast::Union__Old) -> TypeExpr {
        let variants = ast
            .variants()
            .iter()
            .map(|item| {
                let key = self.interner.intern(&item.ident_as_string());
                let type_expr = item
                    .type_expr()
                    .map(|t| self.lower_type_expr(Some(t), None))
                    .unwrap_or_else(|| self.alloc_type_expr(TypeExpr::Unit, item.range()));

                (key, type_expr)
            })
            .collect_vec();

        TypeExpr::Union(UnionTypeExpr {
            name: None,
            variants,
        })
    }

    ///
    fn lower_type_call(&mut self, ast: ast::expr::CallExpr) -> TypeExpr {
        // let path = ast.path().unwrap();
        // let mut idents = path.ident_strings();

        // TODO: handle multiple idents in a Path
        // let name = idents.next().unwrap();

        let name = String::new();

        if let Some(call_args) = ast.args() {
            let args = call_args
                .type_args()
                .map(|expr| self.lower_type_expr(Some(expr), None))
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

    fn lower_type_binding(&mut self, ast: ast::TypeBinding) -> Expr {
        let name = ast.name().unwrap().text().to_string();
        let (.., symbol) = self.lower_type_name(name);

        let type_expr = self.lower_type_expr(ast.type_expr(), Some(symbol));
        let type_var_def = TypeExpr::type_variable_def(symbol, type_expr);
        let type_var_def = self.alloc_type_expr(type_var_def, ast.range());
        Expr::TypeStatement(type_var_def)
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

#[derive(Debug, Default)]
struct BreaksStack(Vec<Vec<Idx<Expr>>>);

impl BreaksStack {
    pub fn push_loop(&mut self) {
        self.0.push(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add(&mut self, expr: Idx<Expr>) {
        self.0
            .last_mut()
            .expect("Compiler Bug: BreaksStack was empty")
            .push(expr);
    }

    pub fn pop(&mut self) -> Vec<Idx<Expr>> {
        self.0.pop().expect("Compiler Bug: BreaksStack was empty")
    }
}
