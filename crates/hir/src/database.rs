use std::collections::HashMap;

use crate::interner::Key;
use crate::type_expr::TypeSymbol;
use crate::{Context, ContextDisplay, Expr, TypeExpr, ValueSymbol};
use la_arena::{Arena, ArenaMap, Idx};
use text_size::TextRange;

// TODO: remove pub(crate) ?
#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) expr_ranges: ArenaMap<Idx<Expr>, TextRange>,

    /// Allocated type expressions
    pub(crate) type_exprs: Arena<TypeExpr>,

    /// Text ranges of the expressions from `type_exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) type_expr_ranges: ArenaMap<Idx<TypeExpr>, TextRange>,

    /// Symbols that are function definitions (static/constant).
    ///
    /// Typically (?) these are defined at the top level and must be immutable
    /// bindings with no dynamic captures.
    ///
    /// In the JIT backend, calls of these symbols are translated into a direct
    /// function call (rather than an indirect call) and the "size" of these are
    /// a zero-sized type (rather than pointer-sized for an indirect call).
    pub(crate) function_defs: HashMap<ValueSymbol, Idx<Expr>>,

    /// Reverse mapping between every symbol defining a value (i.e. variable)
    /// and its original (interned) name.
    pub(crate) value_names: HashMap<ValueSymbol, Key>,

    /// Reverse mapping between every symbol defining a type (i.e. type variable)
    /// and its interned string name.
    pub(crate) type_names: HashMap<TypeSymbol, Key>,
}

impl ContextDisplay for Database {
    fn display(&self, _context: &Context) -> String {
        let mut output = String::new();

        output.push_str("\nExpressions:\n");
        for (idx, expr) in self.exprs.iter() {
            let range = self.expr_ranges[idx];
            output.push_str(&format!("{idx:?} ({range:?}): {expr:?}\n"));
        }

        output.push_str("\nType Expressions:\n");
        for (idx, expr) in self.type_exprs.iter() {
            let range = self.type_expr_ranges[idx];
            output.push_str(&format!("{idx:?} ({range:?}): {expr:?}\n"));
        }

        output
    }
}

// Non-Mutating functions
impl Database {
    /// Returns the expression at the given index.
    ///
    /// Panics if the index doesn't exist. An invalid index indicates
    /// a bug in the compiler.
    pub(crate) fn expr(&self, idx: Idx<Expr>) -> (&Expr, &TextRange) {
        (&self.exprs[idx], &self.expr_ranges[idx])
    }

    /// Returns the type expression at the given index.
    ///
    /// Panics if the index doesn't exist. An invalid index indicates
    /// a bug in the compiler.
    pub(crate) fn type_expr(&self, idx: Idx<TypeExpr>) -> (&TypeExpr, &TextRange) {
        (&self.type_exprs[idx], &self.type_expr_ranges[idx])
    }

    /// Returns the range of a given expression
    ///
    /// Panics if the index doesn't exist. An invalid index indicates
    /// a bug in the compiler.
    pub(crate) fn range_of_expr(&self, idx: Idx<Expr>) -> TextRange {
        self.expr_ranges[idx]
    }

    /// Returns the range of a given type expression
    ///
    /// Panics if the index doesn't exist. An invalid index indicates
    /// a bug in the compiler.
    pub(crate) fn range_of_type_expr(&self, idx: Idx<TypeExpr>) -> TextRange {
        self.type_expr_ranges[idx]
    }

    pub(crate) fn function_def(&self, symbol: ValueSymbol) -> Option<Idx<Expr>> {
        self.function_defs.get(&symbol).copied()
    }
}

// Mutating functions
impl Database {
    /// Allocates a term expression into the database, returning an index to store
    /// for later use.
    pub(crate) fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        let idx = self.exprs.alloc(expr);

        let range = ast.map_or_else(Default::default, ast::Expr::range);
        self.expr_ranges.insert(idx, range);

        idx
    }

    /// Allocates a type expression into the database, returning an index to store
    /// for later use.
    pub(crate) fn alloc_type_expr(&mut self, expr: TypeExpr, range: TextRange) -> Idx<TypeExpr> {
        let idx = self.type_exprs.alloc(expr);
        self.type_expr_ranges.insert(idx, range);

        idx
    }
}
