use crate::{Expr, Interner, TypeExpr};
use la_arena::{Arena, ArenaMap, Idx};
use text_size::TextRange;

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) expr_ranges: ArenaMap<Idx<Expr>, TextRange>,

    /// Allocated type expressions
    pub(crate) type_exprs: Arena<TypeExpr>,

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) type_expr_ranges: ArenaMap<Idx<TypeExpr>, TextRange>,
}

impl Database {
    pub(crate) fn display(&self, interner: &Interner) -> String {
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
    /// Returns the expression at the given index
    ///
    /// Panics if the index doesn't exist, but this is not a problem
    /// in practice because it shouldn't be possible to create an arbitrary index
    /// in safe code.
    pub(crate) fn expr(&self, idx: Idx<Expr>) -> (&Expr, &TextRange) {
        (&self.exprs[idx], &self.expr_ranges[idx])
    }

    /// Returns the type expression at the given index
    ///
    /// Panics if the index doesn't exist, but this is not a problem
    /// in practice because it shouldn't be possible to create an arbitrary index
    /// in safe code.
    pub(crate) fn type_expr(&self, idx: Idx<TypeExpr>) -> (&TypeExpr, &TextRange) {
        (&self.type_exprs[idx], &self.type_expr_ranges[idx])
    }

    pub(crate) fn range_of_expr(&self, idx: Idx<Expr>) -> TextRange {
        self.expr_ranges.get(idx).copied().unwrap_or_default()
    }

    pub(crate) fn range_of_type_expr(&self, idx: Idx<TypeExpr>) -> TextRange {
        self.type_expr_ranges.get(idx).copied().unwrap_or_default()
    }
}

// Mutating functions
impl Database {
    pub(crate) fn alloc_expr(&mut self, expr: Expr, ast: Option<ast::Expr>) -> Idx<Expr> {
        let idx = self.exprs.alloc(expr);

        let range = ast.map_or(Default::default(), |ast| ast.range());
        self.expr_ranges.insert(idx, range);

        idx
    }

    pub(crate) fn alloc_type_expr(&mut self, expr: TypeExpr, range: TextRange) -> Idx<TypeExpr> {
        let idx = self.type_exprs.alloc(expr);
        self.type_expr_ranges.insert(idx, range);

        idx
    }
}
