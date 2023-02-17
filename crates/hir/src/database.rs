use crate::Expr;
use la_arena::{Arena, ArenaMap, Idx};
use text_size::TextRange;

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    /// Allocated expressions
    pub(crate) exprs: Arena<Expr>, // todo: reduce visibility

    /// Text ranges of the expressions from `exprs`
    /// Invariant: The indexes must be kept in sync
    pub(crate) expr_ranges: ArenaMap<Idx<Expr>, TextRange>,
}

// Non-Mutating functions
impl Database {
    /// Returns the expression at the given index
    pub(crate) fn expr(&self, idx: Idx<Expr>) -> (&Expr, &TextRange) {
        (&self.exprs[idx], &self.expr_ranges[idx])
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
}
