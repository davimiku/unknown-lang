use crate::Name;

// TODO: store type of each Expr in a corresponding map
// ArenaMap<Idx<Expr>, Type>
// where the Idx<Expr> is the same as the allocated Expr

use super::{BinaryOp, Expr, Stmt, UnaryOp};
use la_arena::{Arena, Idx};

#[derive(Debug, PartialEq)]
pub enum Type {
    Undetermined,

    Bool,
    BoolLiteral(bool),
    Float,
    FloatLiteral(f64),
    Int,
    IntLiteral(i64),
    String,
    StringLiteral(String),
    Named(Name),
    // Fun -- (Vec<Type>, Type) ? for params type and return type
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
