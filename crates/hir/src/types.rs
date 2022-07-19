use crate::Name;

use super::{BinaryOp, Expr, Stmt, UnaryOp};
use la_arena::{Arena, Idx};

#[derive(Debug, PartialEq)]
pub enum Type {
    // #[default]  // TODO: Rust 1.63.0
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
