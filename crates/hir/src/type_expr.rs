use la_arena::Idx;

use crate::interner::Key;
use crate::{BinaryOp, LocalDefExpr, LocalRefExpr, UnaryOp};

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    /// A missing expression from the parse tree
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64), // TODO: shared definition of Float

    /// 32-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i32), // TODO: shared definition of Int

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(Key),

    /// Binary expression, ex. `a + b`, `c ^ d`
    Binary(BinaryTypeExpr),

    /// Unary expression, ex. `-a`, `!b`
    Unary(UnaryTypeExpr),
    // Block(BlockExpr),

    // TODO: these can't reuse the same structs as value expressions
    LocalRef(LocalRefExpr),
    LocalDef(LocalDefExpr),
    // Call(CallExpr),
    // Function(FunctionExpr),
    // // TODO: should If be a special case of Match?
    // If(IfExpr),
}

#[derive(Debug, PartialEq, Eq)]
/// Binary expression
pub struct BinaryTypeExpr {
    pub op: BinaryOp,
    pub lhs: Idx<TypeExpr>,
    pub rhs: Idx<TypeExpr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryTypeExpr {
    pub op: UnaryOp,
    pub expr: Idx<TypeExpr>,
}
