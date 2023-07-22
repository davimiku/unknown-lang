use la_arena::Idx;

use crate::interner::{Interner, Key};
use crate::{BinaryOp, UnaryOp, COMPILER_BRAND};

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    /// A missing expression from the parse tree
    Empty,

    /// Boolean literal value, `true` or `false`
    BoolLiteral(bool),

    /// 64-bit Floating point literal value, ex. `1.0`, `-7654.321`
    FloatLiteral(f64), // TODO: shared definition of Float

    /// 64-bit Integer literal value, ex. `0`, `12345`, `-98765`
    IntLiteral(i64), // TODO: shared definition of Int

    /// String literal value, ex. `"hello"`, `"world"`
    StringLiteral(Key),

    /// Binary expression, ex. `a + b`, `c ^ d`
    Binary(BinaryTypeExpr),

    /// Unary expression, ex. `-a`, `!b`
    Unary(UnaryTypeExpr),
    // Block(BlockExpr),
    Call(CallExpr),

    LocalRef(LocalTypeRefExpr),
    LocalDef(LocalTypeDefExpr),
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

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpr {
    // TODO: create a Path struct to handle multiple
    pub path: String,

    /// Arguments to the parametric type
    pub args: Vec<Idx<TypeExpr>>,
}

/// Local type definition
///
/// Defines a new type in a given scope.
#[derive(Debug, PartialEq, Eq)]
pub struct LocalTypeDefExpr {
    pub key: LocalTypeDefKey,

    /// Expression value assigned to the type
    pub value: Idx<TypeExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalTypeDefKey {
    name: Key,

    /// Unique number for this Name within this Context
    idx: u32,
}

impl LocalTypeDefKey {
    pub(crate) fn display(&self, interner: &Interner) -> String {
        let name = interner.lookup(self.name);
        let idx = self.idx;

        format!("{name}{COMPILER_BRAND}{idx}")
    }
}

impl From<(Key, u32)> for LocalTypeDefKey {
    fn from(value: (Key, u32)) -> Self {
        Self {
            name: value.0,
            idx: value.1,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LocalTypeRefExpr {
    pub name: LocalTypeRefName,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LocalTypeRefName {
    // TODO: handle `a`, `a.b`, `a.b.c`, etc.
    Resolved(LocalTypeDefKey),
    Unresolved(Key),
}
