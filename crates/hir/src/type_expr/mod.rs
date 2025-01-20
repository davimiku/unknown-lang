mod display;

use la_arena::Idx;

use crate::interner::Key;
use crate::{BinaryOp, UnaryOp};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    /// A missing expression from the parse tree
    Empty,

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

    /// Reference to a type variable
    VarRef(TypeRefExpr),

    /// Reference to a type variable not defined in the current scope
    UnresolvedVarRef {
        key: Key,
    },

    /// Definition of a local type variable
    LocalDef(TypeDefExpr),

    Union(UnionTypeExpr),

    /// Expression evaluating to the unit type, such as `()`
    Unit,
    // Call(CallExpr),
    // Function(FunctionExpr),
    // // TODO: should If be a special case of Match?
    // If(IfExpr),
}

/// Binary type expression
///
/// ```ignore
/// type Five = 2 + 3
/// //          ^^^^^
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryTypeExpr {
    pub op: BinaryOp,
    pub lhs: Idx<TypeExpr>,
    pub rhs: Idx<TypeExpr>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpr {
    // TODO: create a Path struct to handle multiple
    pub path: String,

    /// Arguments to the parametric type
    pub args: Vec<Idx<TypeExpr>>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryTypeExpr {
    pub op: UnaryOp,
    pub expr: Idx<TypeExpr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnionTypeExpr {
    pub variants: Vec<(Key, Idx<TypeExpr>)>,
}

/// Local type definition
///
/// Defines a new type in a given scope.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeDefExpr {
    pub key: TypeSymbol,

    /// Expression value assigned to the type
    pub value: Idx<TypeExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeSymbol {
    /// Unique id of this symbol within this module
    symbol_id: u32,

    /// Unique id of the module where this symbol resides
    module_id: u32,
}

impl TypeSymbol {
    pub fn new(module_id: u32, symbol_id: u32) -> Self {
        Self {
            module_id,
            symbol_id,
        }
    }
}

/// Reference to a variable that lives in the "type" universe
#[derive(Debug, PartialEq, Clone)]
pub struct TypeRefExpr {
    /// Interned string of the type name
    pub key: Key,

    /// Unique identifier for the type symbol
    pub symbol: TypeSymbol,
}
