use itertools::Itertools;

use crate::{interner::Interner, Key};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    Bool,
    BoolLiteral(bool),
    Float,
    FloatLiteral(f64), // TODO: shared alias of Float
    Int,
    IntLiteral(i32), // TODO: shared alias of Int
    String,
    StringLiteral(Key),
    Named(Key),
    Unit,
    Function {
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
    // Struct
    // Union
    // Newtype
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
