use crate::Key;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    Top,
    Bottom,
    Unit,

    // Literals
    BoolLiteral(bool),
    FloatLiteral(f64), // TODO: shared alias of Float
    IntLiteral(i64),   // TODO: shared alias of Int
    StringLiteral(Key),

    // Scalars
    Bool,
    Float,
    Int,
    String,

    // TODO: arena allocate Type so that recursive types can be Copy?

    // Sum
    // Union

    // Product
    // Struct

    // Exponential
    Function(FunctionType),
    Array(ArrayType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_ty: Box<Type>,
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub of: Box<Type>,
}
