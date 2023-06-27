use crate::Key;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Type {
    #[default]
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

impl Type {
    pub(crate) fn array_of(ty: Type) -> Self {
        Self::Array(ArrayType { of: Box::new(ty) })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_ty: Box<Type>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ArrayType {
    // TODO: arena allocate types, then this is Idx<Type> ?
    pub of: Box<Type>,
}
