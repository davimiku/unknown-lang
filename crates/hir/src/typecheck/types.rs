use crate::Key;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    // Literals
    BoolLiteral(bool),
    FloatLiteral(f64), // TODO: shared alias of Float
    IntLiteral(i64),   // TODO: shared alias of Int
    StringLiteral(Key),

    // Unit
    Unit,

    // Scalars
    Bool,
    Float,
    Int,
    String,

    Top,
    Bottom,

    // Compound
    // TODO: arena allocate Type so that recursive types can be Copy?
    // Struct
    // Union
    // Newtype
    Function(FunctionType),
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
