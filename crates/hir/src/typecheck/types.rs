use crate::Key;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    // Literals
    BoolLiteral(bool),
    FloatLiteral(f64), // TODO: shared alias of Float
    IntLiteral(i32),   // TODO: shared alias of Int
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

    // TODO: why is this a type?
    // Named(Key),

    // Compound
    // TODO: arena allocate Type so that recursive types can be Copy?
    // Struct
    // Union
    // Newtype
    Function {
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
