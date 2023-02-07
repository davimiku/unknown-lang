use crate::Name;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    Bool,
    BoolLiteral(bool),
    Float,
    FloatLiteral(f64), // TODO: shared definition of Float
    Int,
    IntLiteral(i32), // TODO: shared definition of Int
    String,
    StringLiteral(String), // TODO: use an interned string
    Named(Name),
    Unit,
    // Fun -- (Vec<Type>, Type) ? for params type and return type
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
