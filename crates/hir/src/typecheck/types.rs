use crate::Name;

#[derive(Debug, PartialEq)]
pub enum Type {
    Undetermined,
    Error,

    Bool,
    BoolLiteral(bool),
    Float,
    FloatLiteral(f64),
    Int,
    IntLiteral(i64),
    String,
    StringLiteral(String), // TODO: use a borrowed string instead (borrow from whom?)
    Named(Name),
    // Fun -- (Vec<Type>, Type) ? for params type and return type
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
