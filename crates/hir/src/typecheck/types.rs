use crate::{interner::Interner, Key};

#[derive(Debug, Clone, Copy, PartialEq)]
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
    StringLiteral(Key),
    Named(Key),
    Unit,
    // Function
    // Struct
    // Union
    // Newtype
}

impl Type {
    pub(crate) fn display(&self, interner: &Interner) -> String {
        match self {
            Type::Undetermined => "Undetermined".to_string(),
            Type::Error => "Error".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::BoolLiteral(b) => b.to_string(),
            Type::Float => "Float".to_string(),
            Type::FloatLiteral(f) => f.to_string(),
            Type::Int => "Int".to_string(),
            Type::IntLiteral(i) => i.to_string(),
            Type::String => "String".to_string(),
            Type::StringLiteral(s) => format!("\"{}\"", interner.lookup(*s)),
            Type::Named(name) => interner.lookup(*name).to_string(),
            Type::Unit => "Unit".to_string(),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Undetermined
    }
}
