use itertools::Itertools;
use la_arena::Idx;

use crate::{Context, ContextDisplay, Key};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Type {
    #[default]
    Unknown,
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
    pub(crate) fn array_of(of: Idx<Type>) -> Self {
        Self::Array(ArrayType { of })
    }

    pub(crate) fn func(params: Vec<Idx<Type>>, return_ty: Idx<Type>) -> Self {
        Self::Function(FunctionType { params, return_ty })
    }
}

impl ContextDisplay for Idx<Type> {
    fn display(&self, context: &Context) -> String {
        let ty = context.borrow_type(*self);

        ty.display(context)
    }
}

impl ContextDisplay for Type {
    fn display(&self, context: &Context) -> String {
        match self {
            Type::Bool => "Bool".to_owned(),
            Type::BoolLiteral(b) => b.to_string(),
            Type::Float => "Float".to_owned(),
            Type::FloatLiteral(f) => f.to_string(),
            Type::Int => "Int".to_owned(),
            Type::IntLiteral(i) => i.to_string(),
            Type::String => "String".to_owned(),
            Type::StringLiteral(key) => format!("\"{}\"", context.lookup(*key)),

            Type::Function(func) => func.display(context),
            Type::Array(arr) => arr.display(context),

            Type::Unit => "()".to_owned(),
            Type::Top => "{top}".to_owned(),
            Type::Bottom => "{bottom}".to_owned(),

            Type::Unknown => "{unknown}".to_owned(),
            Type::Error => "{ERROR}".to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Idx<Type>>,
    pub return_ty: Idx<Type>,
}

impl ContextDisplay for FunctionType {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();

        s.push('(');
        let params = self
            .params
            .iter()
            .map(|param| param.display(context))
            .join(", ");
        s.push_str(&params);
        s.push_str(") -> ");
        s.push_str(&self.return_ty.display(context));

        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub of: Idx<Type>,
}

impl ContextDisplay for ArrayType {
    fn display(&self, context: &Context) -> String {
        format!("[]{}", self.of.display(context))
    }
}
