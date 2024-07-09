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

    // Sum
    Sum(SumType),

    // Product
    // Product(ProductType)

    // Exponential
    Function(FunctionType),
    Array(ArrayType),
    // TODO: consider arena allocating larger variants
    // and could consider making this Copy
}

impl Type {
    pub fn is_bool(&self) -> bool {
        matches!(self, Type::BoolLiteral(_) | Type::Bool)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::IntLiteral(_) | Type::Int)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::FloatLiteral(_) | Type::Float)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::StringLiteral(_) | Type::String)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Type::Unit)
    }
}

impl Type {
    pub(crate) fn sum(variants: Vec<(Key, Idx<Type>)>) -> Self {
        Self::Sum(SumType { variants })
    }

    pub(crate) fn array_of(of: Idx<Type>) -> Self {
        Self::Array(ArrayType { of })
    }

    pub(crate) fn func(signatures: Vec<FuncSignature>) -> Self {
        Self::Function(FunctionType { signatures })
    }
}

impl ContextDisplay for Idx<Type> {
    fn display(&self, context: &Context) -> String {
        let ty = context.type_(*self);

        ty.display(context)
    }
}

impl ContextDisplay for Type {
    fn display(&self, context: &Context) -> String {
        match self {
            Type::Bool => "Bool".to_owned(),
            Type::BoolLiteral(b) => b.to_string(),
            Type::Float => "Float".to_owned(),
            Type::FloatLiteral(f) => {
                let mut buf = ryu::Buffer::new();
                buf.format_finite(*f).to_owned()
            }
            Type::Int => "Int".to_owned(),
            Type::IntLiteral(i) => i.to_string(),
            Type::String => "String".to_owned(),
            Type::StringLiteral(key) => format!("\"{}\"", context.lookup(*key)),

            Type::Sum(sum_type) => sum_type.display(context),

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
pub struct SumType {
    pub variants: Vec<(Key, Idx<Type>)>,
}

impl ContextDisplay for SumType {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        s.push('\n');
        for (tag, ty) in self.variants.iter() {
            s.push_str("| ");
            s.push_str(context.lookup(*tag));
            s.push_str(": ");
            s.push_str(&ty.display(context));
        }
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    // TODO: use a SmallVec or something like that because
    // most functions probably have a single signature
    pub signatures: Vec<FuncSignature>,
}

impl ContextDisplay for FunctionType {
    fn display(&self, context: &Context) -> String {
        if self.signatures.len() == 1 {
            return self.signatures[0].display(context);
        }
        let mut s = String::new();
        for signature in &self.signatures {
            s.push_str("| ");
            s.push_str(&signature.display(context));
            s.push('\n');
        }
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncSignature {
    pub params: Box<[Idx<Type>]>,
    pub return_ty: Idx<Type>,
}

impl From<((), Idx<Type>)> for FuncSignature {
    fn from(value: ((), Idx<Type>)) -> Self {
        let (_, return_ty) = value;
        Self {
            params: Box::new([]),
            return_ty,
        }
    }
}

impl From<((Idx<Type>,), Idx<Type>)> for FuncSignature {
    fn from(value: ((Idx<Type>,), Idx<Type>)) -> Self {
        let (params, return_ty) = value;
        Self {
            params: Box::new([params.0]),
            return_ty,
        }
    }
}

impl From<((Idx<Type>, Idx<Type>), Idx<Type>)> for FuncSignature {
    fn from(value: ((Idx<Type>, Idx<Type>), Idx<Type>)) -> Self {
        let (params, return_ty) = value;
        Self {
            params: Box::new([params.0, params.1]),
            return_ty,
        }
    }
}

impl ContextDisplay for FuncSignature {
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
