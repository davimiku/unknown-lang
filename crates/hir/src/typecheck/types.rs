use std::hash::{DefaultHasher, Hash, Hasher};

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
    FloatLiteral(f64), // TODO: shared alias of Float
    IntLiteral(i64),   // TODO: shared alias of Int
    StringLiteral(Key),

    // Scalars
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
    pub(crate) fn sum(variants: Box<[(Key, Idx<Type>)]>) -> Self {
        let mut s = DefaultHasher::new();
        for (key, ty) in variants.iter() {
            (*key).hash(&mut s);
            (*ty).into_raw().into_u32().hash(&mut s);
        }
        let hash = s.finish();

        Self::Sum(SumType { variants, hash })
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
    /// Named variants of the sum type in the form of `key: Type`
    // TODO - consider a different data structure allowing for access by Key, to avoid O(n)
    // review all existing usages of direct indexes
    pub variants: Box<[(Key, Idx<Type>)]>,

    /// Hash computed on creation for faster comparisons
    pub(crate) hash: u64,
}

impl SumType {
    pub fn index_of(&self, key: Key) -> Option<i64> {
        self.variants
            .iter()
            .position(|(k, _)| *k == key)
            .map(|u| u as i64)
    }

    pub fn variant_type_of(&self, key: Key) -> Option<Idx<Type>> {
        self.variants
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(.., ty)| ty)
            .copied()
    }

    pub fn is_unit(&self, context: &Context) -> bool {
        self.variants
            .iter()
            .all(|(.., ty)| *ty == context.core_types().unit)
    }
}

impl ContextDisplay for SumType {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        s.push('(');
        let mut variants = self.variants.iter().peekable();
        while let Some((tag, ty)) = variants.next() {
            s.push_str(context.lookup(*tag));
            if *ty != context.core_types().unit {
                s.push_str(": ");
                s.push_str(&ty.display(context));
            }
            if variants.peek().is_some() {
                s.push_str(" | ");
            }
        }
        s.push(')');

        s.truncate(s.trim_end().len());
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
