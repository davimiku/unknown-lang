use std::collections::HashMap;
use std::fmt::{self};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::{Index, IndexMut};

use itertools::Itertools;
use la_arena::Idx;

use crate::type_expr::TypeSymbol;
use crate::{Context, ContextDisplay, Key, ValueSymbol};

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
    UnionNamespace(UnionNamespaceType),

    // Product
    Product(ProductType),
    Function(FunctionType),
    Array(ArrayType),
    // TODO: consider arena allocating larger variants
    // and could consider making this Copy - but would need a custom impl of PartialEq?
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
    pub(crate) fn sum(
        variants: Box<[(Key, Idx<Type>)]>,
        name: Option<TypeSymbol>,
        namespace_ty: Option<Idx<Type>>,
    ) -> Self {
        let mut s = DefaultHasher::new();
        for (key, ty) in variants.iter() {
            (*key).hash(&mut s);
            (*ty).into_raw().into_u32().hash(&mut s);
        }
        let hash = s.finish();

        Self::Sum(SumType {
            variants,
            hash,
            name,
            namespace_ty,
        })
    }

    pub(crate) fn product(field_list: Box<[(Key, Idx<Type>)]>, name: Option<TypeSymbol>) -> Self {
        let mut s = DefaultHasher::new();
        let mut fields = HashMap::new();
        for (key, ty) in field_list.iter() {
            (*key).hash(&mut s);
            (*ty).into_raw().into_u32().hash(&mut s);
            fields.insert(*key, *ty);
        }
        let hash = s.finish();

        Self::Product(ProductType { fields, hash, name })
    }

    pub(crate) fn func(signatures: Vec<FuncSignature>) -> Self {
        Self::Function(FunctionType {
            signatures,
            variant: None,
        })
    }

    pub(crate) fn union_variant_func(
        signature: FuncSignature,
        variant: (Idx<Type>, u32, Key),
    ) -> Self {
        Self::Function(FunctionType {
            signatures: vec![signature],
            variant: Some((variant.0, variant.1.into(), variant.2)),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct VariantIdx {
    value: u32,
}

impl VariantIdx {
    pub fn into_raw(self) -> u32 {
        self.value
    }
}

impl fmt::Display for VariantIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.value.to_string())
    }
}

impl From<u32> for VariantIdx {
    fn from(value: u32) -> Self {
        Self { value }
    }
}

impl From<usize> for VariantIdx {
    fn from(value: usize) -> Self {
        Self {
            value: value as u32,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VecVariantIdx<T> {
    inner: Vec<T>,
}

impl<T> Default for VecVariantIdx<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<T> Index<VariantIdx> for VecVariantIdx<T> {
    type Output = T;

    fn index(&self, index: VariantIdx) -> &Self::Output {
        &self.inner[index.into_raw() as usize]
    }
}

impl<T> IndexMut<VariantIdx> for VecVariantIdx<T> {
    fn index_mut(&mut self, index: VariantIdx) -> &mut Self::Output {
        &mut self.inner[index.into_raw() as usize]
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
            Type::UnionNamespace(union_namespace) => union_namespace.display(context),
            Type::Product(product_type) => product_type.display(context),

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

    /// Symbol for this sum type in the type world if it exists. This would not exist for
    /// anonymous sum types.
    pub(crate) name: Option<TypeSymbol>,

    /// Optional reference to the [`UnionNamespaceType`] that produces instances of this sum type,
    /// if it exists. This would not exist for anonymous sum types
    pub(crate) namespace_ty: Option<Idx<Type>>,
}

impl SumType {
    pub fn index_of(&self, key: Key) -> Option<VariantIdx> {
        self.variants
            .iter()
            .position(|(k, _)| *k == key)
            .map(VariantIdx::from)
    }

    pub fn type_of_key(&self, key: Key) -> Option<Idx<Type>> {
        self.variants
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(.., ty)| ty)
            .copied()
    }

    pub fn type_of_idx(&self, idx: VariantIdx) -> Idx<Type> {
        self.variants[idx.into_raw() as usize].1
    }

    pub fn is_unit(&self, context: &Context) -> bool {
        self.variants
            .iter()
            .all(|(.., ty)| *ty == context.core_types().unit)
    }
}

impl ContextDisplay for SumType {
    fn display(&self, context: &Context) -> String {
        if let Some(name) = self.name {
            return name.display(context);
        }
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

        s
    }
}

/// Represents the type of the union namespace value itself,
/// **not** the type of an instance of that union (i.e. when a variant is constructed)
///
/// ```ignore
/// type Color = (red: Int | green: Int | blue: Int)
/// let example = Color
/// //            ^^^^^
/// let example_red = Color.red 255
/// //                ^^^^^
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct UnionNamespaceType {
    /// Symbol for the term in the value world
    pub name: ValueSymbol,

    /// Names of the variants with their inferred type of the "constructor" for that variant.
    /// Unit variants are just the SumType.
    /// All other variants are a synthesized "function" producing that SumType.
    ///
    /// ```ignore
    /// let i = NumberUnion.int 123
    /// //                  ^^^^^^^
    /// // this is a "call" producing an instance of "NumberUnion"
    /// ```
    pub variant_constructors: HashMap<Key, (u32, Idx<Type>)>,

    /// Index to the associated [`SumType`] for instances constructed from this union/namespace
    pub associated_sum_type: Idx<Type>,
}

impl ContextDisplay for UnionNamespaceType {
    fn display(&self, context: &Context) -> String {
        self.name.display(context)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProductType {
    /// Named fields like `key: Type`
    // TODO - optimized data storage for this kind of key (NonZeroU32)
    pub fields: HashMap<Key, Idx<Type>>,

    /// Hash computed on creation for faster comparisons
    pub(crate) hash: u64,

    pub(crate) name: Option<TypeSymbol>,
}

impl ProductType {
    pub fn type_of_key(&self, key: Key) -> Option<Idx<Type>> {
        self.fields.get(&key).copied()
    }
}

impl ContextDisplay for ProductType {
    fn display(&self, context: &Context) -> String {
        if let Some(name) = self.name {
            return name.display(context);
        }
        let mut s = String::new();
        s.push_str("[ ");

        // Sort fields by name for deterministic display
        let mut fields: Vec<_> = self.fields.iter().map(|(key, ty)| (*key, *ty)).collect();
        fields.sort_by_key(|(tag, _)| context.lookup(*tag));

        let mut fields = fields.iter().peekable();
        while let Some((tag, ty)) = fields.next() {
            s.push_str(context.lookup(*tag));
            if *ty != context.core_types().unit {
                s.push_str(" : ");
                s.push_str(&ty.display(context));
            }
            if fields.peek().is_some() {
                s.push_str(", ");
            }
        }
        s.push_str(" ]");

        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    // TODO: use a SmallVec or something like that because
    // most functions probably have a single signature
    pub signatures: Vec<FuncSignature>,

    /// Non-unit variants treated as a function returning an instance of that union for type checking,
    /// so this field distinguishes that case vs. a regular function for CFG construction
    pub variant: Option<(Idx<Type>, VariantIdx, Key)>,
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
