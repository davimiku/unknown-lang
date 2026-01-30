use std::ops;

use cranelift::prelude::{types, Type as CType};
use hir::{ContextDisplay, CoreTypes, Type as HType, VariantIdx};
use la_arena::{Arena, ArenaMap, Idx};

use crate::place::Pointer;
use crate::translate::FunctionTranslator;

#[derive(Debug, Clone)]
pub(crate) struct Layouts {
    cache: Arena<Layout>,
    map: ArenaMap<Idx<HType>, Idx<Layout>>,
    pub(crate) int: Idx<Layout>,
    pub(crate) float: Idx<Layout>,
}

impl Layouts {
    pub(crate) fn new(core_types: &CoreTypes) -> Self {
        let mut cache = Arena::default();
        let mut map = ArenaMap::default();

        let unit_layout = cache.alloc(Layout {
            fields: FieldsShape::Scalar,
            variants: VariantsShape::Empty,
            backend_repr: BackendRepr::None,
            size: Size::ZERO,
        });
        map.insert(core_types.unit, unit_layout);

        let int_layout = cache.alloc(Layout {
            fields: FieldsShape::Scalar,
            variants: VariantsShape::Single {
                index: VariantIdx::default(),
            },
            backend_repr: BackendRepr::Scalar(Scalar::Int),
            size: Size::ONE_WORD,
        });
        map.insert(core_types.int, int_layout);

        let float_layout = cache.alloc(Layout {
            fields: FieldsShape::Scalar,
            variants: VariantsShape::Single {
                index: VariantIdx::default(),
            },
            backend_repr: BackendRepr::Scalar(Scalar::Float),
            size: Size::ONE_WORD,
        });
        map.insert(core_types.float, float_layout);

        Self {
            cache,
            map,
            int: int_layout,
            float: float_layout,
        }
    }

    pub(crate) fn alloc(&mut self, layout: Layout, ty: Idx<HType>) -> Idx<Layout> {
        let layout_idx = self.cache.alloc(layout);
        self.map.insert(ty, layout_idx);
        layout_idx
    }

    pub(crate) fn get_cached_idx(&self, ty: Idx<HType>) -> Option<Idx<Layout>> {
        self.map.get(ty).copied()
    }

    pub(crate) fn get_cached(&self, ty: Idx<HType>) -> Option<&Layout> {
        self.map.get(ty).map(|idx| &self.cache[*idx])
    }
}

impl FunctionTranslator<'_> {
    pub(crate) fn layout_for_type(&mut self, ty: Idx<HType>) -> Idx<Layout> {
        // widen literal types -- TODO should add a helper in HIR crate?
        let ty = match self.context.type_(ty) {
            HType::FloatLiteral(_) => self.context.core_types().float,
            HType::IntLiteral(_) => self.context.core_types().int,
            // TODO - string literals might have a different layout (embedded or ptr to data section of binary)
            // HType::StringLiteral(key) => todo!(),
            _ => ty,
        };
        let cached = self.layouts.get_cached_idx(ty);
        if let Some(layout) = cached {
            return layout;
        }

        let layout = match self.context.type_(ty) {
            HType::Int | HType::IntLiteral(_) | HType::Float | HType::FloatLiteral(_) => {
                unreachable!(
                    "Internal Compiler Error (CLIF): Layout for {} was not cached",
                    ty.display(self.context)
                )
            }
            HType::String | HType::StringLiteral(_) => todo!(),
            HType::Sum(sum_type) => self.compute_sum_type_layout(sum_type),
            HType::UnionNamespace(_) => todo!("something like a product type, maybe"),
            HType::Product(product_type) => self.compute_product_type_layout(product_type),
            HType::Function(_function_type) => todo!(),
            HType::Array(_array_type) => todo!(),
            HType::Unit => {
                panic!();
            }
            HType::Bottom | HType::Top | HType::Error | HType::Unknown => {
                unreachable!(
                    "Internal Compiler Error (CLIF): Attempted to compute layout for {}",
                    ty.display(self.context)
                )
            }
        };
        self.layouts.alloc(layout, ty)
    }

    /// Compute the layout for a Sum type (tagged union)
    ///
    /// The layout depends on the variants:
    /// - If all variants are unit types (no data), the union is just a tag (scalar int)
    /// - If any variant has data, the entire union is stack-allocated (tag + payload)
    fn compute_sum_type_layout(&mut self, sum_type: &hir::SumType) -> Layout {
        let core_types = self.context.core_types();
        let unit_ty = core_types.unit;

        // Analyze all variants to determine the layout
        let mut has_any_payload = false;
        let mut max_payload_size = Size::ZERO;
        let mut variant_layouts: Vec<Idx<Layout>> = Vec::with_capacity(sum_type.variants.len());

        for (_key, variant_ty) in sum_type.variants.iter() {
            let variant_layout_idx = self.layout_for_type(*variant_ty);
            variant_layouts.push(variant_layout_idx);

            let variant_layout = &self.layouts[variant_layout_idx];

            // Check if this variant has a payload (not unit)
            if *variant_ty != unit_ty && !matches!(variant_layout.backend_repr, BackendRepr::None) {
                has_any_payload = true;
                if variant_layout.size > max_payload_size {
                    max_payload_size = variant_layout.size;
                }
            }
        }

        // Determine the backend representation
        if !has_any_payload {
            // Pure unit union - just a tag (enum without data)
            // e.g., `type Color = red | green | blue`
            Layout {
                fields: FieldsShape::Scalar,
                variants: VariantsShape::Multiple {
                    tag_encoding: TagEncoding::Explicit,
                    layouts: variant_layouts,
                },
                backend_repr: BackendRepr::Scalar(Scalar::Int),
                size: Size::ONE_WORD,
            }
        } else {
            // Union with payload - use stack allocation
            // Tag (1 word) + payload
            // e.g., `type Number = (int: Int | float: Float)` or nested unions
            let total_size = Size::ONE_WORD + max_payload_size;
            Layout {
                fields: FieldsShape::Fields {
                    offsets: vec![Size::ZERO, Size::ONE_WORD],
                },
                variants: VariantsShape::Multiple {
                    tag_encoding: TagEncoding::Explicit,
                    layouts: variant_layouts,
                },
                backend_repr: BackendRepr::StackSlot,
                size: total_size,
            }
        }
    }

    fn compute_product_type_layout(&mut self, product_type: &hir::ProductType) -> Layout {
        todo!()
    }
}

impl ops::Index<Idx<Layout>> for Layouts {
    type Output = Layout;

    fn index(&self, index: Idx<Layout>) -> &Self::Output {
        &self.cache[index]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Layout {
    /// Shape of fields, if this is a record
    pub fields: FieldsShape,

    /// Shape of variants, if this is a union
    pub variants: VariantsShape,

    /// How this layout will be translated to CLIF code
    pub backend_repr: BackendRepr,

    /// Total size of this layout
    pub size: Size,
}

/// The size of something, in number of bytes
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub(crate) struct Size {
    num_bytes: u64,
}

impl Default for Size {
    fn default() -> Self {
        Self::ZERO
    }
}

impl Size {
    pub(crate) const ZERO: Size = Size { num_bytes: 0 };

    pub(crate) const ONE_WORD: Size = Size { num_bytes: 8 };

    pub(crate) const TWO_WORDS: Size = Size { num_bytes: 16 };

    pub(crate) fn bytes(&self) -> u64 {
        self.num_bytes
    }

    pub(crate) fn num_words(&self) -> u64 {
        self.num_bytes / 8
    }
}

impl From<u64> for Size {
    fn from(num_bytes: u64) -> Self {
        Self { num_bytes }
    }
}

impl ops::Add for Size {
    type Output = Size;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            num_bytes: self.num_bytes + rhs.num_bytes,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum FieldsShape {
    Scalar,

    Fields {
        // index of Vec is FieldIdx - todo make newtype
        offsets: Vec<Size>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum BackendRepr {
    /// For ZST (unit)
    None,
    /// Scalar types being "simple" primitives
    Scalar(Scalar),
    /// String primitive or tag + scalar in sum type or 2 field product type
    /// NOTE: This is no longer used for unions - they all use StackSlot now.
    /// Keeping this for potential future use with strings or small structs.
    ScalarPair(Scalar, Scalar),
    /// Allocated on the stack - used for all non-unit unions
    /// The size is stored in the Layout's size field
    StackSlot,
}

#[derive(Debug, Clone)]
pub(crate) enum VariantsShape {
    /// basically for ZST or never types
    Empty,

    /// Items that are not a union, using a 0 sentinel value
    Single { index: VariantIdx },

    Multiple {
        tag_encoding: TagEncoding,
        layouts: Vec<Idx<Layout>>,
    },
}

/// How the tag is encoded in the value
///
/// Currently, only an explicit tag is supported but in the future
/// using a "niche" (such as how Rust encoded Option::None) may be
/// supported
#[derive(Debug, Clone)]
pub(crate) enum TagEncoding {
    Explicit,
    Niche { todo: () },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Scalar {
    Int,
    Float,
    Pointer(Pointer),
}

impl From<Scalar> for CType {
    fn from(value: Scalar) -> Self {
        match value {
            Scalar::Int => types::I64,
            Scalar::Float => types::F64,
            Scalar::Pointer(..) => types::I64,
        }
    }
}
