use std::ops;

use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::{types, StackSlotData, StackSlotKind, Type as CType};
use hir::{ContextDisplay, CoreTypes, Type as HType, VariantIdx};
use la_arena::{Arena, ArenaMap, Idx};

use crate::{place::Pointer, translate::FunctionTranslator};

const ALIGN_SHIFT: u8 = 3;

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
            HType::Sum(sum_type) => {
                todo!()
            }
            HType::Function(function_type) => todo!(),
            HType::Array(array_type) => todo!(),
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
}

impl ops::Index<Idx<Layout>> for Layouts {
    type Output = Layout;

    fn index(&self, index: Idx<Layout>) -> &Self::Output {
        &self.cache[index]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Layout {
    pub fields: FieldsShape,

    pub variants: VariantsShape,

    pub backend_repr: BackendRepr,

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
        // memory_offsets: Vec<u32>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum BackendRepr {
    None, // for unit or ZST (maybe?)
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar), // tag + scalar in sum type or 2 field product type
    ScalarTriple(Scalar, Scalar, Scalar), // tag + 2 field product type in sum type, 3 field product type, etc.
    ScalarMemory(Scalar, Memory),         // tag + pointer
                                          // SimdVector?
                                          // todo - wide pointer (data+vtable or closure data+fnptr)
}

/// Representation
#[derive(Debug, Clone)]
pub(crate) enum Memory {
    Inline,
    Allocated,
}

#[derive(Debug, Clone)]
pub(crate) enum VariantsShape {
    Empty, // might not be used, basically for ZST or never types

    Single {
        index: VariantIdx, // 0 sentinel value for everything but unions
    },

    Multiple {
        // tag: Scalar, // TODO - tag is always Int scalar? remove?
        tag_encoding: TagEncoding,

        /// At least one variant has an underlying representation of each of Int and Float
        mixed_int_float: bool,

        layouts: Vec<Idx<Layout>>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum TagEncoding {
    Direct,
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
