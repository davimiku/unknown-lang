use std::{
    cmp,
    ops::{self, Index},
};

use cranelift::prelude::{types, Type as CType};
use hir::{Context, ContextDisplay, CoreTypes, Type as HType, VariantIdx, VecVariantIdx};
use la_arena::{Arena, ArenaMap, Idx};

use crate::place::Pointer;

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

    pub(crate) fn for_type(&mut self, ty: Idx<HType>, context: &Context) -> Idx<Layout> {
        // widen literal types -- TODO should add a helper in HIR crate?
        let ty = match context.type_(ty) {
            HType::FloatLiteral(_) => context.core_types().float,
            HType::IntLiteral(_) => context.core_types().int,
            HType::StringLiteral(key) => todo!(),
            _ => ty,
        };
        let cached = self.map.get(ty).copied();
        if let Some(layout) = cached {
            return layout;
        }

        let layout = match context.type_(ty) {
            HType::Int | HType::IntLiteral(_) | HType::Float | HType::FloatLiteral(_) => {
                unreachable!(
                    "Internal Compiler Error (CLIF): Layout for {} was not cached",
                    ty.display(context)
                )
            }
            HType::String | HType::StringLiteral(_) => todo!(),
            HType::Sum(sum_type) => {
                let mut max_size: Size = Size::ZERO;
                let mut variant_layouts: Vec<Idx<Layout>> = vec![];
                for (_, variant_ty) in &sum_type.variants {
                    let layout = self.for_type(*variant_ty, context);
                    variant_layouts.push(layout);
                    let layout = &self.cache[layout];
                    if layout.size > max_size {
                        max_size = layout.size;
                    }
                }

                Layout {
                    fields: FieldsShape::Scalar,
                    variants: VariantsShape::Multiple {
                        tag: Scalar::Int,
                        tag_encoding: TagEncoding::Direct,
                        tag_field: 0,
                        layouts: variant_layouts,
                    },
                    backend_repr: BackendRepr::Scalar(Scalar::Int),
                    size: Size::ONE_WORD + max_size, // one word for the tag
                }
            }
            HType::Function(function_type) => todo!(),
            HType::Array(array_type) => todo!(),
            HType::Unit => {
                panic!();
            }
            HType::Bottom | HType::Top | HType::Error | HType::Unknown => {
                unreachable!(
                    "Internal Compiler Error (CLIF): Attempted to compute layout for {}",
                    ty.display(context)
                )
            }
        };
        let layout_idx = self.cache.alloc(layout);
        self.map.insert(ty, layout_idx);
        layout_idx
    }
}

impl Index<Idx<Layout>> for Layouts {
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

impl Size {
    const ZERO: Size = Size { num_bytes: 0 };

    const ONE_WORD: Size = Size { num_bytes: 8 };

    const TWO_WORDS: Size = Size { num_bytes: 16 };
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
        // index is FieldIdx - todo make newtype
        offsets: Vec<Size>,
        // memory_offsets: Vec<u32>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum BackendRepr {
    None, // for unit or ZST (maybe?)
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    // SimdVector
    Memory, // todo - pointer, or wide pointer
}

#[derive(Debug, Clone)]
pub(crate) enum VariantsShape {
    Empty, // might not be used, basically for ZST or never types

    Single {
        index: VariantIdx, // 0 sentinel value for everything but unions
    },

    Multiple {
        tag: Scalar,
        tag_encoding: TagEncoding,
        tag_field: usize, // TODO - this was shamelessly copied from rustc_codegen_clif, find out if we need it
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
