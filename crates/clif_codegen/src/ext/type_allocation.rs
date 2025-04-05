//! Determines memory layout, byte and word sizes, alignment, etc. for types
//!
//! This lives in the CLIF crate and not the HIR because this information is specific to
//! CLIF codegen, not relevant to other backends (such as JavaScript codegen)

pub(crate) struct Layout {
    // todo!
    // size on the stack
    // size on the heap
    // header/metadata prepended to the heap data
    // anything special on the stack like packing in string length or "is ASCII" or something else
}

pub(crate) enum AllocationStrategy {
    /// Uses the Cranelift concept of a "Variable", which is for types that
    /// fit within a word and are stack allocated
    Variable,

    /// Uses the Cranelift stack slots for types larger than a word, but are still
    /// stack allocated. Roughly speaking, this would be structs or unions w/ variant data
    /// that is too large to split into variables but not large enough to warrant putting on the heap
    StackSlot,

    /// Uses a pointer on the stack which points to data on the heap. The first word (?) is
    /// the reference count for the data. Memory is deallocated when the reference count
    /// goes to zero
    HeapReferenceCounted,

    /// Uses a pointer on the stack
    HeapGarbageCollected,
    // TODO - "wide pointer"? two pointers basically, 1 to data and 1 to vtable or function

    // TODO - "length included pointer"? for strings or immutable lists, 1 word pointer to data
    // and 1 word containing the length and maybe other stuff packed in
}

pub(crate) trait TypeAllocation {
    fn allocation_strategy(&self, context: &hir::Context) -> AllocationStrategy;

    fn word_size(&self, context: &hir::Context) -> u32;

    fn byte_size(&self, context: &hir::Context) -> u32 {
        self.word_size(context) * 8
    }
}

impl TypeAllocation for hir::Type {
    fn allocation_strategy(&self, context: &hir::Context) -> AllocationStrategy {
        todo!()
    }
    fn word_size(&self, context: &hir::Context) -> u32 {
        match self {
            hir::Type::Unit => 0,
            hir::Type::FloatLiteral(_) | hir::Type::Float => 1,
            hir::Type::IntLiteral(_) | hir::Type::Int => 1,
            hir::Type::StringLiteral(key) => todo!(),
            hir::Type::String => todo!(),
            hir::Type::Sum(sum_type) => sum_type.word_size(context),
            hir::Type::Function(function_type) => todo!(),
            hir::Type::Array(array_type) => todo!(),
            hir::Type::Top | hir::Type::Bottom | hir::Type::Unknown | hir::Type::Error => {
                unreachable!(
                    "Internal Compiler Error (CLIF): Invalid type for size calculation: {self:?}",
                )
            }
        }
    }
}

impl TypeAllocation for hir::SumType {
    fn allocation_strategy(&self, context: &hir::Context) -> AllocationStrategy {
        match self.word_size(context) {
            1 => AllocationStrategy::Variable,
            2 => AllocationStrategy::Variable,
            words if words <= 8 => AllocationStrategy::StackSlot,
            _ => todo!(),
        }
    }

    fn word_size(&self, context: &hir::Context) -> u32 {
        let max = self
            .variants
            .iter()
            .map(|(.., idx)| context.type_(*idx).word_size(context))
            .max()
            .unwrap_or_default();
        // add the tag
        max + 1
    }
}
