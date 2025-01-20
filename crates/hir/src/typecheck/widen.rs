use la_arena::Idx;

use crate::{ArrayType, Context, Type};

/// Given a Type, if it is a literal then widen to the corresponding scalar.
/// For compound types, this is applied recursively to the inner types.
/// Otherwise return the same Type.
///
/// For example, an `IntLiteral(123)` is widened to `Int`
///
/// This is used to help with the subtype nature of the literals. One use case is
/// inferring `let mut a = 10`. This should be inferred as `Int` rather than `IntLiteral(10)`
/// because it may be reassigned.
pub(super) fn widen_to_scalar(ty_idx: Idx<Type>, context: &mut Context) -> Idx<Type> {
    let ty = context.type_(ty_idx);
    match ty {
        Type::FloatLiteral(_) | Type::Float => context.core_types().float,
        Type::IntLiteral(_) | Type::Int => context.core_types().int,
        Type::StringLiteral(_) | Type::String => context.core_types().string,

        // Sum types...? Don't think so
        Type::Array(ArrayType { of }) => {
            let of = widen_to_scalar(*of, context);
            // TODO: pre-allocate `Array Bool`, `Array Float`, `Array Int`, `Array String`
            context
                .type_database
                .alloc_type(Type::Array(ArrayType { of }))
        }

        _ => ty_idx,
    }
}
