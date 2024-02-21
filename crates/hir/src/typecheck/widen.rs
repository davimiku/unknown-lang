use la_arena::Idx;

use crate::{ArrayType, Context, Type};

/// Given a Type, if it is a literal then widen to the corresponding scalar.
/// For compound types, this is applied recursively to the inner types.
/// Otherwise return the same Type.
///
/// For example, an `IntLiteral(123)` is widened to `Int`
///
/// This is used to handle the subtype nature of the literals
pub(super) fn widen_to_scalar(ty_idx: Idx<Type>, context: &mut Context) -> Idx<Type> {
    let ty = context.type_(ty_idx);
    match ty {
        Type::BoolLiteral(_) | Type::Bool => context.core_types().bool,
        Type::FloatLiteral(_) | Type::Float => context.core_types().float,
        Type::IntLiteral(_) | Type::Int => context.core_types().int,
        Type::StringLiteral(_) | Type::String => context.core_types().string,

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
