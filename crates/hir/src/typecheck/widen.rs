use la_arena::Idx;

use crate::{ArrayType, Context, Type};

/// Given a Type, if it is a literal then widen to the corresponding scalar.
/// Otherwise return the same Type.
///
/// This is used to handle the special/builtin subtype nature of the scalars
pub(super) fn widen_to_scalar(ty_idx: Idx<Type>, context: &mut Context) -> Idx<Type> {
    let ty = context.type_database.type_(ty_idx);
    match ty {
        Type::BoolLiteral(_) | Type::Bool => context.type_database.bool(),
        Type::FloatLiteral(_) | Type::Float => context.type_database.float(),
        Type::IntLiteral(_) | Type::Int => context.type_database.int(),
        Type::StringLiteral(_) | Type::String => context.type_database.string(),

        Type::Array(ArrayType { of }) => {
            let inner = context.type_database.type_(of);
            let inner = context.type_database.alloc_type(inner);
            let inner = widen_to_scalar(inner, context);
            context
                .type_database
                .alloc_type(Type::Array(ArrayType { of: inner }))
        }

        _ => ty_idx,
    }
}
