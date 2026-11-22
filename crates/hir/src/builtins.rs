use la_arena::Idx;

use crate::typecheck::{CoreTypes, FuncSignature, Type};
use crate::TypeSymbol;
use strum::EnumString;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
pub enum BuiltinId {
    #[strum(serialize = "Array.new")]
    ArrayNew,
    #[strum(serialize = "Array.len")]
    ArrayLen,
    #[strum(serialize = "Array.capacity")]
    ArrayCapacity,
    #[strum(serialize = "Array.reserve")]
    ArrayReserve,
    #[strum(serialize = "Array.write_next")]
    ArrayWriteNext,
}

pub(crate) const ARRAY_ELEMENT_TYPE_PARAMETER: TypeSymbol = TypeSymbol::new(u32::MAX, 0);

pub fn signature(
    id: BuiltinId,
    types: &CoreTypes,
    type_parameter: TypeSymbol,
    array_element_ty: Idx<Type>,
    array_ty: Idx<Type>,
) -> Type {
    match id {
        BuiltinId::ArrayNew => Type::func(vec![FuncSignature {
            params: Box::new([types.int]),
            return_ty: array_ty,
            type_params: Box::new([type_parameter]),
        }]),
        BuiltinId::ArrayLen | BuiltinId::ArrayCapacity => Type::func(vec![FuncSignature {
            params: Box::new([array_ty]),
            return_ty: types.int,
            type_params: Box::new([type_parameter]),
        }]),
        BuiltinId::ArrayReserve => Type::func(vec![FuncSignature {
            params: Box::new([array_ty, types.int]),
            return_ty: types.unit,
            type_params: Box::new([type_parameter]),
        }]),
        BuiltinId::ArrayWriteNext => Type::func(vec![FuncSignature {
            params: Box::new([array_ty, types.int, array_element_ty]),
            return_ty: types.unit,
            type_params: Box::new([type_parameter]),
        }]),
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::BuiltinId;

    #[test]
    fn builtin_attribute_names_are_qualified() {
        assert_eq!(BuiltinId::from_str("Array.new"), Ok(BuiltinId::ArrayNew));
        assert_eq!(BuiltinId::from_str("Array.len"), Ok(BuiltinId::ArrayLen));
        assert_eq!(
            BuiltinId::from_str("Array.capacity"),
            Ok(BuiltinId::ArrayCapacity)
        );
        assert_eq!(
            BuiltinId::from_str("Array.reserve"),
            Ok(BuiltinId::ArrayReserve)
        );
        assert_eq!(
            BuiltinId::from_str("Array.write_next"),
            Ok(BuiltinId::ArrayWriteNext)
        );
        assert!(BuiltinId::from_str("List.new").is_err());
    }
}
