use la_arena::Idx;

use crate::typecheck::{CoreTypes, FuncSignature, Type};
use crate::{IntrinsicExpr, TypeSymbol};
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
    #[strum(serialize = "String.byte_len")]
    StringByteLen,
    #[strum(serialize = "String.concat")]
    StringConcat,

    #[strum(serialize = "Int.add")]
    IntAdd,
    #[strum(serialize = "Int.sub")]
    IntSub,
    #[strum(serialize = "Int.mul")]
    IntMul,
    #[strum(serialize = "Int.div")]
    IntDiv,
    #[strum(serialize = "Int.rem")]
    IntRem,
    #[strum(serialize = "Int.eq")]
    IntEq,
    #[strum(serialize = "Int.ne")]
    IntNe,
    #[strum(serialize = "Int.lt")]
    IntLt,
    #[strum(serialize = "Int.le")]
    IntLe,
    #[strum(serialize = "Int.gt")]
    IntGt,
    #[strum(serialize = "Int.ge")]
    IntGe,

    #[strum(serialize = "Float.add")]
    FloatAdd,
    #[strum(serialize = "Float.sub")]
    FloatSub,
    #[strum(serialize = "Float.mul")]
    FloatMul,
    #[strum(serialize = "Float.div")]
    FloatDiv,
    #[strum(serialize = "Float.eq")]
    FloatEq,
    #[strum(serialize = "Float.ne")]
    FloatNe,
    #[strum(serialize = "Float.lt")]
    FloatLt,
    #[strum(serialize = "Float.le")]
    FloatLe,
    #[strum(serialize = "Float.gt")]
    FloatGt,
    #[strum(serialize = "Float.ge")]
    FloatGe,
}

pub(crate) const ARRAY_ELEMENT_TYPE_PARAMETER: TypeSymbol = TypeSymbol::new(u32::MAX, 0);

impl BuiltinId {
    pub(crate) fn operator(self) -> Option<IntrinsicExpr> {
        use BuiltinId as B;
        use IntrinsicExpr as I;

        Some(match self {
            B::IntAdd | B::FloatAdd => I::Add,
            B::IntSub | B::FloatSub => I::Sub,
            B::IntMul | B::FloatMul => I::Mul,
            B::IntDiv | B::FloatDiv => I::Div,
            B::IntRem => I::Rem,
            B::IntEq | B::FloatEq => I::Eq,
            B::IntNe | B::FloatNe => I::Ne,
            B::IntLt | B::FloatLt => I::Lt,
            B::IntLe | B::FloatLe => I::Le,
            B::IntGt | B::FloatGt => I::Gt,
            B::IntGe | B::FloatGe => I::Ge,
            B::ArrayNew
            | B::ArrayLen
            | B::ArrayCapacity
            | B::ArrayReserve
            | B::ArrayWriteNext
            | B::StringByteLen => return None,
            B::StringConcat => I::Concat,
        })
    }
}

pub fn signature(
    id: BuiltinId,
    types: &CoreTypes,
    type_parameter: TypeSymbol,
    array_element_ty: Idx<Type>,
    array_ty: Idx<Type>,
) -> Type {
    match id {
        BuiltinId::StringByteLen => numeric_unary(types.string, types.int),
        BuiltinId::StringConcat => numeric_binary(types.string, types.string, types.string),
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
        BuiltinId::IntAdd | BuiltinId::IntSub | BuiltinId::IntMul => {
            numeric_binary(types.int, types.int, types.int)
        }
        BuiltinId::IntDiv => numeric_binary(types.int, types.int, types.float),
        BuiltinId::IntRem => numeric_binary(types.int, types.int, types.int),
        BuiltinId::IntEq
        | BuiltinId::IntNe
        | BuiltinId::IntLt
        | BuiltinId::IntLe
        | BuiltinId::IntGt
        | BuiltinId::IntGe => numeric_binary(types.int, types.int, types.bool),
        BuiltinId::FloatAdd | BuiltinId::FloatSub | BuiltinId::FloatMul | BuiltinId::FloatDiv => {
            numeric_binary(types.float, types.float, types.float)
        }
        BuiltinId::FloatEq
        | BuiltinId::FloatNe
        | BuiltinId::FloatLt
        | BuiltinId::FloatLe
        | BuiltinId::FloatGt
        | BuiltinId::FloatGe => numeric_binary(types.float, types.float, types.bool),
    }
}

fn numeric_binary(lhs: Idx<Type>, rhs: Idx<Type>, result: Idx<Type>) -> Type {
    Type::func(vec![FuncSignature {
        params: Box::new([lhs, rhs]),
        return_ty: result,
        type_params: Box::new([]),
    }])
}

fn numeric_unary(param: Idx<Type>, result: Idx<Type>) -> Type {
    Type::func(vec![FuncSignature {
        params: Box::new([param]),
        return_ty: result,
        type_params: Box::new([]),
    }])
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
        assert_eq!(BuiltinId::from_str("Int.add"), Ok(BuiltinId::IntAdd));
        assert_eq!(BuiltinId::from_str("Float.lt"), Ok(BuiltinId::FloatLt));
        assert_eq!(
            BuiltinId::from_str("String.byte_len"),
            Ok(BuiltinId::StringByteLen)
        );
        assert_eq!(
            BuiltinId::from_str("String.concat"),
            Ok(BuiltinId::StringConcat)
        );
        assert_eq!(
            BuiltinId::StringConcat.operator(),
            Some(crate::IntrinsicExpr::Concat)
        );
        assert!(BuiltinId::from_str("List.new").is_err());
    }
}
