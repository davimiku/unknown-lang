use la_arena::Idx;

use crate::typecheck::{CoreTypes, FuncSignature, Type};
use crate::TypeSymbol;
use strum::EnumString;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
pub enum BuiltinId {
    #[strum(serialize = "list_new")]
    ListNew,
    #[strum(serialize = "list_push")]
    ListPush,
    #[strum(serialize = "list_len")]
    ListLen,
}

pub(crate) const LIST_ELEMENT_TYPE_PARAMETER: TypeSymbol = TypeSymbol::new(u32::MAX, 0);

pub fn signature(
    id: BuiltinId,
    types: &CoreTypes,
    list_element_ty: Idx<Type>,
    list_ty: Idx<Type>,
) -> Type {
    match id {
        BuiltinId::ListNew => Type::func(vec![FuncSignature {
            params: Box::new([]),
            return_ty: list_ty,
            type_params: Box::new([LIST_ELEMENT_TYPE_PARAMETER]),
        }]),
        BuiltinId::ListPush => Type::func(vec![FuncSignature {
            params: Box::new([list_ty, list_element_ty]),
            return_ty: types.unit,
            type_params: Box::new([LIST_ELEMENT_TYPE_PARAMETER]),
        }]),
        BuiltinId::ListLen => Type::func(vec![FuncSignature {
            params: Box::new([list_ty]),
            return_ty: types.int,
            type_params: Box::new([LIST_ELEMENT_TYPE_PARAMETER]),
        }]),
    }
}
