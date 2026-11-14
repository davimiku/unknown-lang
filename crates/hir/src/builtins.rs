use crate::typecheck::{CoreTypes, FuncSignature, Type};
use strum::EnumString;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
pub enum BuiltinId {
    #[strum(serialize = "list_len")]
    ListLen,
}

pub fn signature(id: BuiltinId, types: &CoreTypes) -> Type {
    match id {
        BuiltinId::ListLen => Type::func(vec![FuncSignature {
            params: Box::new([types.top]),
            return_ty: types.int,
        }]),
    }
}
