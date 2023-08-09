use cranelift::prelude::{AbiParam, Signature, Type};
use cranelift_jit::JITModule;
use cranelift_module::{DataId, FuncId, FuncOrDataId, Module};

pub(crate) trait JITModuleExt {
    fn make_sig_types(&self, params: &[Type], returns: &[Type]) -> Signature;

    fn get_func_id(&self, name: &str) -> Option<FuncId>;

    fn get_data_id(&self, name: &str) -> Option<DataId>;
}

impl JITModuleExt for JITModule {
    fn make_sig_types(&self, params: &[Type], returns: &[Type]) -> Signature {
        let mut sig = self.make_signature();

        for param in params {
            sig.params.push(AbiParam::new(*param));
        }
        for ret in returns {
            sig.returns.push(AbiParam::new(*ret));
        }

        sig
    }

    fn get_func_id(&self, name: &str) -> Option<FuncId> {
        match self.get_name(name) {
            Some(FuncOrDataId::Func(id)) => Some(id),
            _ => None,
        }
    }

    fn get_data_id(&self, name: &str) -> Option<DataId> {
        match self.get_name(name) {
            Some(FuncOrDataId::Data(id)) => Some(id),
            _ => None,
        }
    }
}
