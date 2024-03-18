use std::collections::HashMap;

use cranelift_module::ModuleError;
use jit::JIT;

mod builtins;
mod ext;
mod jit;
#[cfg(test)]
mod tests;
mod translate;

type EntryPoints = HashMap<String, *const u8>;

pub fn compile_module(input: &str) -> Result<EntryPoints, CompileErrors> {
    let (module, context) = mir::construct(input);
    assert_eq!(context.diagnostics, vec![]);

    let mut jit = JIT::with_builtins();
    let mut func_map = HashMap::new();

    jit.compile_module(&module, &mut func_map, &context)?;

    jit.module.finalize_definitions()?;

    // map all the entry points to executable functions
    {
        let entry_points: EntryPoints = module
            .entry_points
            .iter()
            .map(|(key, mir_func_id)| {
                let name = context.lookup(*key).to_string();
                let func_id = func_map[mir_func_id];
                let func_ptr = jit.module.get_finalized_function(func_id);
                (name, func_ptr)
            })
            .collect();

        Ok(entry_points)
    }
}

pub type FunctionErrors = HashMap<mir::FuncId, ModuleError>;

#[derive(Debug)]
pub enum CompileErrors {
    Module(Box<ModuleError>),
    Function(FunctionErrors),
}

impl From<FunctionErrors> for CompileErrors {
    fn from(errors: FunctionErrors) -> Self {
        Self::Function(errors)
    }
}

impl From<ModuleError> for CompileErrors {
    fn from(error: ModuleError) -> Self {
        Self::Module(Box::new(error))
    }
}
