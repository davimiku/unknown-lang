use std::collections::HashMap;

use cranelift::codegen::{print_errors, CodegenError};
use cranelift_module::ModuleError;
use jit::JIT;

mod builtins;
mod ext;
mod jit;
mod layout;
mod macros;
mod place;
#[cfg(test)]
mod sandbox;
mod stackmaps;
#[cfg(test)]
mod tests;
mod translate;

type EntryPoints = HashMap<String, *const u8>;

/// Compiles a module and returns untyped JIT entry addresses.
///
/// # Calling returned entry points
///
/// Calling a raw address requires matching its ABI. Since automatic GC is
/// enabled, native callers must not retain unrooted GC results across possible
/// collections (including GC references inside returned records or unions).
/// For a no-argument String-returning entry, use
/// `runtime::NativeRoot::call_no_arg_string` to register its result before
/// another Hopper thread can collect.
pub fn compile_module(input: &str) -> Result<EntryPoints, CompileErrors> {
    let (module, context) = mir::construct(input);
    assert_eq!(context.diagnostics, vec![]);

    let mut jit = JIT::with_builtins();
    let mut func_map = HashMap::new();

    match jit.compile_module(&module, &mut func_map, &context) {
        Ok(()) => {}
        Err(err @ CompileErrors::UnsupportedGcAggregate { .. }) => return Err(err),
        Err(err) => {
            for err in err.compilation_errors() {
                let pretty = print_errors::pretty_error(&jit.ctx.func, err);
                println!("{pretty}");
            }
            println!("{}", &jit.ctx.func.display());
            panic!("Internal Compiler Error (CLIF): See codegen errors above")
        }
    };

    match jit.module.finalize_definitions() {
        Ok(()) => {}
        Err(err) => {
            if let ModuleError::Compilation(err) = err {
                println!("{}", print_errors::pretty_error(&jit.ctx.func, err));
            } else {
                println!("{err}");
            }
            panic!("ICE: error while finalizing definitions");
        }
    };

    jit.register_stack_maps();

    // map all the entry points to executable functions
    {
        let entry_points: EntryPoints = module
            .entry_points
            .iter()
            .map(|(key, mir_func_id)| {
                let name = context.lookup(*key).to_string();
                let func_id = jit
                    .entry_point_wrappers
                    .get(mir_func_id)
                    .copied()
                    .unwrap_or(func_map[mir_func_id]);
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
    /// An error that occurred at the module level
    Module(Box<ModuleError>),

    /// Error(s) that occurred for individual functions
    Function(FunctionErrors),

    /// This GC-bearing aggregate does not yet have precise roots.
    UnsupportedGcAggregate { function: String, ty: String },
}

impl CompileErrors {
    fn compilation_errors(self) -> Vec<CodegenError> {
        match self {
            CompileErrors::UnsupportedGcAggregate { .. } => vec![],
            CompileErrors::Module(err) => {
                if let ModuleError::Compilation(err) = *err {
                    vec![err]
                } else {
                    vec![]
                }
            }
            CompileErrors::Function(errors) => {
                let mut v = vec![];
                for (_, err) in errors {
                    if let ModuleError::Compilation(err) = err {
                        v.push(err);
                    }
                }
                v
            }
        }
    }
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
