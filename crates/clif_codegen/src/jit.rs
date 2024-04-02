use std::collections::HashMap;

use cranelift::codegen::print_errors::{self, pretty_error};
#[cfg(debug_assertions)]
use cranelift::codegen::write_function;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError, ModuleResult};

use crate::ext::jit_builder::JITBuilderExt;
use crate::ext::jit_module::JITModuleExt;
use crate::translate::FunctionTranslator;
use crate::{builtins, CompileErrors, FunctionErrors};

/// The basic JIT class.
#[allow(clippy::upper_case_acronyms)]
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    pub(crate) builder_ctx: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation with a
    /// context per thread.
    pub(crate) ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    pub(crate) data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd functions.
    pub(crate) module: JITModule,

    /// Map between builtin names and their Cranelift function ID.
    pub(crate) builtins: HashMap<String, FuncId>,
    // Map between MIR functions and CLIF functions
    // pub(crate) func_id_map: HashMap<mir::FuncId, FuncId>,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::with_builtins();
        let module = JITModule::new(builder);
        let ctx = module.make_context();

        Self {
            ctx,
            module,
            builder_ctx: FunctionBuilderContext::new(),
            data_description: DataDescription::new(),
            builtins: Default::default(),
        }
    }
}
impl JIT {
    // TODO: make this the Default ?
    pub(crate) fn with_builtins() -> Self {
        let mut jit = JIT::default();

        let builtins = vec![
            (&[I64], &[], builtins::PRINT_INT),
            (&[F64], &[], builtins::PRINT_FLOAT),
            (&[I64], &[], builtins::PRINT_BOOL),
        ];

        for (params, returns, name) in builtins {
            let sig = jit.module.make_sig_types(params, returns);
            let func_id = jit
                .module
                .declare_function(name, Linkage::Import, &sig)
                .unwrap();
            jit.builtins.insert(name.to_string(), func_id);
        }

        jit
    }
}

impl JIT {
    /// Compiles the full program and returns a pointer to `main`
    ///
    /// In normal mode, the `main` function is defined by the user.
    /// TODO: will it always be `[]String -> Int` or can the user define it with
    /// arbitrary parameter/return types?
    ///
    /// In script mode, `main` is a synthetic function that wraps the script,
    /// takes a `[]String` argument for the CLI args, and `return 0` on success.
    pub fn compile_module(
        &mut self,
        module: &mir::Module,
        func_map: &mut HashMap<mir::FuncId, FuncId>,
        hir_context: &hir::Context,
    ) -> Result<(), CompileErrors> {
        let mut errors: FunctionErrors = Default::default();

        for (_, func) in module.functions.iter() {
            match self.compile_function(func, func_map, hir_context) {
                Ok(func_id) => {
                    func_map.insert(func.id, func_id);
                }
                Err(err) => {
                    errors.insert(func.id, err);
                }
            };
        }

        if !errors.is_empty() {
            return Err(errors.into());
        }
        Ok(())
    }

    /// Compiles a function expression within this module
    pub fn compile_function(
        &mut self,
        func: &mir::Function,
        func_map: &mut HashMap<mir::FuncId, FuncId>,
        hir_context: &hir::Context,
    ) -> ModuleResult<FuncId> {
        {
            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let builder = FunctionBuilder::new(&mut self.ctx.func, &mut fn_builder_ctx);
            let translator =
                FunctionTranslator::new(builder, &mut self.module, func, func_map, hir_context);

            translator.translate_function();
        }

        let func_name = func.name.as_deref().unwrap_or("{anonymous}");

        let func_id =
            self.module
                .declare_function(func_name, Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(func_id, &mut self.ctx)?;

        // TODO: for garbage collection, need to get the stack maps something
        // like this and store them somewhere global-ish (accessible by the runtime,
        // could be in a lazy_static or once_cell)
        // Questions:
        //   1. How to get the &dyn TargetIsa to pass into compile_and_emit ?
        //   2. How to get the ControlPlane to pass into compile_and_emit ?

        // // is this how to get a &dyn TargetIsa?
        // let shared_builder = settings::builder();
        // let shared_flags = settings::Flags::new(shared_builder);
        // let isa_builder = isa::lookup(triple!("x86_64")).unwrap();

        // is this how to get a ControlPlane?
        // let control_plane = ControlPlane::default();
        // let mut emit_buffer = Vec::new();
        // let context = self.module.make_context();
        // let res = context.compile_and_emit(isa, &mut emit_buffer, ctrl_plane);
        // let stack_maps = res.unwrap().buffer.stack_maps();
        // stack_maps[0].stack_map;

        #[cfg(debug_assertions)]
        {
            let mut s = String::new();
            write_function(&mut s, &self.ctx.func).unwrap_or_else(|err| {
                dbg!(err);
            });
            println!("{s}");
        }

        self.module.clear_context(&mut self.ctx);

        Ok(func_id)
    }
}
