use std::collections::HashMap;

use cranelift::codegen::ir::UserStackMap;
use cranelift::codegen::print_errors::{self, pretty_error};
#[cfg(debug_assertions)]
use cranelift::codegen::write_function;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleResult};
use itertools::Itertools;

use crate::ext::jit_builder::JITBuilderExt;
use crate::ext::jit_module::JITModuleExt;
use crate::stackmaps::{PendingFunctionStackMap, PendingSafepoint};
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

    pub(crate) pending_stack_maps: HashMap<FuncId, PendingFunctionStackMap>,
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
            pending_stack_maps: Default::default(),
        }
    }
}

impl JIT {
    // TODO: make this the Default ?
    pub(crate) fn with_builtins() -> Self {
        let mut jit = JIT::default();

        let ptr_ty = I64;
        let ptr = [ptr_ty];
        let ptr_len = [ptr_ty, I64];
        let builtins: Vec<(&[Type], &[Type], &str)> = vec![
            (&ptr_len, &ptr, builtins::ALLOC_STRING),
            (&[I64], &[], builtins::PRINT_INT),
            (&[F64], &[], builtins::PRINT_FLOAT),
            (&[I64], &[], builtins::PRINT_BOOL),
            (&ptr, &[], builtins::PRINT_STRING),
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

        let compiled = self.ctx.compiled_code().expect("compiled code to exist");

        let code_size = compiled.buffer.total_size();

        type ReturnAddressOffset = u32; // aliases to help remember what these are
        type StackSpan = u32;
        type StackMap = (ReturnAddressOffset, StackSpan, UserStackMap);
        let stack_maps: &[StackMap] = compiled.buffer.user_stack_maps();
        // Call-site frame offsets may not be provided by Cranelift
        // The frame layout describes the fixed frame's bottom (SP) relative to its FP
        let frame_to_fp_offset = compiled
            .buffer
            .frame_layout()
            .expect("ICE: compiled Hopper function to have a frame layout")
            .frame_to_fp_offset;

        let mut safepoints: Vec<PendingSafepoint> = vec![];
        for (return_address_offset, stack_span, stack_map) in stack_maps {
            let root_offsets = stack_map
                .entries()
                .map(|(ty, offset)| {
                    assert_eq!(ty, I64, "ICE: GcRefs to be one word");
                    offset
                })
                .collect_vec();

            safepoints.push(PendingSafepoint {
                return_address_offset: *return_address_offset,
                stack_span: *stack_span,
                root_offsets,
            });
        }

        self.pending_stack_maps.insert(
            func_id,
            PendingFunctionStackMap {
                code_size,
                frame_to_fp_offset,
                safepoints,
            },
        );

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
