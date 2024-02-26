use std::collections::HashMap;

#[cfg(debug_assertions)]
use cranelift::codegen::write_function;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleResult};

use crate::builtins;
use crate::ext::jit_builder::JITBuilderExt;
use crate::ext::jit_module::JITModuleExt;
use crate::translate::FunctionTranslator;

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
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::with_builtins();
        let module = JITModule::new(builder);

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            builtins: HashMap::new(),
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
    pub fn compile(
        &mut self,
        // expr: Idx<Expr>,
        context: &mir::Builder,
    ) -> ModuleResult<*const u8> {
        todo!()
    }

    /// Compiles a function expression within this module
    pub fn compile_function(
        &mut self,
        func: &mir::Function,
        hir_context: &hir::Context,
    ) -> ModuleResult<*const u8> {
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut fn_builder_ctx);
        let mut translator =
            FunctionTranslator::new(&mut builder, &mut self.module, hir_context, func);

        translator.translate_function();

        let func_name = func
            .name
            .map_or("{anonymous}", |(key, ..)| hir_context.lookup(key));

        let func_id =
            self.module
                .declare_function(func_name, Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(func_id, &mut self.ctx)?;

        #[cfg(debug_assertions)]
        {
            let mut s = String::new();
            write_function(&mut s, &self.ctx.func).unwrap_or_else(|err| {
                dbg!(err);
            });
            println!("{s}");
        }

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(func_id);

        Ok(code)
    }
}

// self.module.get_finalized_function(func_id)
//         // Then, translate the AST nodes into Cranelift IR.
//         // self.translate(params, the_return, stmts)?;

//         // Next, declare the function to jit. Functions must be declared
//         // before they can be called, or defined.
//         //
//         // TODO: This may be an area where the API should be streamlined; should
//         // we have a version of `declare_function` that automatically declares
//         // the function?
//         // let id = self
//         //     .module
//         //     .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
//         //     .map_err(|e| e.to_string())?;

//         // Define the function to jit. This finishes compilation, although
//         // there may be outstanding relocations to perform. Currently, jit
//         // cannot finish relocations until all functions to be called are
//         // defined. For this toy demo for now, we'll just finalize the
//         // function below.
//         // self.module
//         //     .define_function(id, &mut self.ctx)
//         //     .map_err(|e| e.to_string())?;

//         // Now that compilation is finished, we can clear out the context state.
//         // self.module.clear_context(&mut self.ctx);

//         // Finalize the functions which we just defined, which resolves any
//         // outstanding relocations (patching in addresses, now that they're
//         // available).
//         // self.module.finalize_definitions().unwrap();

//         // We can now retrieve a pointer to the machine code.
//         // let code = self.module.get_finalized_function(id);

//         // Ok(code)
//     }

//     /// Create a zero-initialized data section.
//     pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
//         // The steps here are analogous to `compile`, except that data is much
//         // simpler than functions.
//         self.data_description.define(contents.into_boxed_slice());
//         let id = self
//             .module
//             .declare_data(name, Linkage::Export, true, false)
//             .map_err(|e| e.to_string())?;

//         self.module
//             .define_data(id, &self.data_description)
//             .map_err(|e| e.to_string())?;
//         self.data_description.clear();
//         self.module.finalize_definitions().unwrap();
//         let buffer = self.module.get_finalized_data(id);
//         // TODO: Can we move the unsafe into cranelift?
//         Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
//     }

//     // Translate from toy-language AST nodes into Cranelift IR.
//     fn translate(
//         &mut self,
//         params: Vec<String>,
//         the_return: String,
//         stmts: Vec<Expr>,
//     ) -> Result<(), String> {
//         // Our toy language currently only supports I64 values, though Cranelift
//         // supports other types.
//         let int = self.module.target_config().pointer_type();

//         for _p in &params {
//             self.ctx.func.signature.params.push(AbiParam::new(int));
//         }

//         // Our toy language currently only supports one return value, though
//         // Cranelift is designed to support more.
//         self.ctx.func.signature.returns.push(AbiParam::new(int));

//         // Create the builder to build a function.
//         let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

//         // Create the entry block, to start emitting code in.
//         let entry_block = builder.create_block();

//         // Since this is the entry block, add block parameters corresponding to
//         // the function's parameters.
//         //
//         // TODO: Streamline the API here.
//         builder.append_block_params_for_function_params(entry_block);

//         // Tell the builder to emit code in this block.
//         builder.switch_to_block(entry_block);

//         // And, tell the builder that this block will have no further
//         // predecessors. Since it's the entry block, it won't have any
//         // predecessors.
//         builder.seal_block(entry_block);

//         // The toy language allows variables to be declared implicitly.
//         // Walk the AST and declare all implicitly-declared variables.
//         let variables =
//             declare_variables(int, &mut builder, &params, &the_return, &stmts, entry_block);

//         // Now translate the statements of the function body.
//         let mut trans = FunctionTranslator {
//             int,
//             builder,
//             variables,
//             module: &mut self.module,
//         };
//         for expr in stmts {
//             trans.translate_expr(expr);
//         }

//         // Set up the return variable of the function. Above, we declared a
//         // variable to hold the return value. Here, we just do a use of that
//         // variable.
//         let return_variable = trans.variables.get(&the_return).unwrap();
//         let return_value = trans.builder.use_var(*return_variable);

//         // Emit the return instruction.
//         trans.builder.ins().return_(&[return_value]);

//         // Tell the builder we're done with this function.
//         trans.builder.finalize();
//         Ok(())
//     }
// }
