pub fn compile_adder_printer() -> Result<*const u8, String> {
    let mut jit = JIT::with_builtins();

    {
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut fn_builder_ctx);
        builder.func.signature.params.push(AbiParam::new(I64));
        builder.func.signature.params.push(AbiParam::new(I64));

        // let mut print_int_signature = Signature::new(jit.call_conv);
        // print_int_signature.params.push(AbiParam::new(I64));

        // let print_int_data = ExtFuncData {
        //     name: ExternalName::User(print_int_name_ref),
        //     signature: builder.import_signature(print_int_signature),
        //     colocated: false,
        // };
        // let print_int_ref = builder.import_function(print_int_data);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // x
        let x = Variable::new(0);
        builder.declare_var(x, I64);
        let x_val = builder.block_params(entry_block)[0];
        builder.def_var(x, x_val);

        // y
        let y = Variable::new(1);
        builder.declare_var(y, I64);
        let y_val = builder.block_params(entry_block)[1];
        builder.def_var(y, y_val);

        // z = x + y
        let z = Variable::new(2);
        builder.declare_var(z, I64);
        let lhs = builder.use_var(x);
        let rhs = builder.use_var(y);
        let z_val = builder.ins().iadd(lhs, rhs);
        builder.def_var(z, z_val);

        let print_int_id = jit.builtins[builtins::PRINT_INT];
        let local_print_int = jit.module.declare_func_in_func(print_int_id, builder.func);

        // print z
        builder.ins().call(local_print_int, &[z_val]);

        // return
        builder.ins().return_(&[]);
    }

    let adder_printer_id = jit
        .module
        .declare_function("adder_printer", Linkage::Export, &jit.ctx.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(adder_printer_id, &mut jit.ctx)
        .map_err(|e| {
            dbg!(&e);
            e.to_string()
        })?;

    jit.module.clear_context(&mut jit.ctx);

    jit.module.finalize_definitions().unwrap();

    let code = jit.module.get_finalized_function(adder_printer_id);

    Ok(code)
}
