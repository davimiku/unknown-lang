use std::error::Error;

use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::verifier::verify_function;
use cranelift::codegen::{settings, write_function};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::Configurable;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use util_macros::assert_matches;

use crate::ext::jit_builder::JITBuilderExt;

#[test]
fn multiple_returns() -> Result<(), Box<dyn Error>> {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder()?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    let mut module = JITModule::new(jit_builder);
    let mut ctx = module.make_context();

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut sig = Signature::new(CallConv::AppleAarch64);
    sig.params.push(AbiParam::new(F64));
    sig.returns.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(F64));
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    let func_id = module
        .declare_function("test", Linkage::Export, &func.signature)
        .unwrap();

    let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

    {
        let block0_entry = builder.create_block();
        builder.append_block_params_for_function_params(block0_entry);
        builder.switch_to_block(block0_entry);
        builder.seal_block(block0_entry);

        let return_var = builder.declare_var(F64);
        let first_var = builder.declare_var(I64);
        let second_var = builder.declare_var(I64);

        let first_val = builder.ins().iconst(I64, 20);
        builder.def_var(first_var, first_val);
        let second_val = builder.ins().iconst(I64, 30);
        builder.def_var(second_var, second_val);
        let return_val = builder.block_params(block0_entry)[0];
        builder.def_var(return_var, return_val);

        let first_val = builder.use_var(first_var);
        let second_val = builder.use_var(second_var);
        let return_val = builder.use_var(return_var);
        builder.ins().return_(&[first_val, second_val, return_val]);
    }

    builder.finalize();
    // let flags = settings::Flags::new(settings::builder());
    // verify_function(&func, &flags)?;
    // println!("{}", func.display());

    module.define_function(func_id, &mut ctx)?;

    {
        let mut s = String::new();
        write_function(&mut s, &func).unwrap_or_else(|err| {
            dbg!(err);
        });
        println!("{s}");
    }
    module.finalize_definitions()?;
    let code_ptr = module.get_finalized_function(func_id);

    let fn_ptr = unsafe { std::mem::transmute::<*const u8, fn(f64) -> (i64, i64, f64)>(code_ptr) };

    let result = fn_ptr(1.23);

    Ok(())
}

#[test]
fn from_text_format() -> Result<(), Box<dyn Error>> {
    let parse_result = cranelift_reader::parse_functions(
        "function u1:0(f64) -> i64, i64, f64 apple_aarch64 {
block0(v0: f64):
    v1 = iconst.i64 1
    v2 = iconst.i64 0
    return v1, v2, v0  ; v1 = 1, v2 = 0
}",
    );

    // self.module.define_function(func_id, &mut self.ctx)?;
    // let mut jit = JIT::with_builtins();
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap_or_else(|error| panic!("ISA error: {error}"));
    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    let mut module = JITModule::new(jit_builder);
    let mut ctx = module.make_context();

    let mut functions = assert_matches!(parse_result, Result::Ok);
    let mut func = functions.pop().unwrap();
    let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
    builder.finalize();
    let res = verify_function(&func, &settings::Flags::new(settings::builder()));
    println!("{}", func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    let func_id = module
        .declare_function("test", Linkage::Export, &func.signature)
        .unwrap();

    module.define_function(func_id, &mut ctx)?;

    {
        let mut s = String::new();
        write_function(&mut s, &func).unwrap_or_else(|err| {
            dbg!(err);
        });
        println!("{s}");
    }
    module.finalize_definitions()?;
    let code_ptr = module.get_finalized_function(func_id);

    let fn_ptr = unsafe { std::mem::transmute::<*const u8, fn(f64) -> (i64, i64, f64)>(code_ptr) };

    let result = fn_ptr(1.23);
    println!("{result:?}");
    Ok(())
}

#[test]
fn test_branch_block_param() {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(I64));

    let mut ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    let mut builder = FunctionBuilder::new(&mut func, &mut ctx);

    {
        let block0_entry = builder.create_block();
        let block1_then = builder.create_block();
        let block2_else = builder.create_block();
        let block3_join = builder.create_block();

        let return_var = builder.declare_var(I64);
        let condition = builder.declare_var(I64);

        builder.append_block_params_for_function_params(block0_entry);

        builder.switch_to_block(block0_entry);
        builder.seal_block(block0_entry);

        builder.def_var(condition, builder.block_params(block0_entry)[0]);

        let condition_val = builder.use_var(condition);
        builder
            .ins()
            .brif(condition_val, block1_then, &[], block2_else, &[]);

        builder.switch_to_block(block1_then);
        builder.seal_block(block1_then);
        let val = builder.ins().iconst(I64, 16);
        builder.def_var(return_var, val);
        let block_arg = builder.use_var(return_var);
        builder.ins().jump(block3_join, &[]);

        builder.switch_to_block(block2_else);
        builder.seal_block(block2_else);
        let val = builder.ins().iconst(I64, 8);
        builder.def_var(return_var, val);
        let block_arg = builder.use_var(return_var);
        builder.ins().jump(block3_join, &[]);

        builder.switch_to_block(block3_join);
        builder.seal_block(block3_join);

        let return_val = builder.use_var(return_var);
        builder.ins().return_(&[return_val]);
    }

    builder.finalize();
    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    println!("{}", func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }
}

fn from_documentation() {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(I32));
    sig.params.push(AbiParam::new(I32));
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    {
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        let block0 = builder.create_block();
        let block1 = builder.create_block();
        let block2 = builder.create_block();
        let block3 = builder.create_block();
        let x = builder.declare_var(I32);
        let y = builder.declare_var(I32);
        let z = builder.declare_var(I32);
        builder.append_block_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.block_params(block0)[0]; // the first function parameter
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.ins().iconst(I32, 2);
            builder.def_var(y, tmp);
        }
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        builder.ins().jump(block1, &[]);

        builder.switch_to_block(block1);
        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(z);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().brif(arg, block3, &[], block2, &[]);
        }

        builder.switch_to_block(block2);
        builder.seal_block(block2);
        {
            let arg1 = builder.use_var(z);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().return_(&[arg]);
        }

        builder.switch_to_block(block3);
        builder.seal_block(block3);

        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(y, tmp);
        }
        builder.ins().jump(block1, &[]);
        builder.seal_block(block1);

        builder.finalize();
    }

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    println!("{}", func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }
}
