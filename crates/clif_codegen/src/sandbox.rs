use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

fn before(sig: Signature) -> (FunctionBuilderContext, Function) {
    let ctx = FunctionBuilderContext::new();
    let func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    (ctx, func)
}

// TODO: make this a macro
fn after(builder: FunctionBuilder, func: &Function) {
    builder.finalize();
    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    println!("{}", func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }
}

#[test]
fn test_branch_block_param() {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(I64));
    sig.params.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(I64));

    let (mut ctx, mut func) = before(sig);
    let mut builder = FunctionBuilder::new(&mut func, &mut ctx);

    {
        let block0_entry = builder.create_block();
        let block1_then = builder.create_block();
        let block2_else = builder.create_block();
        let block3_join = builder.create_block();

        let return_var = Variable::new(0);
        let condition = Variable::new(1);
        let int_from_params = Variable::new(2);
        let int_from_conditional = Variable::new(3);

        builder.declare_var(return_var, I64);
        builder.declare_var(condition, I64);
        builder.declare_var(int_from_params, I64);
        builder.declare_var(int_from_conditional, I64);

        builder.append_block_params_for_function_params(block0_entry);

        builder.switch_to_block(block0_entry);
        builder.seal_block(block0_entry);

        builder.def_var(condition, builder.block_params(block0_entry)[0]);
        builder.def_var(int_from_params, builder.block_params(block0_entry)[1]);

        let condition_val = builder.use_var(condition);
        builder
            .ins()
            .brif(condition_val, block1_then, &[], block2_else, &[]);

        builder.switch_to_block(block1_then);
        // builder.seal_block(block1_then);
        let val = builder.ins().iconst(I64, 11);
        builder.def_var(int_from_conditional, val);
        builder.ins().jump(block3_join, &[]);

        builder.switch_to_block(block2_else);
        // builder.seal_block(block2_else);
        let val = builder.ins().iconst(I64, 22);
        builder.def_var(int_from_conditional, val);
        builder.ins().jump(block3_join, &[]);

        builder.switch_to_block(block3_join);
        // builder.seal_block(block3_join);
        let lhs = builder.use_var(int_from_params);
        let rhs = builder.use_var(int_from_conditional);
        let return_val = builder.ins().iadd(lhs, rhs);
        builder.def_var(return_var, return_val);

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
        let x = Variable::new(0);
        let y = Variable::new(1);
        let z = Variable::new(2);
        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.declare_var(z, I32);
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
