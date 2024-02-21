//! Translation of the mid-level intermediate representation (MIR) to the
//! Cranelift intermediate format (CLIF).
//!
//! https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
//!
//! Uses the cranelift_frontend Builder and Context to walk the control flow graph of
//! the MIR and translate it into the CLIF instructions. This is a "translation" rather
//! than a lowering because it's between two representations at roughly the same level of
//! abstraction.
//!

use std::{collections::HashMap, ops::Deref};

use cranelift::codegen::ir::UserFuncName;
use cranelift::prelude::Block as ClifBlock;
use cranelift::prelude::Type as ClifType;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use hir::Type as HType;
use la_arena::Idx;
use mir::Terminator;
use mir::{BasicBlock, BinOp, Constant, Local, Operand, Rvalue, Statement};

// TODO: decide on nomenclature for "Type"
// currently "ClifType" means "Cranelift Type" and
// "HType" means "HIR Type"

pub(crate) struct CommonTypes {
    int: ClifType,
    float: ClifType,
    bool: ClifType,
    ptr: ClifType,
}

impl Default for CommonTypes {
    fn default() -> Self {
        Self {
            int: types::I64,
            float: types::F64,
            bool: types::I64,
            ptr: types::R64,
        }
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub(crate) struct FunctionTranslator<'a> {
    /// Convenience shortcut for common types
    pub(crate) types: CommonTypes,

    pub(crate) func: &'a mir::Function,

    /// The builder for the function that is currently being constructed
    builder: &'a mut FunctionBuilder<'a>,

    /// Reference to the module that this function is being constructed inside of
    module: &'a mut JITModule,

    /// Lowered and typechecked HIR context
    context: &'a hir::Context,

    /// map from *our* block to *CLIF* block
    /// TODO: swap HashMap for FxMap or similar, doesn't need the security of the default hasher
    block_map: HashMap<Idx<BasicBlock>, ClifBlock>,

    /// Blocks that are yet to be translated
    block_queue: Vec<Idx<BasicBlock>>,

    /// TODO: swap HashMap for FxMap or similar, doesn't need the security of the default hasher
    variables: HashMap<Idx<Local>, Variable>,
}

impl<'a> FunctionTranslator<'a> {
    pub(crate) fn new(
        builder: &'a mut FunctionBuilder<'a>,
        module: &'a mut JITModule,
        context: &'a hir::Context,
        func: &'a mir::Function,
    ) -> Self {
        Self {
            types: CommonTypes::default(),
            func,
            builder,
            module,
            context,
            block_map: HashMap::default(),
            block_queue: Vec::default(),
            variables: HashMap::default(),
        }
    }
}

impl<'a> FunctionTranslator<'a> {
    pub(crate) fn translate_function(&mut self) {
        let mir::Function {
            name: _,
            blocks,
            params,
            locals,
        } = self.func;
        dbg!(params);
        dbg!(locals);

        self.translate_signature();

        // create all blocks (empty/default)
        for (idx, _) in blocks.iter() {
            let block = self.builder.create_block();
            self.block_map.insert(idx, block);
        }

        // declare all locals with type. value is defined later with def_var
        // params will def_var shortly, other locals are def_var when the block is translated
        for (idx, local) in locals.iter() {
            let var_index = idx.into_raw().into_u32() as usize;
            let var = Variable::new(var_index);
            let var_ty = self.translate_type(local.type_(self.context));
            self.builder.declare_var(var, var_ty);
            self.variables.insert(idx, var);
        }

        let first_basic_block = self.func.entry_block();
        let entry_block = self.block_map[&first_basic_block];
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        self.builder.seal_block(entry_block);

        // define values for parameter locals: _1, _2, _3, ..., _n
        let param_locals = self
            .func
            .locals
            .iter()
            .skip(1)
            .take(params.len())
            .enumerate();
        for (i, (local_idx, _)) in param_locals {
            let param_val = self.builder.block_params(entry_block)[i];
            let var = self.variables[&local_idx];
            self.builder.def_var(var, param_val);
        }

        self.translate_basic_block(&self.func.blocks[first_basic_block]);

        // translate the rest of the blocks after the entry block
        for (idx, block) in self.block_map.iter().skip(1) {}

        // return local: _0
        // self.emit_return();
    }

    fn translate_signature(&mut self) {
        if let Some((_, symbol)) = self.func.name {
            let (module_id, symbol_id) = symbol.into();
            self.builder.func.name = UserFuncName::user(module_id, symbol_id);
        }

        // TODO: this level of abstraction may not work once we have compound types (unions, records)
        // how do those types get translated for a signature?
        for ty in &self.func.params {
            let clif_ty = self.translate_type_idx(*ty);
            self.builder
                .func
                .signature
                .params
                .push(AbiParam::new(clif_ty));
        }

        let return_ty = self.context.type_(self.func.return_ty());
        let return_ty = self.translate_type(return_ty);
        self.builder
            .func
            .signature
            .returns
            .push(AbiParam::new(return_ty));
    }

    fn translate_basic_block(&mut self, block: &BasicBlock) {
        for statement in &block.statements {
            self.translate_statement(statement);
        }
        self.translate_terminator(&block.terminator);
    }

    fn translate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Assign(assign) => {
                let (place, rvalue) = assign.deref();

                let val = self.translate_rvalue(rvalue);
                let var = self.variables[&place.local];
                self.builder.def_var(var, val);
            }
            Statement::SetDiscriminant {
                place,
                variant_index,
            } => todo!(),
            Statement::StorageLive(_) => todo!(),
            Statement::StorageDead(_) => todo!(),
            Statement::Intrinsic(_) => todo!(),
        }
    }

    fn translate_terminator(&mut self, terminator: &Terminator) {
        match terminator {
            Terminator::Goto { target } => todo!(),
            Terminator::Return => self.emit_return(),
            Terminator::Call {
                func,
                args,
                destination,
                target,
            } => todo!(),
            Terminator::Drop { place, target } => todo!(),
            Terminator::Unreachable => todo!(),
        }
    }

    fn emit_return(&mut self) {
        let (return_local, ..) = self.func.return_local();
        let ret_val = self.builder.use_var(self.variables[&return_local]);
        self.builder.ins().return_(&[ret_val]);
    }

    // fn translate_expr(&mut self, expr: Idx<Expr>) -> Value {
    //     match self.context.expr(expr) {
    //         Expr::Empty => todo!(), // self.builder.ins().nop(),

    //         Expr::BoolLiteral(b) => self.builder.ins().iconst(self.types.int, (*b) as i64),
    //         Expr::FloatLiteral(f) => self.builder.ins().f64const(*f),
    //         Expr::IntLiteral(i) => self.builder.ins().iconst(self.types.int, *i),
    //         Expr::StringLiteral(_) => todo!(),
    //         Expr::ArrayLiteral(_) => todo!(),

    //         Expr::Binary(expr) => self.translate_binary(expr),
    //         Expr::Unary(_) => todo!(),
    //         Expr::Block(expr) => self.translate_block(expr),
    //         Expr::Call(_) => todo!(),
    //         Expr::VarRef(_) => todo!(),
    //         Expr::UnresolvedVarRef { .. } => unreachable!(),
    //         Expr::Path(_) => todo!(),
    //         Expr::IndexInt(_) => todo!(),
    //         // will not need this hack after implementing MIR (control flow graph)
    //         Expr::Function(f) => unreachable!("function would have already been translated"),
    //         Expr::VarDef(_) => todo!(),
    //         Expr::If(_) => todo!(),
    //         Expr::Statement(expr) => self.translate_expr(*expr),
    //         Expr::ReturnStatement(_) => todo!(),
    //     }
    // }

    fn translate_rvalue(&mut self, rvalue: &Rvalue) -> Value {
        match rvalue {
            Rvalue::Use(op) => match op {
                Operand::Copy(p) => todo!(),
                Operand::Constant(c) => match c {
                    Constant::Int(i) => self.builder.ins().iconst(self.types.int, *i),
                    Constant::Float(f) => self.builder.ins().f64const(*f),
                    Constant::StringLiteral(_) => todo!(),
                },
                Operand::Move(p) => todo!(),
            },
            Rvalue::BinaryOp(binop, ops) => self.translate_binary_op(binop, ops.deref()),
            Rvalue::UnaryOp(unop, op) => todo!(),
        }
    }

    fn translate_binary_op(&mut self, binop: &BinOp, ops: &(Operand, Operand)) -> Value {
        let (lhs, rhs) = ops;
        // let lhs_type = self.context.
        match binop {
            BinOp::Add => todo!(),
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Rem => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Le => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Ge => todo!(),
            BinOp::Gt => todo!(),
        }
    }

    fn translate_type_idx(&mut self, idx: Idx<HType>) -> ClifType {
        let ty = self.context.type_(idx);
        self.translate_type(ty)
    }

    fn translate_type(&mut self, ty: &HType) -> ClifType {
        match ty {
            HType::BoolLiteral(_) | HType::Bool => self.types.bool,
            HType::FloatLiteral(_) | HType::Float => self.types.float,
            HType::IntLiteral(_) | HType::Int => self.types.int,
            HType::StringLiteral(_) | HType::String => todo!(),

            _ => todo!(),
        }
    }

    //     fn translate_block(&mut self, block: &hir::BlockExpr) -> Value {
    //         let hir::BlockExpr { exprs } = block;
    //         assert!(!exprs.is_empty());

    //         let values = exprs
    //             .iter()
    //             .map(|expr| self.translate_expr(*expr))
    //             .collect::<Vec<_>>();

    //         *values.last().unwrap()
    //     }

    //     fn translate_binary(&mut self, binary: &hir::BinaryExpr) -> Value {
    //         let hir::BinaryExpr { op, lhs, rhs } = binary;
    //         let lhs_type = self.context.borrow_expr_type(*lhs);
    //         let lhs = self.translate_expr(*lhs);

    //         let rhs_type = self.context.expr_type_idx(*rhs);
    //         let rhs = self.translate_expr(*rhs);

    //         match op {
    //             hir::BinaryOp::Add => match lhs_type {
    //                 hir::Type::FloatLiteral(_) | hir::Type::Float => self.builder.ins().fadd(lhs, rhs),
    //                 hir::Type::IntLiteral(_) | hir::Type::Int => self.builder.ins().iadd(lhs, rhs),
    //                 _ => unreachable!(),
    //             },
    //             hir::BinaryOp::Sub => todo!(),
    //             hir::BinaryOp::Mul => todo!(),
    //             hir::BinaryOp::Div => todo!(),
    //             hir::BinaryOp::Concat => todo!(),
    //             hir::BinaryOp::Rem => todo!(),
    //             hir::BinaryOp::Exp => todo!(),
    //             hir::BinaryOp::Path => todo!(),
    //             hir::BinaryOp::Eq => todo!(),
    //             hir::BinaryOp::Ne => todo!(),
    //         }
    //     }
}

//     /// When you write out instructions in Cranelift, you get back `Value`s. You
//     /// can then use these references in other instructions.
//     fn translate_expr(&mut self, expr: Expr) -> Value {
//         match expr {
//             Expr::Literal(literal) => {
//                 let imm: i32 = literal.parse().unwrap();
//                 self.builder.ins().iconst(self.int, i64::from(imm))
//             }

//             Expr::Add(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().iadd(lhs, rhs)
//             }

//             Expr::Sub(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().isub(lhs, rhs)
//             }

//             Expr::Mul(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().imul(lhs, rhs)
//             }

//             Expr::Div(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().udiv(lhs, rhs)
//             }

//             Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
//             Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
//             Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
//             Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
//             Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
//             Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
//             Expr::Call(name, args) => self.translate_call(name, args),
//             Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
//             Expr::Identifier(name) => {
//                 // `use_var` is used to read the value of a variable.
//                 let variable = self.variables.get(&name).expect("variable not defined");
//                 self.builder.use_var(*variable)
//             }
//             Expr::Assign(name, expr) => self.translate_assign(name, *expr),
//             Expr::IfElse(condition, then_body, else_body) => {
//                 self.translate_if_else(*condition, then_body, else_body)
//             }
//             Expr::WhileLoop(condition, loop_body) => {
//                 self.translate_while_loop(*condition, loop_body)
//             }
//         }
//     }

//     fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
//         // `def_var` is used to write the value of a variable. Note that
//         // variables can have multiple definitions. Cranelift will
//         // convert them into SSA form for itself automatically.
//         let new_value = self.translate_expr(expr);
//         let variable = self.variables.get(&name).unwrap();
//         self.builder.def_var(*variable, new_value);
//         new_value
//     }

//     fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
//         let lhs = self.translate_expr(lhs);
//         let rhs = self.translate_expr(rhs);
//         self.builder.ins().icmp(cmp, lhs, rhs)
//     }

//     fn translate_if_else(
//         &mut self,
//         condition: Expr,
//         then_body: Vec<Expr>,
//         else_body: Vec<Expr>,
//     ) -> Value {
//         let condition_value = self.translate_expr(condition);

//         let then_block = self.builder.create_block();
//         let else_block = self.builder.create_block();
//         let merge_block = self.builder.create_block();

//         // If-else constructs in the toy language have a return value.
//         // In traditional SSA form, this would produce a PHI between
//         // the then and else bodies. Cranelift uses block parameters,
//         // so set up a parameter in the merge block, and we'll pass
//         // the return values to it from the branches.
//         self.builder.append_block_param(merge_block, self.int);

//         // Test the if condition and conditionally branch.
//         self.builder
//             .ins()
//             .brif(condition_value, then_block, &[], else_block, &[]);

//         self.builder.switch_to_block(then_block);
//         self.builder.seal_block(then_block);
//         let mut then_return = self.builder.ins().iconst(self.int, 0);
//         for expr in then_body {
//             then_return = self.translate_expr(expr);
//         }

//         // Jump to the merge block, passing it the block return value.
//         self.builder.ins().jump(merge_block, &[then_return]);

//         self.builder.switch_to_block(else_block);
//         self.builder.seal_block(else_block);
//         let mut else_return = self.builder.ins().iconst(self.int, 0);
//         for expr in else_body {
//             else_return = self.translate_expr(expr);
//         }

//         // Jump to the merge block, passing it the block return value.
//         self.builder.ins().jump(merge_block, &[else_return]);

//         // Switch to the merge block for subsequent statements.
//         self.builder.switch_to_block(merge_block);

//         // We've now seen all the predecessors of the merge block.
//         self.builder.seal_block(merge_block);

//         // Read the value of the if-else by reading the merge block
//         // parameter.
//         let phi = self.builder.block_params(merge_block)[0];

//         phi
//     }

//     fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
//         let header_block = self.builder.create_block();
//         let body_block = self.builder.create_block();
//         let exit_block = self.builder.create_block();

//         self.builder.ins().jump(header_block, &[]);
//         self.builder.switch_to_block(header_block);

//         let condition_value = self.translate_expr(condition);
//         self.builder
//             .ins()
//             .brif(condition_value, body_block, &[], exit_block, &[]);

//         self.builder.switch_to_block(body_block);
//         self.builder.seal_block(body_block);

//         for expr in loop_body {
//             self.translate_expr(expr);
//         }
//         self.builder.ins().jump(header_block, &[]);

//         self.builder.switch_to_block(exit_block);

//         // We've reached the bottom of the loop, so there will be no
//         // more backedges to the header to exits to the bottom.
//         self.builder.seal_block(header_block);
//         self.builder.seal_block(exit_block);

//         // Just return 0 for now.
//         self.builder.ins().iconst(self.int, 0)
//     }

//     fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
//         let mut sig = self.module.make_signature();

//         // Add a parameter for each argument.
//         for _arg in &args {
//             sig.params.push(AbiParam::new(self.int));
//         }

//         // For simplicity for now, just make all calls return a single I64.
//         sig.returns.push(AbiParam::new(self.int));

//         // TODO: Streamline the API here?
//         let callee = self
//             .module
//             .declare_function(&name, Linkage::Import, &sig)
//             .expect("problem declaring function");
//         let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

//         let mut arg_values = Vec::new();
//         for arg in args {
//             arg_values.push(self.translate_expr(arg))
//         }
//         let call = self.builder.ins().call(local_callee, &arg_values);
//         self.builder.inst_results(call)[0]
//     }

//     fn translate_global_data_addr(&mut self, name: String) -> Value {
//         let sym = self
//             .module
//             .declare_data(&name, Linkage::Export, true, false)
//             .expect("problem declaring data object");
//         let local_id = self.module.declare_data_in_func(sym, self.builder.func);

//         let pointer = self.module.target_config().pointer_type();
//         self.builder.ins().symbol_value(pointer, local_id)
//     }
// }

// fn declare_variables(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     params: &[String],
//     the_return: &str,
//     stmts: &[Expr],
//     entry_block: Block,
// ) -> HashMap<String, Variable> {
//     let mut variables = HashMap::new();
//     let mut index = 0;

//     for (i, name) in params.iter().enumerate() {
//         // TODO: cranelift_frontend should really have an API to make it easy to set
//         // up param variables.
//         let val = builder.block_params(entry_block)[i];
//         let var = declare_variable(int, builder, &mut variables, &mut index, name);
//         builder.def_var(var, val);
//     }
//     let zero = builder.ins().iconst(int, 0);
//     let return_variable = declare_variable(int, builder, &mut variables, &mut index, the_return);
//     builder.def_var(return_variable, zero);
//     for expr in stmts {
//         declare_variables_in_stmt(int, builder, &mut variables, &mut index, expr);
//     }

//     variables
// }

// /// Recursively descend through the AST, translating all implicit
// /// variable declarations.
// fn declare_variables_in_stmt(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     variables: &mut HashMap<String, Variable>,
//     index: &mut usize,
//     expr: &Expr,
// ) {
//     match *expr {
//         Expr::Assign(ref name, _) => {
//             declare_variable(int, builder, variables, index, name);
//         }
//         Expr::IfElse(ref _condition, ref then_body, ref else_body) => {
//             for stmt in then_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//             for stmt in else_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//         }
//         Expr::WhileLoop(ref _condition, ref loop_body) => {
//             for stmt in loop_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//         }
//         _ => (),
//     }
// }

// /// Declare a single variable declaration.
// fn declare_variable(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     variables: &mut HashMap<String, Variable>,
//     index: &mut usize,
//     name: &str,
// ) -> Variable {
//     let var = Variable::new(*index);
//     if !variables.contains_key(name) {
//         variables.insert(name.into(), var);
//         builder.declare_var(var, int);
//         *index += 1;
//     }
//     var
// }
