//! Translation of the mid-level intermediate representation (MIR) to the
//! Cranelift intermediate format (CLIF).
//!
//! https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
//!
//! Uses the cranelift_frontend Builder and Context to walk the control flow graph of
//! the MIR and translate it into the CLIF instructions.

mod arithmetic;

use std::collections::HashMap;
use std::ops::Deref;

use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::ir::BlockArg;
use cranelift::codegen::ir::UserFuncName;
use cranelift::codegen::ir::{StackSlotData, StackSlotKind};
use cranelift::frontend::Switch;
use cranelift::prelude::types::{F64, I64};
use cranelift::prelude::Block as ClifBlock;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Module};
use hir::{Type as HType, VariantIdx};
use la_arena::Entry;
use la_arena::{ArenaMap, Idx};
use mir::{
    BinOpKind, BlockTarget, BranchIntTargets, Constant, Local, Operand, Place, Rvalue, Statement,
    Terminator,
};

use crate::ext::function_builder::FunctionBuilderExt;
use crate::layout::{BackendRepr, Layouts, Scalar};
use crate::place::Pointer;
use crate::place::{to_vec_values, CPlace, CValue};

const WORD_SIZE: u32 = 8;

type BlockMap = ArenaMap<Idx<mir::BasicBlock>, ClifBlock>;

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub(crate) struct FunctionTranslator<'a> {
    pub(crate) func: &'a mir::Function,

    /// The builder for the function that is currently being constructed
    pub(crate) builder: FunctionBuilder<'a>,

    /// Reference to the module that this function is being constructed inside of
    pub(crate) module: &'a mut JITModule,

    /// Lowered and typechecked HIR context
    pub(crate) context: &'a hir::Context,

    /// map from our MIR FuncId to CLIF FuncId
    pub(crate) func_map: &'a HashMap<mir::FuncId, FuncId>,

    pub(crate) places: ArenaMap<Idx<Local>, CPlace>,

    pub(crate) layouts: Layouts,

    pub(crate) statuses: TranslateStatus,

    pub(crate) next_var_idx: usize,

    /// Variable holding the sret (struct return) pointer, if the return type is stack-allocated
    pub(crate) sret_var: Option<Variable>,
}

impl<'a> FunctionTranslator<'a> {
    pub(crate) fn new(
        builder: FunctionBuilder<'a>,
        module: &'a mut JITModule,
        func: &'a mir::Function,
        func_map: &'a HashMap<mir::FuncId, FuncId>,
        context: &'a hir::Context,
    ) -> Self {
        Self {
            func,
            builder,
            module,
            context,
            func_map,
            places: Default::default(),
            layouts: Layouts::new(context.core_types()),
            statuses: Default::default(),
            next_var_idx: 0,
            sret_var: None,
        }
    }
}

impl FunctionTranslator<'_> {
    pub(crate) fn translate_function(mut self) {
        let mir::Function {
            blocks,
            locals,
            predecessors,
            ..
        } = self.func;

        self.translate_signature_abi_params();

        let mut block_map: BlockMap = ArenaMap::with_capacity(blocks.len());

        // create all reachable blocks (empty/default)
        for (idx, _) in blocks.iter() {
            if predecessors.has_predecessors(idx) {
                block_map.insert(idx, self.builder.create_block());
                self.statuses.insert(idx, Status::Empty);
            }
        }

        // declare all locals with type. value is defined later with def_var
        // params will def_var shortly, other locals are def_var when the block is translated
        for (local_idx, local) in locals.iter() {
            self.declare_local(local_idx, *local);
        }

        let entry_block = {
            let entry_block_idx = self.func.entry_block();
            let entry_block = block_map[entry_block_idx];
            self.builder
                .append_block_params_for_function_params(entry_block);
            self.builder.switch_to_block(entry_block);
            self.safe_seal_block(entry_block_idx, entry_block);
            entry_block
        };

        // def_var the values for parameter locals: _1, _2, _3, ..., _n
        let param_locals = self.func.param_locals();
        let mut i = 0;
        for (local_idx, _) in param_locals {
            match self.places[local_idx] {
                CPlace::Var { variable, .. } => {
                    let val = self.builder.block_params(entry_block)[i];
                    i += 1;
                    self.builder.def_var(variable, val);
                }
                CPlace::VarPair { first, second, .. } => {
                    let val = self.builder.block_params(entry_block)[i];
                    i += 1;
                    self.builder.def_var(first, val);

                    let val = self.builder.block_params(entry_block)[i];
                    i += 1;
                    self.builder.def_var(second, val);
                }
                CPlace::Address { pointer, layout } => {
                    // Stack-allocated values: copy from parameter to our stack slot
                    let (slot, offset) = match pointer {
                        Pointer::Stack { slot, offset } => (slot, offset),
                        Pointer::Heap { addr, offset } => {
                            panic!("Expected stack pointer for stack-allocated parameter")
                        }
                    };
                    let size = self.layouts[layout].size;
                    // The parameter is passed as a pointer - load it from the block params
                    let param_ptr = self.builder.block_params(entry_block)[i];
                    i += 1;
                    // Copy from the parameter's memory to our stack slot
                    let dest_addr = self.builder.ins().stack_addr(I64, slot, offset);
                    self.builder.emit_small_memory_copy(
                        self.module.target_config(),
                        dest_addr,
                        param_ptr,
                        size.bytes() as u64,
                        1,    // dest align
                        1,    // src align
                        true, // non-overlapping
                        MemFlags::new(),
                    );
                }
            }
        }

        // If return type is stack-allocated, capture the sret pointer in a variable
        // The sret pointer is passed as the last parameter
        {
            let return_ty = self.func.return_ty();
            let return_layout_idx = self.layout_for_type(return_ty);
            let return_layout = &self.layouts[return_layout_idx];
            if matches!(return_layout.backend_repr, BackendRepr::StackSlot) {
                let sret_var = self.builder.declare_var(I64);
                let sret_ptr = self.builder.block_params(entry_block)[i];
                self.builder.def_var(sret_var, sret_ptr);
                self.sret_var = Some(sret_var);
            }
        }

        // translate the entry block first (we're already switched to it)
        let entry_block_idx = self.func.entry_block();
        self.translate_basic_block(entry_block_idx, &block_map);
        self.statuses
            .entry(entry_block_idx)
            .and_modify(|entry| *entry = entry.finish());

        // translate the rest of the blocks after the entry block
        for (idx, block) in block_map.iter() {
            // Skip entry block since we already translated it above
            if idx == entry_block_idx {
                continue;
            }
            self.builder.switch_to_block(*block);
            // TODO - cranelift recommends sealing blocks as soon as possible
            // uncomment and run all tests after more of the language is implemented
            // self.seal_block(idx, *block);

            self.translate_basic_block(idx, &block_map);
            self.statuses
                .entry(idx)
                .and_modify(|entry| *entry = entry.finish());

            // this block's successors might be able to be sealed as well
            let successors = self.func.blocks[idx]
                .terminator
                .as_ref()
                .expect("terminator exists")
                .targets();
            for successor in successors {
                let successor_block = block_map[successor];
                // self.seal_block(successor, successor_block);
            }
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    fn declare_local(&mut self, local_idx: Idx<Local>, local: Local) {
        if self.places.contains_idx(local_idx) {
            panic!("already declared local {:?}, {:?}", local_idx, local);
        }

        let layout_idx = self.layout_for_type(local.type_idx());
        let layout = &self.layouts[layout_idx];
        match layout.backend_repr.clone() {
            BackendRepr::None => {}
            BackendRepr::Scalar(scalar) => {
                let var = self.builder.declare_var(scalar.into());
                let place = CPlace::Var {
                    local,
                    variable: var,
                    layout: layout_idx,
                };
                self.places.insert(local_idx, place);
            }
            BackendRepr::ScalarPair(first, second) => {
                self.next_var_idx += 1;
                let var_first = self.builder.declare_var(first.into());

                self.next_var_idx += 1;
                let var_second = self.builder.declare_var(second.into());

                let place = CPlace::VarPair {
                    local,
                    first: var_first,
                    second: var_second,
                    layout: layout_idx,
                };
                self.places.insert(local_idx, place);
            }
            BackendRepr::StackSlot => {
                // Create a stack slot for this local
                let size = layout.size.bytes() as u32;
                let slot_data = StackSlotData::new(StackSlotKind::ExplicitSlot, size, 3); // align 8
                let slot = self.builder.create_sized_stack_slot(slot_data);
                let place = CPlace::Address {
                    pointer: Pointer::Stack {
                        slot,
                        offset: 0.into(),
                    },
                    layout: layout_idx,
                };
                self.places.insert(local_idx, place);
            }
        }
    }

    fn translate_signature_abi_params(&mut self) {
        let (module_id, symbol_id) = self.func.id.into();
        self.builder.func.name = UserFuncName::user(module_id, symbol_id);

        for ty_idx in &self.func.params {
            let layout_idx = self.layout_for_type(*ty_idx);
            let layout = &self.layouts[layout_idx];
            match layout.backend_repr.clone() {
                BackendRepr::None => {}
                BackendRepr::Scalar(scalar) => {
                    let abi_param = self.translate_scalar_to_abi_param(scalar);
                    self.builder.func.signature.params.push(abi_param);
                }
                BackendRepr::ScalarPair(first, second) => {
                    let first_abi_param = self.translate_scalar_to_abi_param(first);
                    self.builder.func.signature.params.push(first_abi_param);

                    let second_abi_param = self.translate_scalar_to_abi_param(second);
                    self.builder.func.signature.params.push(second_abi_param);
                }
                BackendRepr::StackSlot => {
                    // Stack-allocated parameters are passed by pointer
                    // The caller passes a pointer to the data
                    // We use a regular i64 parameter instead of ArgumentPurpose::StructArgument
                    // to avoid ABI complications (StructArgument not supported on arm64)
                    let abi_param = AbiParam::new(I64);
                    self.builder.func.signature.params.push(abi_param);
                }
            }
        }

        {
            // return type
            let return_ty = self.func.return_ty();

            let layout_idx = self.layout_for_type(return_ty);
            let layout = &self.layouts[layout_idx];
            match layout.backend_repr.clone() {
                BackendRepr::None => {}
                BackendRepr::Scalar(scalar) => {
                    let abi_param = self.translate_scalar_to_abi_param(scalar);
                    self.builder.func.signature.returns.push(abi_param);
                }
                BackendRepr::ScalarPair(first, second) => {
                    let first_abi_param = self.translate_scalar_to_abi_param(first);
                    self.builder.func.signature.returns.push(first_abi_param);

                    let second_abi_param = self.translate_scalar_to_abi_param(second);
                    self.builder.func.signature.returns.push(second_abi_param);
                }
                BackendRepr::StackSlot => {
                    // Stack-allocated returns: caller provides a pointer where we write
                    // the return value. We use a regular i64 parameter instead of
                    // ArgumentPurpose::StructReturn to avoid ABI complications with
                    // different calling conventions.
                    let abi_param = AbiParam::new(I64);
                    self.builder.func.signature.params.push(abi_param);
                }
            }
        }
    }

    fn translate_scalar_to_abi_param(&self, scalar: Scalar) -> AbiParam {
        match scalar {
            Scalar::Int => AbiParam::new(I64),
            Scalar::Float => AbiParam::new(F64),
            Scalar::Pointer(Pointer::Heap { .. }) => AbiParam::new(I64),
            Scalar::Pointer(Pointer::Stack { .. }) => AbiParam::new(I64),
        }
    }

    fn translate_basic_block(&mut self, block_idx: Idx<mir::BasicBlock>, block_map: &BlockMap) {
        let block: &mir::BasicBlock = &self.func.blocks[block_idx];
        for statement in &block.statements {
            self.translate_statement(statement);
        }
        let terminator = block
            .terminator
            .as_ref()
            .expect("Compiler Error: Missing mir::BasicBlock::Terminator");
        self.translate_terminator(terminator, block_map);
    }

    fn translate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Assign(assign) | Statement::ReAssign(assign) => {
                let (place, rvalue) = assign.deref();

                self.translate_rvalue(place, rvalue);
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

    fn translate_terminator(&mut self, terminator: &Terminator, block_map: &BlockMap) {
        match terminator {
            Terminator::Jump(BlockTarget { target, .. }) => {
                self.translate_jump_terminator(block_map[*target])
            }

            Terminator::Return => self.translate_return_terminator(),
            Terminator::Call {
                func,
                args,
                destination,
                target,
            } => {
                let target = target
                    .as_ref()
                    .map(|block_target| (block_map[block_target.target], &block_target.args));
                self.translate_call_terminator(func, args, destination, target)
            }
            Terminator::BranchInt {
                discriminant,
                targets,
            } => self.translate_branch_terminator(discriminant, targets, block_map),
            Terminator::Drop { place, target } => todo!(),
            Terminator::Unreachable => todo!(),
        }
    }

    fn translate_jump_terminator(&mut self, target: ClifBlock) {
        self.builder.ins().jump(target, &[]);
    }

    fn translate_call_terminator(
        &mut self,
        func: &Operand,
        args: &[Operand],
        destination: &Place,
        target: Option<(ClifBlock, &Vec<Idx<Local>>)>,
    ) {
        let args: Vec<CValue> = args.iter().map(|arg| self.translate_operand(arg)).collect();
        let args = to_vec_values(args);

        let returns = match func {
            Operand::Constant(constant) => match constant {
                Constant::Func(func_id) => {
                    let func_id = self.func_map[func_id];
                    let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                    self.builder.ins().call(func_ref, &args)
                }

                _ => unreachable!(),
            },
            Operand::Copy(place) => todo!("func_addr and indirect call"),
            Operand::Move(place) => todo!("closures that capture resources might become Move?"),
        };
        let returns = self.builder.inst_results(returns);
        match returns.len() {
            0 => {}
            1 => {
                // FIXME: use projections
                let cplace = &self.places[destination.local];
                if let CPlace::Var {
                    local,
                    variable,
                    layout,
                } = cplace
                {
                    self.builder.def_var(*variable, returns[0]);
                };
            }
            2 => {}
            _ => unreachable!("3+ returns?"),
        }

        // FIXME: remove unwrap when panics are implemented
        let block_call_args: Vec<BlockArg> = self
            .locals_to_values(target.unwrap().1)
            .into_iter()
            .map(|value| BlockArg::Value(value))
            .collect();
        self.builder.ins().jump(target.unwrap().0, &block_call_args);
    }

    fn translate_branch_terminator(
        &mut self,
        discriminant: &Operand,
        targets: &BranchIntTargets,
        block_map: &BlockMap,
    ) {
        let mut switch = Switch::new();
        for (i, block_target) in targets.branches.iter() {
            let block = block_map[block_target.target];
            switch.set_entry((*i) as u128, block);
        }
        // TODO - for `match` with no 'otherwise', this creates a CLIF block not tracked in block_map
        // is that an issue?
        // Does this "dummy block" need a terminator?
        let mut dummy_block: Option<Block> = None;
        let otherwise = targets
            .otherwise
            .as_ref()
            .map(|b| block_map[b.target])
            .unwrap_or_else(|| *dummy_block.insert(self.builder.create_block()));

        let condition = self.translate_operand(discriminant);
        let condition = condition.as_val().expect("a single value");

        switch.emit(&mut self.builder, condition, otherwise);
        if let Some(dummy_block) = dummy_block {
            self.builder.switch_to_block(dummy_block);
            self.builder.ins().nop();
            // TODO - this is meant to be a "dummy" terminator, does this work?
            // Find out how to handle exhaustive Switch in CLIF
            self.translate_return_terminator();
            self.builder.seal_block(dummy_block);
        }
    }

    fn translate_return_terminator(&mut self) {
        let (return_local_idx, return_local) = self.func.return_local();
        if return_local.type_(self.context).is_unit() {
            self.builder.ins().return_(&[]);
        } else {
            match &self.places[return_local_idx] {
                CPlace::Var { variable, .. } => {
                    let val = self.builder.use_var(*variable);
                    self.builder.ins().return_(&[val]);
                }
                CPlace::VarPair { first, second, .. } => {
                    let first_val = self.builder.use_var(*first);
                    let second_val = self.builder.use_var(*second);
                    self.builder.ins().return_(&[first_val, second_val]);
                }
                CPlace::Address {
                    pointer: Pointer::Stack { slot, offset },
                    layout,
                } => {
                    // For stack-allocated returns, we need to copy our local stack slot
                    // to the sret pointer that was stored in sret_var at function entry
                    let sret_var = self
                        .sret_var
                        .expect("sret_var should be set for stack-allocated return types");
                    let sret_ptr = self.builder.use_var(sret_var);
                    let size = self.layouts[*layout].size;
                    let src_addr = self.builder.ins().stack_addr(I64, *slot, *offset);

                    self.builder.emit_small_memory_copy(
                        self.module.target_config(),
                        sret_ptr,
                        src_addr,
                        size.bytes() as u64,
                        1,    // dest align
                        1,    // src align
                        true, // non-overlapping
                        MemFlags::new(),
                    );
                    self.builder.ins().return_(&[]);
                }
                CPlace::Address {
                    pointer: Pointer::Heap { .. },
                    ..
                } => {
                    unreachable!("Heap-allocated return not yet supported");
                }
            }
        }
    }

    fn locals_to_values(&mut self, locals: &[Idx<Local>]) -> Vec<Value> {
        locals
            .iter()
            .flat_map(|local| self.places[*local].variables())
            .map(|var| self.builder.use_var(var))
            .collect()
    }

    fn translate_rvalue(&mut self, place: &Place, rvalue: &Rvalue) {
        let cval = match rvalue {
            Rvalue::Use(op) => self.translate_operand(op),
            Rvalue::BinaryOp(binop, ops) => self.translate_binary_op(binop, ops.deref()),
            Rvalue::UnaryOp(_unop, _op) => todo!(),
            Rvalue::Aggregate(aggregate) => todo!(),
            Rvalue::Discriminant(place) => self.translate_discriminant(place),
            Rvalue::UnionVariant(variant_idx, _, operand) => {
                self.translate_union_variant(*variant_idx, operand, place.type_idx_of(self.func))
            }
        };
        let cplace = self.places[place.local].clone();
        match (&cplace, &cval) {
            (CPlace::Var { variable, .. }, CValue::Val { val, .. }) => {
                self.builder.def_var(*variable, *val);
            }
            (
                CPlace::VarPair {
                    first: first_var,
                    second: second_var,
                    ..
                },
                CValue::ValPair {
                    first: first_val,
                    second: second_val,
                    ..
                },
            ) => {
                self.builder.def_var(*first_var, *first_val);
                self.builder.def_var(*second_var, *second_val);
            }
            (
                CPlace::Address {
                    pointer: Pointer::Stack { slot, offset },
                    layout,
                },
                CValue::Ref { ptr: src_ptr, .. },
            ) => {
                // Copy from source stack slot to destination stack slot
                let size = self.layouts[*layout].size;
                let dest_addr = self.builder.ins().stack_addr(I64, *slot, *offset);
                let src_addr = match src_ptr {
                    Pointer::Stack {
                        slot: src_slot,
                        offset: src_offset,
                    } => self.builder.ins().stack_addr(I64, *src_slot, *src_offset),
                    Pointer::Heap { addr, .. } => *addr,
                };
                self.builder.emit_small_memory_copy(
                    self.module.target_config(),
                    dest_addr,
                    src_addr,
                    size.bytes() as u64,
                    1,    // dest align
                    1,    // src align
                    true, // non-overlapping
                    MemFlags::new(),
                );
            }
            (
                CPlace::Address {
                    pointer: Pointer::Stack { slot, offset },
                    ..
                },
                CValue::Val { val, .. },
            ) => {
                // Store a scalar value into a stack slot (e.g., storing discriminant)
                self.builder.ins().stack_store(*val, *slot, *offset);
            }
            (
                CPlace::Address {
                    pointer: Pointer::Stack { slot, offset },
                    ..
                },
                CValue::ValPair { first, second, .. },
            ) => {
                // Store two values into stack slot (tag + payload)
                self.builder.ins().stack_store(*first, *slot, *offset);
                let second_offset: i32 = (*offset).into();
                let second_offset = Offset32::new(second_offset + WORD_SIZE as i32);
                self.builder
                    .ins()
                    .stack_store(*second, *slot, second_offset);
            }
            (
                CPlace::Var {
                    local: _,
                    variable,
                    layout: var_layout,
                },
                CValue::Ref {
                    ptr,
                    val: _,
                    layout: _,
                },
            ) => {
                // Load a scalar from memory (stack or heap) into a local variable.
                let var_layout = &self.layouts[*var_layout];
                let loaded_val = match &var_layout.backend_repr {
                    BackendRepr::Scalar(Scalar::Int) => match ptr {
                        Pointer::Stack { slot, offset } => {
                            self.builder.ins().stack_load(I64, *slot, *offset)
                        }
                        Pointer::Heap { addr, offset } => {
                            self.builder
                                .ins()
                                .load(I64, MemFlags::new(), *addr, *offset)
                        }
                    },
                    BackendRepr::Scalar(Scalar::Float) => match ptr {
                        Pointer::Stack { slot, offset } => {
                            self.builder.ins().stack_load(F64, *slot, *offset)
                        }
                        Pointer::Heap { addr, offset } => {
                            self.builder
                                .ins()
                                .load(F64, MemFlags::new(), *addr, *offset)
                        }
                    },
                    BackendRepr::Scalar(Scalar::Pointer(_)) => match ptr {
                        Pointer::Stack { slot, offset } => {
                            self.builder.ins().stack_load(I64, *slot, *offset)
                        }
                        Pointer::Heap { addr, offset } => {
                            self.builder
                                .ins()
                                .load(I64, MemFlags::new(), *addr, *offset)
                        }
                    },
                    other => {
                        unreachable!(
                            "Internal Compiler Error (CLIF): Cannot load Ref into Var for backend_repr {:?}",
                            other
                        )
                    }
                };

                self.builder.def_var(*variable, loaded_val);
            }
            (_, _) => unreachable!(
                "Internal Compiler Error (CLIF): Unexpected Place/Value combination: {:?}/{:?}",
                cplace, cval
            ),
        }
    }

    fn translate_binary_op(&mut self, binop: &BinOpKind, ops: &(Operand, Operand)) -> CValue {
        let (lhs, rhs) = ops;

        match binop {
            BinOpKind::Add => self.emit_add(lhs, rhs),
            BinOpKind::Sub => self.emit_sub(lhs, rhs),
            BinOpKind::Mul => self.emit_mul(lhs, rhs),
            BinOpKind::Div => self.emit_div(lhs, rhs),
            BinOpKind::Rem => self.emit_rem(lhs, rhs),
            BinOpKind::Concat => todo!(),
            BinOpKind::Eq => self.emit_comparison(lhs, rhs, Cmp::Equal),
            BinOpKind::Ne => self.emit_comparison(lhs, rhs, Cmp::NotEqual),
            BinOpKind::Lt => self.emit_comparison(lhs, rhs, Cmp::LessThan),
            BinOpKind::Le => self.emit_comparison(lhs, rhs, Cmp::LessThanOrEqual),
            BinOpKind::Gt => self.emit_comparison(lhs, rhs, Cmp::GreaterThan),
            BinOpKind::Ge => self.emit_comparison(lhs, rhs, Cmp::GreaterThanOrEqual),
        }
    }

    fn emit_comparison(&mut self, lhs: &Operand, rhs: &Operand, comparison: Cmp) -> CValue {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);

        let lhs_val = self.translate_operand(lhs);
        let lhs_val = lhs_val.as_val().expect("to be a single value");
        let rhs_val = self.translate_operand(rhs);
        let rhs_val = rhs_val.as_val().expect("to be a single value");

        let val = if self.context.type_(lhs_ty).is_float() {
            self.builder
                .ins()
                .fcmp(comparison.as_float_cc(), lhs_val, rhs_val)
        } else if self.context.type_(lhs_ty).is_int() {
            self.emit_int_comparison(lhs_val, rhs_val, comparison.as_int_cc())
        } else {
            unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for comparison")
        };
        let layout = self.layout_for_type(self.context.core_types().bool);
        CValue::Val { val, layout }
    }

    fn emit_int_comparison(&mut self, lhs_val: Value, rhs_val: Value, comparison: IntCC) -> Value {
        let val_i8 = self.builder.ins().icmp(comparison, lhs_val, rhs_val);
        self.builder.ins().sextend(I64, val_i8)
    }

    fn emit_int_constant_comparison(&mut self, lhs: i64, rhs: i64, comparison: IntCC) -> Value {
        self.builder.bool_const(match comparison {
            IntCC::Equal => lhs == rhs,
            IntCC::NotEqual => lhs != rhs,
            IntCC::SignedLessThan => lhs < rhs,
            IntCC::SignedLessThanOrEqual => lhs <= rhs,
            IntCC::SignedGreaterThan => lhs > rhs,
            IntCC::SignedGreaterThanOrEqual => lhs >= rhs,
            _ => unreachable!(),
        })
    }

    fn translate_operand(&mut self, op: &Operand) -> CValue {
        match op {
            Operand::Copy(place) => {
                if !place.projections.is_empty() {
                    let mut ptr: Option<Pointer> = None;

                    for proj in place.projections.iter() {
                        match proj {
                            mir::ProjectionElem::DowncastVariant(_key, _idx) => {
                                // Currently, all non-unit unions are represented as:
                                // tag (i64) at offset 0 and payload at offset WORD_SIZE in a stack slot.
                                // The local for the union should already be a CPlace::Address::Stack
                                // pointing at the base of that slot. We adjust the offset by WORD_SIZE
                                // to point at the payload.
                                let base_place = &self.places[place.local];
                                match base_place {
                                    CPlace::Address {
                                        pointer: Pointer::Stack { slot, offset },
                                        ..
                                    } => {
                                        let base_offset: i32 = (*offset).into();
                                        let payload_offset =
                                            Offset32::new(base_offset + WORD_SIZE as i32);
                                        ptr = Some(Pointer::Stack {
                                            slot: *slot,
                                            offset: payload_offset,
                                        });
                                    }
                                    CPlace::Address {
                                        pointer: Pointer::Heap { addr, offset },
                                        ..
                                    } => {
                                        let base_offset: i32 = (*offset).into();
                                        let payload_offset =
                                            Offset32::new(base_offset + WORD_SIZE as i32);
                                        ptr = Some(Pointer::Heap {
                                            addr: *addr,
                                            offset: payload_offset,
                                        });
                                    }
                                    _ => {
                                        // DowncastVariant on a non-address place is unexpected with current layouts.
                                        unreachable!(
                                            "Internal Compiler Error (CLIF): DowncastVariant on non-address place: {:?}",
                                            base_place
                                        );
                                    }
                                }
                            }
                            // Other projections (Field, Index, etc.) are not implemented yet.
                            _ => {
                                todo!(
                                    "Projection {:?} not yet implemented in translate_operand",
                                    proj
                                );
                            }
                        }
                    }

                    if let Some(ptr) = ptr {
                        // The layout for the bound variable will be determined at assignment time.
                        // Here we just return a reference to the payload.
                        let layout_idx = self.layout_for_type(place.type_idx_of(self.func));
                        return CValue::Ref {
                            ptr,
                            val: None,
                            layout: layout_idx,
                        };
                    }
                }

                // no projections - use the place directly
                let cplace = &self.places[place.local];
                match cplace {
                    CPlace::Var {
                        variable, layout, ..
                    } => CValue::Val {
                        val: self.builder.use_var(*variable),
                        layout: *layout,
                    },
                    CPlace::VarPair {
                        first,
                        second,
                        layout,
                        ..
                    } => CValue::ValPair {
                        first: self.builder.use_var(*first),
                        second: self.builder.use_var(*second),
                        layout: *layout,
                    },
                    CPlace::Address { pointer, layout } => CValue::Ref {
                        ptr: *pointer,
                        val: None,
                        layout: *layout,
                    },
                }
            }
            Operand::Constant(c) => match c {
                Constant::Int(i) => CValue::Val {
                    val: self.builder.ins().iconst(I64, *i),
                    layout: self.layout_for_type(self.context.core_types().int),
                },
                Constant::Float(f) => CValue::Val {
                    val: self.builder.ins().f64const(*f),
                    layout: self.layout_for_type(self.context.core_types().float),
                },
                Constant::String(_) => todo!(),
                Constant::Func(..) => unreachable!("TODO"),
            },
            Operand::Move(_) => todo!(),
        }
    }

    fn translate_discriminant(&mut self, place: &Place) -> CValue {
        let cplace = &self.places[place.local];
        match cplace {
            // single var means it's a unit sum type, so the variable just is the discriminant
            CPlace::Var { variable, .. } => CValue::Val {
                val: self.builder.use_var(*variable),
                layout: self.layouts.int,
            },
            // pair/triple means there's at least one variant with data. The first Variable is the discriminant
            CPlace::VarPair { first, .. } => CValue::Val {
                val: self.builder.use_var(*first),
                layout: self.layouts.int,
            },
            CPlace::Address {
                pointer: Pointer::Stack { slot, offset },
                ..
            } => {
                // Load the discriminant (first word) from the stack slot
                let val = self.builder.ins().stack_load(I64, *slot, *offset);
                CValue::Val {
                    val,
                    layout: self.layouts.int,
                }
            }
            CPlace::Address {
                pointer: Pointer::Heap { addr, offset },
                ..
            } => {
                // Load the discriminant from a heap address
                let val = self
                    .builder
                    .ins()
                    .load(I64, MemFlags::new(), *addr, *offset);
                CValue::Val {
                    val,
                    layout: self.layouts.int,
                }
            }
        }
    }

    fn translate_union_variant(
        &mut self,
        variant_idx: VariantIdx,
        operand: &Operand,
        ty: Idx<HType>,
    ) -> CValue {
        // operand is the "arg", like the 16 in `Number.int 16` or the `f` in `Number.float f`
        // variant_idx is the numeric index

        let layout_idx = self.layout_for_type(ty);
        let layout = self.layouts.get_cached(ty).unwrap().clone();

        let discriminant = self
            .builder
            .ins()
            .iconst(I64, variant_idx.into_raw() as i64);

        match &layout.backend_repr {
            BackendRepr::None => unreachable!("Union cannot have BackendRepr::None"),

            // unit union - the discriminant is the value
            BackendRepr::Scalar(Scalar::Int) => CValue::Val {
                val: discriminant,
                layout: layout_idx,
            },

            BackendRepr::Scalar(_) => unreachable!("Union tag must be Int scalar"),

            // ScalarPair is no longer used for unions, but keep for compatibility
            BackendRepr::ScalarPair(_, _) => {
                unreachable!("ScalarPair no longer used for unions - use StackSlot instead")
            }

            BackendRepr::StackSlot => {
                // For stack-allocated unions, we need to:
                // 1. Create a temporary stack slot to hold the result
                // 2. Write the discriminant at offset 0
                // 3. Write the payload at offset WORD_SIZE
                // 4. Return a Ref to this stack slot

                let size = layout.size.bytes() as u32;
                let slot_data = StackSlotData::new(StackSlotKind::ExplicitSlot, size, 3); // align 8
                let slot = self.builder.create_sized_stack_slot(slot_data);

                // Write discriminant at offset 0
                self.builder
                    .ins()
                    .stack_store(discriminant, slot, Offset32::new(0));

                // Translate and write the payload at offset WORD_SIZE
                let payload_cval = self.translate_operand(operand);
                let payload_offset = Offset32::new(WORD_SIZE as i32);

                match payload_cval {
                    CValue::Val { val, .. } => {
                        self.builder.ins().stack_store(val, slot, payload_offset);
                    }
                    CValue::ValPair { first, second, .. } => {
                        self.builder.ins().stack_store(first, slot, payload_offset);
                        let second_offset = Offset32::new((WORD_SIZE * 2) as i32);
                        self.builder.ins().stack_store(second, slot, second_offset);
                    }
                    CValue::ValTriple {
                        first,
                        second,
                        third,
                        ..
                    } => {
                        self.builder.ins().stack_store(first, slot, payload_offset);
                        let second_offset = Offset32::new((WORD_SIZE * 2) as i32);
                        self.builder.ins().stack_store(second, slot, second_offset);
                        let third_offset = Offset32::new((WORD_SIZE * 3) as i32);
                        self.builder.ins().stack_store(third, slot, third_offset);
                    }
                    CValue::Ref {
                        ptr:
                            Pointer::Stack {
                                slot: src_slot,
                                offset: src_offset,
                            },
                        layout: payload_layout,
                        ..
                    } => {
                        // Copy from source stack slot to our payload area
                        let payload_size = self.layouts[payload_layout].size;
                        let dest_addr = self.builder.ins().stack_addr(I64, slot, payload_offset);
                        let src_addr = self.builder.ins().stack_addr(I64, src_slot, src_offset);
                        self.builder.emit_small_memory_copy(
                            self.module.target_config(),
                            dest_addr,
                            src_addr,
                            payload_size.bytes() as u64,
                            1,    // dest align
                            1,    // src align
                            true, // non-overlapping
                            MemFlags::new(),
                        );
                    }
                    CValue::Ref {
                        ptr:
                            Pointer::Heap {
                                addr,
                                offset: src_offset,
                            },
                        layout: payload_layout,
                        ..
                    } => {
                        // Copy from heap address to our payload area
                        let payload_size = self.layouts[payload_layout].size;
                        let dest_addr = self.builder.ins().stack_addr(I64, slot, payload_offset);
                        let src_offset_i32: i32 = src_offset.into();
                        let src_addr = if src_offset_i32 == 0 {
                            addr
                        } else {
                            self.builder.ins().iadd_imm(addr, src_offset_i32 as i64)
                        };
                        self.builder.emit_small_memory_copy(
                            self.module.target_config(),
                            dest_addr,
                            src_addr,
                            payload_size.bytes() as u64,
                            1,    // dest align
                            1,    // src align
                            true, // non-overlapping
                            MemFlags::new(),
                        );
                    }
                }

                // Return a reference to our stack slot
                CValue::Ref {
                    ptr: Pointer::Stack {
                        slot,
                        offset: Offset32::new(0),
                    },
                    val: None,
                    layout: layout_idx,
                }
            }
        }
    }

    fn op_type(&self, op: &Operand) -> Idx<HType> {
        let core_types = self.context.core_types();
        let ty_idx = match op {
            Operand::Copy(place) => self.place_type_idx(place),
            Operand::Constant(constant) => self.constant_type_idx(constant),
            Operand::Move(_) => todo!(),
        };
        // widen literal types -- TODO should add a helper in HIR crate?
        match self.context.type_(ty_idx) {
            HType::FloatLiteral(_) => core_types.float,
            HType::IntLiteral(_) => core_types.int,
            HType::StringLiteral(_) => core_types.string,
            _ => ty_idx,
        }
    }

    fn place_type<'b>(&self, place: &Place, context: &'b hir::Context) -> &'b HType {
        let local = &self.func.locals[place.local];
        local.type_(context)
    }

    fn place_type_idx(&self, place: &Place) -> Idx<HType> {
        // TODO: use projections...
        let local = &self.func.locals[place.local];
        local.type_idx()
    }

    fn constant_type(&self, constant: &Constant) -> &HType {
        self.context.type_(self.constant_type_idx(constant))
    }

    fn constant_type_idx(&self, constant: &Constant) -> Idx<HType> {
        match constant {
            Constant::Int(_) => self.context.core_types().int,
            Constant::Float(_) => self.context.core_types().float,
            Constant::String(_) => todo!(),
            Constant::Func(..) => unreachable!("TODO"),
        }
    }

    /// Seals the given block if:
    /// 1. The block is not sealed
    /// 2. All of the predecessors are finished
    fn safe_seal_block(&mut self, idx: Idx<mir::BasicBlock>, block: ClifBlock) {
        let status = self.statuses.get(idx);
        if matches!(status, Status::Empty | Status::Finished)
            && self.statuses.all_finished(self.func.predecessors.get(idx))
        {
            self.builder.seal_block(block);
            self.statuses.insert(idx, status.seal());
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Status {
    Empty,
    EmptySealed,
    Finished,
    FinishedSealed,
}

impl Status {
    fn seal(self) -> Self {
        match self {
            Self::Empty | Self::EmptySealed => Self::EmptySealed,
            Self::Finished | Self::FinishedSealed => Self::FinishedSealed,
        }
    }

    fn is_sealed(&self) -> bool {
        matches!(self, Self::EmptySealed | Self::FinishedSealed)
    }

    fn finish(self) -> Self {
        match self {
            Self::Empty => Self::Finished,
            Self::EmptySealed => Self::FinishedSealed,

            s => s,
        }
    }

    fn is_finished(&self) -> bool {
        matches!(self, Self::Finished | Self::FinishedSealed)
    }
}

#[derive(Default)]
struct TranslateStatus(ArenaMap<Idx<mir::BasicBlock>, Status>);

impl TranslateStatus {
    fn get(&self, idx: Idx<mir::BasicBlock>) -> Status {
        self.0[idx]
    }

    fn all_finished(&self, ids: &[Idx<mir::BasicBlock>]) -> bool {
        ids.iter().all(|idx| self.0[*idx].is_finished())
    }

    fn insert(&mut self, idx: Idx<mir::BasicBlock>, status: Status) {
        self.0.insert(idx, status);
    }

    fn entry(&mut self, idx: Idx<mir::BasicBlock>) -> Entry<'_, Idx<mir::BasicBlock>, Status> {
        self.0.entry(idx)
    }
}

enum Cmp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Cmp {
    fn as_int_cc(&self) -> IntCC {
        match self {
            Cmp::Equal => IntCC::Equal,
            Cmp::NotEqual => IntCC::NotEqual,
            Cmp::LessThan => IntCC::SignedLessThan,
            Cmp::LessThanOrEqual => IntCC::SignedLessThanOrEqual,
            Cmp::GreaterThan => IntCC::SignedGreaterThan,
            Cmp::GreaterThanOrEqual => IntCC::SignedGreaterThanOrEqual,
        }
    }

    fn as_float_cc(&self) -> FloatCC {
        match self {
            Cmp::Equal => FloatCC::Equal,
            Cmp::NotEqual => FloatCC::NotEqual,
            Cmp::LessThan => FloatCC::LessThan,
            Cmp::LessThanOrEqual => FloatCC::LessThanOrEqual,
            Cmp::GreaterThan => FloatCC::GreaterThan,
            Cmp::GreaterThanOrEqual => FloatCC::GreaterThanOrEqual,
        }
    }
}
