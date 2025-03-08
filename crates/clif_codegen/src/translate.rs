//! Translation of the mid-level intermediate representation (MIR) to the
//! Cranelift intermediate format (CLIF).
//!
//! https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
//!
//! Uses the cranelift_frontend Builder and Context to walk the control flow graph of
//! the MIR and translate it into the CLIF instructions. This is a "translation" rather
//! than a lowering because it's between two representations at roughly the same level of
//! abstraction.

mod arithmetic;

use std::collections::HashMap;
use std::ops::Deref;

use cranelift::codegen::ir::UserFuncName;
use cranelift::frontend::Switch;
use cranelift::prelude::Block as ClifBlock;
use cranelift::prelude::Type as ClifType;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Module};
use hir::Type as HType;
use la_arena::Entry;
use la_arena::{ArenaMap, Idx};
use mir::{
    BinOpKind, BlockTarget, BranchIntTargets, Constant, Local, Operand, Place, Rvalue, Statement,
    Terminator,
};
use util_macros::assert_matches;

use crate::ext::function_builder::FunctionBuilderExt;

// TODO: decide on nomenclature for "Type"
// currently "ClifType" means "Cranelift Type" and
// "HType" means "HIR Type"

pub(crate) struct CommonTypes {
    int: ClifType,
    float: ClifType,
    ptr: ClifType,
}

impl Default for CommonTypes {
    fn default() -> Self {
        Self {
            int: types::I64,
            float: types::F64,

            // TODO - cranelift got rid of the R64 type
            // https://github.com/bytecodealliance/wasmtime/pull/9164
            // https://github.com/bytecodealliance/wasmtime/pull/8728
            ptr: types::I64, // types::R64
        }
    }
}

type BlockMap = ArenaMap<Idx<mir::BasicBlock>, ClifBlock>;

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub(crate) struct FunctionTranslator<'a> {
    /// Convenience shortcut for common types
    pub(crate) types: CommonTypes,

    pub(crate) func: &'a mir::Function,

    /// The builder for the function that is currently being constructed
    builder: FunctionBuilder<'a>,

    /// Reference to the module that this function is being constructed inside of
    module: &'a mut JITModule,

    /// Lowered and typechecked HIR context
    context: &'a hir::Context,

    /// map from our MIR FuncId to CLIF FuncId
    func_map: &'a HashMap<mir::FuncId, FuncId>,

    /// Map from MIR local definitions to CLIF variables
    ///
    /// TODO: Cranelift uses SSA, so should the mapping be reversed?
    ///
    /// TODO: swap HashMap for FxMap or similar, doesn't need the security of the default hasher
    variables: HashMap<Idx<Local>, Variable>,

    status: TranslateStatus,
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
            types: Default::default(),
            variables: Default::default(),
            status: Default::default(),
        }
    }
}

impl<'a> FunctionTranslator<'a> {
    pub(crate) fn translate_function(mut self) {
        let mir::Function {
            blocks,
            locals,
            predecessors,
            ..
        } = self.func;

        self.translate_signature();

        let mut block_map: BlockMap = ArenaMap::with_capacity(blocks.len());

        // create all reachable blocks (empty/default)
        for (idx, _) in blocks.iter() {
            if predecessors.has_predecessors(idx) {
                block_map.insert(idx, self.builder.create_block());
                self.status.insert(idx, Status::Empty);
            }
        }

        // declare all locals with type. value is defined later with def_var
        // params will def_var shortly, other locals are def_var when the block is translated
        for (local_idx, local) in locals.iter() {
            let local_ty = local.type_(self.context);
            if !local_ty.is_unit() {
                let var_index = local_idx.into_raw().into_u32() as usize;
                let var = Variable::new(var_index);
                let var_ty = self.translate_type(local.type_(self.context));
                self.builder.declare_var(var, var_ty);
                self.variables.insert(local_idx, var);
            }
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
        let param_locals = self.func.param_locals().enumerate();
        for (i, (local_idx, _)) in param_locals {
            let val = self.builder.block_params(entry_block)[i];
            let var = self.variables[&local_idx];
            self.builder.def_var(var, val);
        }

        // translate the rest of the blocks after the entry block
        for (idx, block) in block_map.iter() {
            self.builder.switch_to_block(*block);
            // cranelift recommends sealing blocks as soon as possible
            // self.seal_block(idx, *block);

            self.translate_basic_block(idx, &block_map);
            self.status
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

    fn translate_signature(&mut self) {
        let (module_id, symbol_id) = self.func.id.into();
        self.builder.func.name = UserFuncName::user(module_id, symbol_id);

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
        if !return_ty.is_unit() {
            let return_ty = self.translate_type(return_ty);
            self.builder
                .func
                .signature
                .returns
                .push(AbiParam::new(return_ty));
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
        let args: Vec<Value> = args.iter().map(|arg| self.translate_operand(arg)).collect();

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
                let var = self.variables[&destination.local];
                self.builder.def_var(var, returns[0]);
            }
            _ => unreachable!("multiple returns not implemented yet"),
        }

        // FIXME: remove unwrap when panics are implemented
        let block_call_args = self.locals_to_values(target.unwrap().1);
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
            let return_var = self.variables[&return_local_idx];
            let ret_val = self.builder.use_var(return_var);
            self.builder.ins().return_(&[ret_val]);
        }
    }

    fn locals_to_values(&mut self, locals: &[Idx<Local>]) -> Vec<Value> {
        locals
            .iter()
            .map(|local| self.variables[local])
            .map(|var| self.builder.use_var(var))
            .collect()
    }

    fn translate_rvalue(&mut self, rvalue: &Rvalue) -> Value {
        match rvalue {
            Rvalue::Use(op) => self.translate_operand(op),
            Rvalue::BinaryOp(binop, ops) => self.translate_binary_op(binop, ops.deref()),
            Rvalue::UnaryOp(unop, op) => todo!(),
            Rvalue::Discriminant(place) => self.translate_discriminant(place),
        }
    }

    fn translate_binary_op(&mut self, binop: &BinOpKind, ops: &(Operand, Operand)) -> Value {
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

    fn emit_comparison(&mut self, lhs: &Operand, rhs: &Operand, comparison: Cmp) -> Value {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);

        if lhs_ty.is_float() {
            let lhs_val = self.translate_operand(lhs);
            let rhs_val = self.translate_operand(rhs);
            self.builder
                .ins()
                .fcmp(comparison.as_float_cc(), lhs_val, rhs_val)
        } else if lhs_ty.is_int() {
            self.emit_int_comparison(lhs, rhs, comparison.as_int_cc())
        } else {
            unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for comparison")
        }
    }

    fn emit_int_comparison(&mut self, lhs: &Operand, rhs: &Operand, comparison: IntCC) -> Value {
        // TODO: check for constants
        //  - if lhs and rhs are constant, fold to a bool_const
        //  - if rhs is a constant, emit icmp_imm
        //  - if lhs is a constant, invert the condition (IntCC::complement)? and emit icmp_imm
        //  - otherwise emit icmp
        let lhs_val = self.translate_operand(lhs);
        let rhs_val = self.translate_operand(rhs);

        let val_i8 = self.builder.ins().icmp(comparison, lhs_val, rhs_val);
        self.builder.ins().sextend(self.types.int, val_i8)
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

    // TODO: better way to use the return type here or determine this earlier?
    fn translate_operand(&mut self, op: &Operand) -> Value {
        match op {
            Operand::Copy(p) => {
                let local = p.local;
                // TODO: use projections too
                let var = self.variables[&local];
                // TODO: is this sharing or copying?
                self.builder.use_var(var)
            }
            Operand::Constant(c) => match c {
                Constant::Int(i) => self.builder.ins().iconst(self.types.int, *i),
                Constant::Float(f) => self.builder.ins().f64const(*f),
                Constant::String(_) => todo!(),
                Constant::Func(..) => unreachable!("TODO"),
            },
            Operand::Move(_) => todo!(),
        }
    }

    fn translate_discriminant(&mut self, place: &Place) -> Value {
        let ty = self.place_type(place, self.context);
        let sum_ty = assert_matches!(ty, HType::Sum);

        if sum_ty.is_unit(self.context) {
            // TODO: use projections too
            let var = self.variables[&place.local];
            self.builder.use_var(var)
        } else {
            // TODO - determine how to lower a sum type with payload
            todo!()
        }
    }

    fn translate_type_idx(&mut self, idx: Idx<HType>) -> ClifType {
        let ty = self.context.type_(idx);
        self.translate_type(ty)
    }

    fn translate_type(&mut self, ty: &HType) -> ClifType {
        match ty {
            HType::FloatLiteral(_) | HType::Float => self.types.float,
            HType::IntLiteral(_) | HType::Int => self.types.int,
            HType::StringLiteral(_) | HType::String => todo!(),

            // TODO: split tag and payload somehow (likely before this point)
            HType::Sum(sum) => self.types.int,

            HType::Unit => unreachable!("Internal Compiler Error (CLIF): unit types should be unused rather than translated"),
            t => todo!("{t:?}"),
        }
    }

    fn op_type(&self, op: &Operand) -> &HType {
        match self.context.type_(self.op_type_idx(op)) {
            HType::FloatLiteral(_) => &HType::Float,
            HType::IntLiteral(_) => &HType::Int,
            HType::StringLiteral(_) => &HType::String,
            t => t,
        }
    }

    fn op_type_idx(&self, op: &Operand) -> Idx<HType> {
        match op {
            Operand::Copy(place) => self.place_type_idx(place),
            Operand::Constant(constant) => self.constant_type_idx(constant),
            Operand::Move(_) => todo!(),
        }
    }

    fn place_type<'b>(&self, place: &Place, context: &'b hir::Context) -> &'b HType {
        let local = &self.func.locals[place.local];
        local.type_(context)
    }

    fn place_type_idx(&self, place: &Place) -> Idx<HType> {
        // TODO: use projections...
        let local = &self.func.locals[place.local];
        local.type_idx_of()
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
        let status = self.status.get(idx);
        if matches!(status, Status::Empty | Status::Finished)
            && self.status.all_finished(self.func.predecessors.get(idx))
        {
            self.builder.seal_block(block);
            self.status.insert(idx, status.seal());
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
