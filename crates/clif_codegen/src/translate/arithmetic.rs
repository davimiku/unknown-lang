use cranelift::codegen::ir::types::{F64, I64};
use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::StackSlotData;
use hir::Type as HType;
use mir::Operand;

use crate::macros::assert_val;
use crate::place::CValue;

use super::FunctionTranslator;

impl FunctionTranslator<'_> {
    pub(super) fn emit_add(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (self.context.type_(lhs_ty), self.context.type_(rhs_ty)) {
            (HType::Float, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));

                let val = self.builder.ins().fadd(lhs_val, rhs_val);
                CValue::Val {
                    val,
                    layout: self.layouts.float,
                }
            }
            (HType::Float, HType::Int) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                let val = self.builder.ins().fadd(lhs_val, rhs_val);
                CValue::Val {
                    val,
                    layout: self.layouts.float,
                }
            }
            (HType::Int, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                let val = self.builder.ins().fadd(lhs_val, rhs_val);
                CValue::Val {
                    val,
                    layout: self.layouts.float,
                }
            }
            (HType::Int, HType::Int) => self.iadd(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_sub(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let lhs_ty = self.context.type_(self.op_type(lhs));
        let rhs_ty = self.context.type_(self.op_type(rhs));
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (lhs_ty, rhs_ty) {
            (HType::Float, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));

                let val = self.builder.ins().fsub(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Float, HType::Int) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);

                let val = self.builder.ins().fsub(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Int, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);

                let val = self.builder.ins().fsub(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Int, HType::Int) => self.isub(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_mul(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (self.context.type_(lhs_ty), self.context.type_(rhs_ty)) {
            (HType::Float, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let val = self.builder.ins().fmul(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Float, HType::Int) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                let val = self.builder.ins().fmul(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Int, HType::Float) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                let val = self.builder.ins().fmul(lhs_val, rhs_val);
                let layout = self.layouts.float;
                CValue::Val { val, layout }
            }
            (HType::Int, HType::Int) => self.imul(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_div(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let lhs_val = assert_val!(self.translate_operand(lhs));
        let rhs_val = assert_val!(self.translate_operand(rhs));
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        let val = match (self.context.type_(lhs_ty), self.context.type_(rhs_ty)) {
            (HType::Float, HType::Float) => self.builder.ins().fdiv(lhs_val, rhs_val),
            (HType::Float, HType::Int) => {
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                self.builder.ins().fdiv(lhs_val, rhs_val)
            }
            (HType::Int, HType::Float) => {
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                self.builder.ins().fdiv(lhs_val, rhs_val)
            }
            (HType::Int, HType::Int) => {
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                self.builder.ins().fdiv(lhs_val, rhs_val)
            }

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        };
        let layout = self.layouts.float;
        CValue::Val { val, layout }
    }

    pub(super) fn emit_rem(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        // TODO: if both LHS and RHS are constants, could fold them here

        let lhs_val = assert_val!(self.translate_operand(lhs));
        let rhs_val = assert_val!(self.translate_operand(rhs));

        // TODO: this traps on divisor=0 or (numerator=Int.MIN && divisor=-1)
        // instead, once panic machinery is built, emit icmp and jumps to unwind blocks
        // (implement these in MIR first)
        let val = self.builder.ins().srem(lhs_val, rhs_val);
        let layout = self.layouts.int;
        CValue::Val { val, layout }
    }

    fn iadd(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let val = match (lhs.as_const(), rhs.as_const()) {
            (None, None) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                self.builder.ins().iadd(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                self.builder.ins().iadd_imm(lhs_val, *i)
            }
            (Some(i), None) => {
                let rhs_val = assert_val!(self.translate_operand(rhs));
                self.builder.ins().iadd_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = (*a) + (*b);
                self.builder.ins().iconst(I64, c)
            }
        };
        let layout = self.layouts.int;
        CValue::Val { val, layout }
    }

    fn isub(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let val = match (lhs.as_const(), rhs.as_const()) {
            (None, None) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                self.builder.ins().isub(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                self.builder.ins().iadd_imm(lhs_val, -(*i))
            }
            (Some(i), None) => {
                let rhs_val = assert_val!(self.translate_operand(rhs));
                let rhs_val = self.builder.ins().ineg(rhs_val);
                self.builder.ins().iadd_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = (*a) - (*b);
                self.builder.ins().iconst(I64, c)
            }
        };
        let layout = self.layouts.int;
        CValue::Val { val, layout }
    }

    fn imul(&mut self, lhs: &Operand, rhs: &Operand) -> CValue {
        let val = match (lhs.as_const(), rhs.as_const()) {
            (None, None) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                let rhs_val = assert_val!(self.translate_operand(rhs));
                self.builder.ins().imul(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = assert_val!(self.translate_operand(lhs));
                self.builder.ins().imul_imm(lhs_val, *i)
            }
            (Some(i), None) => {
                let rhs_val = assert_val!(self.translate_operand(rhs));
                self.builder.ins().imul_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = (*a) * (*b);
                self.builder.ins().iconst(I64, c)
            }
        };
        let layout = self.layouts.int;
        CValue::Val { val, layout }
    }
}
