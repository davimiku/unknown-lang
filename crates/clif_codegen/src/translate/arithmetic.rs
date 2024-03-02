use cranelift::codegen::ir::{
    types::{F64, I64},
    InstBuilder, Value,
};
use hir::Type as HType;
use mir::Operand;

use super::FunctionTranslator;

impl FunctionTranslator<'_> {
    pub(super) fn emit_add(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (lhs_ty, rhs_ty) {
            (HType::Float, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().fadd(lhs_val, rhs_val)
            }
            (HType::Float, HType::Int) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                self.builder.ins().fadd(lhs_val, rhs_val)
            }
            (HType::Int, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                self.builder.ins().fadd(lhs_val, rhs_val)
            }
            (HType::Int, HType::Int) => self.iadd(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_sub(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (lhs_ty, rhs_ty) {
            (HType::Float, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().fsub(lhs_val, rhs_val)
            }
            (HType::Float, HType::Int) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                self.builder.ins().fsub(lhs_val, rhs_val)
            }
            (HType::Int, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                self.builder.ins().fsub(lhs_val, rhs_val)
            }
            (HType::Int, HType::Int) => self.isub(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_mul(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (lhs_ty, rhs_ty) {
            (HType::Float, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().fmul(lhs_val, rhs_val)
            }
            (HType::Float, HType::Int) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let rhs_val = self.builder.ins().fcvt_from_sint(F64, rhs_val);
                self.builder.ins().fmul(lhs_val, rhs_val)
            }
            (HType::Int, HType::Float) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                let lhs_val = self.builder.ins().fcvt_from_sint(F64, lhs_val);
                self.builder.ins().fmul(lhs_val, rhs_val)
            }
            (HType::Int, HType::Int) => self.imul(lhs, rhs),

            _ => unreachable!("unexpected types {lhs_ty:?} and {rhs_ty:?} for addition",),
        }
    }

    pub(super) fn emit_div(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        let lhs_val = self.operand_to_value(lhs);
        let rhs_val = self.operand_to_value(rhs);
        let lhs_ty = self.op_type(lhs);
        let rhs_ty = self.op_type(rhs);
        // these Type should never be IntLiteral or FloatLiteral, since op_type normalizes those to Int/Float
        match (lhs_ty, rhs_ty) {
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
        }
    }

    pub(super) fn emit_rem(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        // TODO: if both LHS and RHS are constants, could fold them here
        // or may be better to do that in MIR

        let lhs_val = self.operand_to_value(lhs);
        let rhs_val = self.operand_to_value(rhs);

        // TODO: this traps on divisor=0 or (numerator=Int.MIN && divisor=-1)
        // instead, once panic machinery is built, emit icmp and jumps to unwind blocks
        // (implement these in MIR first)
        self.builder.ins().srem(lhs_val, rhs_val)
    }

    fn iadd(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        match (lhs.as_int(), rhs.as_int()) {
            (None, None) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().iadd(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = self.operand_to_value(lhs);
                self.builder.ins().iadd_imm(lhs_val, *i)
            }
            (Some(i), None) => {
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().iadd_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = *a + *b;
                self.builder.ins().iconst(I64, c)
            }
        }
    }

    fn isub(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        match (lhs.as_int(), rhs.as_int()) {
            (None, None) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().isub(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = self.operand_to_value(lhs);
                self.builder.ins().iadd_imm(lhs_val, -(*i))
            }
            (Some(i), None) => {
                let rhs_val = self.operand_to_value(rhs);
                let rhs_val = self.builder.ins().ineg(rhs_val);
                self.builder.ins().iadd_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = *a - *b;
                self.builder.ins().iconst(I64, c)
            }
        }
    }

    fn imul(&mut self, lhs: &Operand, rhs: &Operand) -> Value {
        match (lhs.as_int(), rhs.as_int()) {
            (None, None) => {
                let lhs_val = self.operand_to_value(lhs);
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().imul(lhs_val, rhs_val)
            }
            (None, Some(i)) => {
                let lhs_val = self.operand_to_value(lhs);
                self.builder.ins().imul_imm(lhs_val, *i)
            }
            (Some(i), None) => {
                let rhs_val = self.operand_to_value(rhs);
                self.builder.ins().imul_imm(rhs_val, *i)
            }
            // regardless of optimization settings, this gets trivially constant-folded
            (Some(a), Some(b)) => {
                // TODO: overflow should probably have been detected earlier
                // but we could also detect that here and report a diagnostic, or _maybe_ an ICE
                let c = *a * *b;
                self.builder.ins().iconst(I64, c)
            }
        }
    }
}
