use super::{Diagnostic, LoweringDiagnostic, TypeDiagnostic, TypeDiagnosticVariant};
use crate::lowering_context::ContextDisplay;
use crate::{BinaryOp, Context};

impl ContextDisplay for Diagnostic {
    fn display(&self, context: &Context) -> String {
        match self {
            Diagnostic::Lowering(lowering) => lowering.display(context),
            Diagnostic::Type(ty) => ty.display(context),
        }
    }
}

impl ContextDisplay for LoweringDiagnostic {
    fn display(&self, context: &Context) -> String {
        todo!()
    }
}

impl ContextDisplay for TypeDiagnostic {
    fn display(&self, context: &Context) -> String {
        self.variant.display(context)
    }
}

impl ContextDisplay for TypeDiagnosticVariant {
    fn display(&self, context: &Context) -> String {
        use TypeDiagnosticVariant as V;
        match self {
            V::ArgsMismatch { expected, actual } => args_mismatch_message(*expected, *actual),
            V::BinaryMismatch { op, lhs, rhs } => {
                let lhs = context.borrow_type(*lhs);
                let rhs = context.borrow_type(*rhs);
                binary_mismatch_message(*op, &lhs.display(context), &rhs.display(context))
            }
            V::CalleeNotFunction { actual } => todo!(),
            V::CannotConvertIntoString { actual } => todo!(),
            V::Empty { expr } => todo!(),
            V::Incompatible { a, b } => todo!(),
            V::NoOverloadFound { name } => todo!(),
            V::TypeMismatch { expected, actual } => todo!(),
            V::UndefinedFunction { name } => todo!(),
            V::UnresolvedVarRef { key } => {
                let name = context.lookup(*key);
                format!("Unable to resolve variable ‘{name}’")
            }
            V::UndefinedSymbol { name } => todo!(),
        }
    }
}

pub fn args_mismatch_message(expected: u32, actual: u32) -> String {
    format!("Expected {expected} arguments, received {actual}")
}

pub fn binary_mismatch_message(op: BinaryOp, lhs: &str, rhs: &str) -> String {
    format!("Cannot apply operation ‘{op}’ to ‘{lhs}’ and ‘{rhs}’")
}

pub fn callee_not_function_message(actual: &str) -> String {
    format!("Tried to call a function, but found an expression with type {actual}")
}
