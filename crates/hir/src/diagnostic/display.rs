use super::{Diagnostic, LoweringDiagnostic, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{BinaryOp, Context, ContextDisplay};

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
                binary_mismatch_message(*op, &lhs.display(context), &rhs.display(context))
            }
            V::CalleeNotFunction { actual } => todo!(),
            V::CannotConvertIntoString { actual } => todo!(),
            V::Empty { expr } => todo!(),
            V::Immutable { expr } => immutable_message(&expr.display(context)),
            V::Incompatible { a, b } => todo!(),
            V::NoMatchingSignature { .. } => todo!(),
            V::TypeMismatch { expected, actual } => {
                type_mismatch_message(&expected.display(context), &actual.display(context))
            }
            V::UndefinedFunction { name } => todo!(),
            V::UnresolvedVarRef { key } => {
                let name = context.lookup(*key);
                format!("Unable to resolve variable ‘{name}’")
            }
            V::UndefinedSymbol { name } => {
                let name = name.display(context);
                panic!("unexpected undefined symbol: ‘{name}’")
            }
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

pub fn type_mismatch_message(expected: &str, actual: &str) -> String {
    format!("Expected type {expected}, received type {actual}")
}

pub fn immutable_message(expr: &str) -> String {
    format!("Expression '{expr}' attempts to mutate an immutable value.")
}
