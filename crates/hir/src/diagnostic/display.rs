use std::fmt::format;

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
        match &self.variant {
            super::LoweringDiagnosticVariant::BreakOutsideLoop => {
                "break used outside of a loop".to_owned()
            }
            super::LoweringDiagnosticVariant::UnresolvedImport { path } => {
                format!("Unable to resolve import ‘{path}’")
            }
            super::LoweringDiagnosticVariant::BuiltinAttributeNotAllowed => {
                "The #builtin attribute is only allowed in compiler-provided source".to_owned()
            }
            super::LoweringDiagnosticVariant::BuiltinAttributeRequired => {
                "An intrinsic declaration requires a #builtin(\"...\") attribute".to_owned()
            }
            super::LoweringDiagnosticVariant::UnknownBuiltin { name } => {
                format!("Unknown compiler builtin ‘{name}’")
            }
        }
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
            V::CannotInferTypeArguments => {
                "Cannot infer generic type arguments from this call".to_owned()
            }
            V::ArgsMismatch { expected, actual } => args_mismatch_message(*expected, *actual),
            V::BinaryMismatch { op, lhs, rhs } => {
                binary_mismatch_message(*op, &lhs.display(context), &rhs.display(context))
            }
            V::CalleeNotFunction { actual } => {
                callee_not_function_message(&actual.display(context))
            }
            V::CannotConvertIntoString { actual } => todo!(),
            V::Empty { expr } => todo!(),
            V::Immutable { expr } => immutable_message(&expr.display(context)),
            V::Incompatible { a, b } => todo!(),
            V::NoMatchingSignature { .. } => todo!(),
            V::TypeMismatch { expected, actual } => {
                type_mismatch_message(&expected.display(context), &actual.display(context))
            }
            V::UndefinedFunction { name } => todo!(),
            V::UnresolvedSumVariant { variant, ty } => {
                let variant_name = context.lookup(*variant);
                let ty = ty.display(context);
                format!("Variant ‘{variant_name}’ does not exist in type ‘{ty}’")
            }
            V::UnresolvedProductField { field, ty } => {
                let field_name = context.lookup(*field);
                let ty = ty.display(context);
                format!("Field ‘{field_name}’ does not exist in type ‘{ty}’")
            }
            V::UfcsFunctionNotFound { function, ty } => {
                let function = context.lookup(*function);
                let ty = ty.display(context);
                format!(
                    "No exported function ‘{function}’ is defined in the module that defines type ‘{ty}’"
                )
            }
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
