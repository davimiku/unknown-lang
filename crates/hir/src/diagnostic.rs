use la_arena::Idx;
use lsp_diagnostic::{LSPDiagnostic, LSPDiagnosticSeverity};
use text_size::TextRange;

use crate::{
    interner::Key, lowering_context::ContextDisplay, BinaryOp, Context, Expr, Type, ValueSymbol,
};

#[derive(Debug, PartialEq)]
pub enum Diagnostic {
    Lowering(LoweringDiagnostic),
    Type(TypeDiagnostic),
}

impl TypeDiagnostic {
    fn to_lsp(self) -> LSPDiagnostic {
        LSPDiagnostic {
            range: (self.range.start().into(), self.range.end().into()),
            severity: LSPDiagnosticSeverity::Error,
            message: todo!(),
            code: None,
            code_description: None,
            source: None,
            tags: None,
        }
    }
}

impl From<&Diagnostic> for LSPDiagnostic {
    fn from(value: &Diagnostic) -> Self {
        match value {
            Diagnostic::Type(value) => {
                let range = (value.range.start().into(), value.range.end().into());
                Self {
                    range,
                    severity: LSPDiagnosticSeverity::Error,
                    message: "TODO".to_owned(),
                    code: None,
                    code_description: None,
                    source: Some("type_checker".to_owned()),
                    tags: None,
                }
            }
            _ => {
                panic!("FIXME");
            }
        }
    }
}

/// Diagnostics found while lowering the AST to HIR
#[derive(Debug, PartialEq)]
pub struct LoweringDiagnostic;

/// Diagnostics found during type checking and type inference
#[derive(Debug, PartialEq)]
pub struct TypeDiagnostic {
    pub variant: TypeDiagnosticVariant,
    pub range: TextRange,
}

impl From<TypeDiagnostic> for Diagnostic {
    fn from(value: TypeDiagnostic) -> Self {
        Diagnostic::Type(value)
    }
}

impl TypeDiagnostic {
    pub fn mismatch(expected: Idx<Type>, actual: Idx<Type>, range: TextRange) -> Self {
        Self {
            variant: TypeDiagnosticVariant::TypeMismatch { expected, actual },
            range,
        }
    }

    /// The number of arguments provided to the function call does not match the
    /// expected number of arguments.
    pub fn num_args_mismatch(expected_len: usize, actual_len: usize, range: TextRange) -> Self {
        Self {
            variant: TypeDiagnosticVariant::ArgsMismatch {
                expected: expected_len as u32,
                actual: actual_len as u32,
            },
            range,
        }
    }

    /// An expression was written as a function call but the callee is not a function
    pub fn expected_function(actual: Idx<Type>, range: TextRange) -> Self {
        Self {
            variant: TypeDiagnosticVariant::CalleeNotFunction { actual },
            range,
        }
    }

    pub fn binary_mismatch(lhs: Idx<Type>, rhs: Idx<Type>, op: BinaryOp, range: TextRange) -> Self {
        Self {
            variant: TypeDiagnosticVariant::BinaryMismatch { op, lhs, rhs },
            range,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeDiagnosticVariant {
    /// The number of arguments provided to the function call does not match the
    /// expected number of arguments.
    ArgsMismatch {
        expected: u32,
        actual: u32,
    },

    /// The binary operation cannot be performed on the provided types
    BinaryMismatch {
        op: BinaryOp,
        lhs: Idx<Type>,
        rhs: Idx<Type>,
    },

    /// An expression was written as a function call but the callee is not a function
    CalleeNotFunction {
        actual: Idx<Type>,
    },
    CannotConvertIntoString {
        actual: Idx<Type>,
    },
    Empty {
        expr: Idx<Expr>,
    },
    Incompatible {
        a: Idx<Type>,
        b: Idx<Type>,
    },
    NoOverloadFound {
        // TODO: use interned key?
        name: String,
    },
    TypeMismatch {
        expected: Idx<Type>,
        actual: Idx<Type>,
    },
    UndefinedFunction {
        // TODO: use interned key?
        name: String,
    },
    UnresolvedLocalRef {
        key: Key,
    },
    UndefinedSymbol {
        name: ValueSymbol,
    },
}

impl ContextDisplay for TypeDiagnosticVariant {
    fn display(&self, context: &Context) -> String {
        use TypeDiagnosticVariant::*;
        match self {
            ArgsMismatch { expected, actual } => args_mismatch_message(*expected, *actual),
            BinaryMismatch { op, lhs, rhs } => {
                let lhs = context.borrow_type(*lhs);
                let rhs = context.borrow_type(*rhs);
                binary_mismatch_message(*op, lhs.display(context), rhs.display(context))
            }
            CalleeNotFunction { actual } => todo!(),
            CannotConvertIntoString { actual } => todo!(),
            Empty { expr } => todo!(),
            Incompatible { a, b } => todo!(),
            NoOverloadFound { name } => todo!(),
            TypeMismatch { expected, actual } => todo!(),
            UndefinedFunction { name } => todo!(),
            UnresolvedLocalRef { key } => todo!(),
            UndefinedSymbol { name } => todo!(),
        }
    }
}

// Below are functions to construct the message strings themselves, which are useful
// for unit tests. Parameters to these functions must be Display.
pub fn args_mismatch_message(expected: u32, actual: u32) -> String {
    format!("Expected {expected} arguments, received {actual}")
}

pub fn binary_mismatch_message(op: BinaryOp, lhs: String, rhs: String) -> String {
    format!("Cannot apply operation ‘{op}’ to ‘{lhs}’ and ‘{rhs}’")
}

pub fn callee_not_function_message(actual: String) -> String {
    format!("Tried to call a function, but found an expression with type {actual}")
}
