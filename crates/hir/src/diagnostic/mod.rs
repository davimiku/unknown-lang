mod display;

use la_arena::Idx;
use lsp_diagnostic::{LSPDiagnostic, LSPDiagnosticSeverity};
use text_size::TextRange;

use crate::interner::Key;
use crate::{BinaryOp, Expr, Type, ValueSymbol};

#[derive(Debug, PartialEq)]
pub enum Diagnostic {
    Lowering(LoweringDiagnostic),
    Type(TypeDiagnostic),
}

impl Diagnostic {
    pub fn range(&self) -> TextRange {
        match self {
            Diagnostic::Lowering(diag) => diag.range,
            Diagnostic::Type(diag) => diag.range,
        }
    }
}

impl From<TypeDiagnostic> for LSPDiagnostic {
    fn from(value: TypeDiagnostic) -> Self {
        Self {
            range: (value.range.start().into(), value.range.end().into()),
            severity: LSPDiagnosticSeverity::Error,
            message: String::from("TODO!"),
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

/// Diagnostics found while lowering the AST to HIR not related to type checking
#[derive(Debug, PartialEq)]
pub struct LoweringDiagnostic {
    pub variant: LoweringDiagnosticVariant,
    pub range: TextRange,
}

impl From<LoweringDiagnostic> for Diagnostic {
    fn from(diagnostic: LoweringDiagnostic) -> Self {
        Diagnostic::Lowering(diagnostic)
    }
}

impl LoweringDiagnostic {
    pub fn break_outside_loop(range: TextRange) -> Self {
        Self {
            variant: LoweringDiagnosticVariant::BreakOutsideLoop,
            range,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LoweringDiagnosticVariant {
    BreakOutsideLoop,
    // ReturnOutsideFunction
}

/// Diagnostics found during type checking and type inference
#[derive(Debug, PartialEq)]
pub struct TypeDiagnostic {
    pub variant: TypeDiagnosticVariant,
    pub range: TextRange,
}

impl From<TypeDiagnostic> for Diagnostic {
    fn from(diagnostic: TypeDiagnostic) -> Self {
        Diagnostic::Type(diagnostic)
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

    pub fn no_matching_signature(range: TextRange) -> Self {
        Self {
            variant: TypeDiagnosticVariant::NoMatchingSignature {},
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
    Immutable {
        /// Expression that is trying to do the mutation
        expr: Idx<Expr>,
        // The original definition of the immutable symbol
        // symbol: ValueSymbol,
    },
    Incompatible {
        a: Idx<Type>,
        b: Idx<Type>,
    },
    NoMatchingSignature {
        // TODO: decide what information is needed in this diagnostic
    },
    TypeMismatch {
        expected: Idx<Type>,
        actual: Idx<Type>,
    },
    UndefinedFunction {
        // TODO: use interned key?
        name: String,
    },
    UnresolvedVarRef {
        key: Key,
    },
    UndefinedSymbol {
        name: ValueSymbol,
    },
}
