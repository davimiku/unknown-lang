//! Typechecking
//!
//! This module group consists of three main concepts:
//!
//! - Type definitions themselves
//! - Type checks for the user's explicit annotations
//! - Type inference for when the user does not annotate

mod check;
mod infer;
mod types;
use std::ops::Index;

use la_arena::{ArenaMap, Idx};
use text_size::TextRange;
pub use types::Type;

use crate::{Context, Expr};

use self::check::check_expr;

// returns diagnostics
// mutates context to add inferred types to TypeCheckResults
pub fn check(expr: Idx<Expr>, context: &Context) -> TypeCheckResults {
    let mut results = TypeCheckResults::default();
    check_expr(expr, Type::Unit, &mut results, context);

    results
}

#[derive(Debug, Default)]
pub struct TypeCheckResults {
    /// Correctly resolved types (inferred or checked)
    /// mapped to the corresponding `Expr` index.
    expr_types: ArenaMap<Idx<Expr>, Type>,
}

impl Index<Idx<Expr>> for TypeCheckResults {
    type Output = Type;

    fn index(&self, idx: Idx<Expr>) -> &Self::Output {
        &self.expr_types[idx]
    }
}

impl TypeCheckResults {
    pub(super) fn set_type(&mut self, idx: Idx<Expr>, r#type: Type) {
        self.expr_types.insert(idx, r#type)
    }
}

pub struct FunctionSignature {
    /// Types of the function parameters, in order
    parameter_types: Vec<Type>,

    /// Return type of the function (inferred or checked)
    return_type: Type,
}

pub struct TypeDiagnostic {
    pub variant: TypeDiagnosticVariant,
    pub range: TextRange,
}

pub enum TypeDiagnosticVariant {
    ArgsMismatch {
        name: String,
        expected: u32,
        actual: u32,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
    },
    Undefined {
        name: String,
    },
}
