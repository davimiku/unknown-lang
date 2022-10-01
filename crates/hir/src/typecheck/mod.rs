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

// returns diagnostics
// mutates context to add inferred types to TypeCheckResults
pub fn typecheck(expr: Idx<Expr>, context: &mut Context) {
    todo!()
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
    TypeMismatch { expected: Type, actual: Type },
    Undefined { name: String },
}
