//! Typechecking
//!
//! This module group consists of three main concepts:
//!
//! - Type definitions themselves
//! - Type checks for the user's explicit annotations
//! - Type inference for when the user does not annotate

mod builtins;
mod check;
mod infer;
mod types;
use std::collections::HashMap;

use la_arena::{ArenaMap, Idx};
use text_size::TextRange;
pub use types::Type;

use crate::{interner::Interner, BinaryOp, Context, Expr, LocalDefKey, LocalRefName};

use self::check::check_expr;

// returns diagnostics
pub fn check(expr: Idx<Expr>, context: &Context) -> TypeCheckResults {
    let mut results = TypeCheckResults::default();
    check_expr(expr, &Type::Unit, &mut results, context);

    results
}

#[derive(Debug, Default, PartialEq)]
pub struct TypeCheckResults {
    /// Correctly resolved types (inferred or checked)
    /// mapped to the corresponding `Expr` index.
    expr_types: ArenaMap<Idx<Expr>, Type>,

    local_types: HashMap<LocalDefKey, Type>,
}

impl TypeCheckResults {
    pub(super) fn get_expr_type(&self, idx: Idx<Expr>) -> Option<&Type> {
        self.expr_types.get(idx)
    }

    pub(super) fn set_expr_type(&mut self, idx: Idx<Expr>, ty: Type) {
        self.expr_types.insert(idx, ty);
    }

    // pub(super) fn get_local_type(&self, key: &LocalDefKey) -> Option<&Type> {
    //     self.local_types.get(key)
    // }

    // pub(super) fn set_local_type(&mut self, key: LocalDefKey, ty: Type) {
    //     self.local_types.insert(key, ty);
    // }
}

pub(crate) fn fmt_local_types(s: &mut String, results: &TypeCheckResults, interner: &Interner) {
    if !results.local_types.is_empty() {
        s.push('\n');
    }
    for (key, ty) in results.local_types.iter() {
        s.push_str(&key.display(interner));
        s.push_str(" : ");
        s.push_str(&format!("{}\n", ty.display(interner)));
    }
}

pub struct FunctionSignature {
    /// Types of the function parameters, in order
    parameter_types: Vec<Type>,

    /// Return type of the function (inferred or checked)
    return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct TypeDiagnostic {
    pub variant: TypeDiagnosticVariant,
    pub range: TextRange,
}

#[derive(Debug, PartialEq)]
pub enum TypeDiagnosticVariant {
    ArgsMismatch {
        name: String,
        expected: u32,
        actual: u32,
    },
    BinaryMismatch {
        op: BinaryOp,
        lhs: Type,
        rhs: Type,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
    },
    UndefinedLocal {
        name: LocalRefName,
    },
    Undefined {
        name: String,
    },
    NoOverloadFound {
        name: String,
    },
    Incompatible {
        a: Type,
        b: Type,
    },
}
