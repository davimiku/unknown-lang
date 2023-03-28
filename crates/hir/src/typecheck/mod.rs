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

use crate::expr::LocalRefName;
use crate::fmt_expr::fmt_type;
use crate::interner::Interner;
use crate::type_expr::{LocalTypeDefKey, TypeExpr};
use crate::{BinaryOp, Diagnostic, Expr, LocalDefKey};

pub(crate) use self::check::check_expr;
pub(crate) use self::infer::infer_expr;

#[derive(Debug, Default)]
pub struct TypeDatabase {
    /// Correctly resolved types (inferred or checked)
    /// mapped to the corresponding `Expr` index.
    expr_types: ArenaMap<Idx<Expr>, Type>,

    type_expr_types: ArenaMap<Idx<TypeExpr>, Type>,

    local_defs: HashMap<LocalDefKey, Type>,

    local_type_defs: HashMap<LocalTypeDefKey, Type>,
}

impl TypeDatabase {
    pub(super) fn get_expr_type(&self, idx: Idx<Expr>) -> Option<&Type> {
        self.expr_types.get(idx)
    }

    pub(super) fn set_expr_type(&mut self, idx: Idx<Expr>, ty: Type) {
        self.expr_types.insert(idx, ty);
    }

    pub(super) fn get_type_expr_type(&self, idx: Idx<TypeExpr>) -> Option<&Type> {
        self.type_expr_types.get(idx)
    }

    pub(super) fn set_type_expr_type(&mut self, idx: Idx<TypeExpr>, ty: Type) {
        self.type_expr_types.insert(idx, ty);
    }

    pub(super) fn get_local_type(&self, key: &LocalTypeDefKey) -> Option<&Type> {
        self.local_type_defs.get(key)
    }

    pub(super) fn set_local_type(&mut self, key: LocalTypeDefKey, ty: Type) {
        self.local_type_defs.insert(key, ty);
    }
}

pub(crate) fn fmt_local_types(s: &mut String, results: &TypeDatabase, interner: &Interner) {
    if !results.local_defs.is_empty() {
        s.push('\n');
    }
    let mut locals: Vec<_> = results
        .local_defs
        .iter()
        .map(|(key, ty)| (key.display(interner), ty.clone()))
        .collect();
    locals.sort_by(|(a, ..), (b, ..)| a.cmp(b));

    for (name, ty) in locals.iter() {
        s.push_str(name);
        s.push_str(" : ");
        s.push_str(&format!("{}\n", fmt_type(ty, interner)));
    }
}

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
    UndefinedFunction {
        name: String,
    },
    Undefined {
        name: LocalDefKey,
    },
    NoOverloadFound {
        name: String,
    },
    Incompatible {
        a: Type,
        b: Type,
    },
}
