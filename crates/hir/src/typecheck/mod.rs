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
pub use types::{ArrayType, FunctionType, Type};

use crate::fmt_expr::fmt_type;
use crate::interner::{Interner, Key};
use crate::type_expr::{LocalTypeDefKey, TypeExpr};
use crate::{BinaryOp, Diagnostic, Expr, LocalDefKey};

pub(crate) use self::check::{check_expr, is_subtype};
pub(crate) use self::infer::infer_expr;

#[derive(Debug, Default)]
pub struct TypeDatabase {
    /// Correctly resolved types (inferred or checked)
    /// mapped to the corresponding `Expr` index.
    expr_types: ArenaMap<Idx<Expr>, Type>,

    /// Resolved and inferred `TypeExpr` with their corresponding
    /// `Type`
    /// For example in the following code:
    /// ```ignore
    /// let square = (a: Int) -> a * a
    /// //               ^^^
    /// type Point = struct { x: Float, y: Float }
    /// //           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    /// The `Int` is a type expression that would be mapped to
    /// `Type::Int`.
    ///
    /// The `struct ...` is a type expression that is mapped to
    /// `Type::Struct { ... }`
    type_expr_types: ArenaMap<Idx<TypeExpr>, Type>,

    /// Types that have been inferred for local variables
    ///
    /// For example:
    /// ```ignore
    /// let abc: Int = 42
    /// //  ^^^
    /// let def = some_func ()
    /// //  ^^^
    /// ```
    /// The `abc` is mapped here to Type::Int and the `def` is
    /// mapped here to the Type variant returned by `some_func`.
    local_defs: HashMap<LocalDefKey, Type>,

    /// Types that have been assigned for local type variables.
    ///
    /// For example:
    /// ```ignore
    /// type Point = struct { x: Float, y: Float }
    /// //   ^^^^^
    /// ```
    ///
    /// `Point` local type definition is mapped here to `Type::Struct { ... }`
    local_type_defs: HashMap<LocalTypeDefKey, Type>,
}

impl TypeDatabase {
    pub(crate) fn display(&self, interner: &Interner) -> String {
        let mut output = String::new();

        output.push_str("\nExpression types:\n");
        for (idx, ty) in self.expr_types.iter() {
            output.push_str(&format!("{idx:?}: {ty:?}\n"));
        }

        output.push_str("\nType Expression types:\n");
        for (idx, ty) in self.type_expr_types.iter() {
            output.push_str(&format!("{idx:?}: {ty:?}\n"));
        }

        output.push_str("\nLocal Defs:\n");
        for (key, ty) in self.local_defs.iter() {
            output.push_str(&format!("{}: {:?}\n", key.display(interner), ty));
        }

        output.push_str("\nLocal Type Defs:\n");
        for (key, ty) in self.local_type_defs.iter() {
            output.push_str(&format!("{}: {:?}\n", key.display(interner), ty));
        }

        output
    }
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

    pub(super) fn get_local(&self, key: &LocalDefKey) -> Option<&Type> {
        self.local_defs.get(key)
    }

    pub(super) fn set_local(&mut self, key: LocalDefKey, ty: Type) {
        self.local_defs.insert(key, ty);
    }

    pub(super) fn get_local_type(&self, key: &LocalTypeDefKey) -> Option<&Type> {
        self.local_type_defs.get(key)
    }

    pub(super) fn set_local_type(&mut self, key: LocalTypeDefKey, ty: Type) {
        self.local_type_defs.insert(key, ty);
    }
}

pub(crate) fn fmt_local_types(s: &mut String, results: &TypeDatabase, interner: &Interner) {
    s.push('\n');
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
    ArgsMismatch { expected: u32, actual: u32 },
    BinaryMismatch { op: BinaryOp, lhs: Type, rhs: Type },
    // TODO: better name "tried to call something that was not a function"
    CalleeNotFunction { actual: Type },
    CannotConvertIntoString { actual: Type },
    Empty { expr: Idx<Expr> },
    Incompatible { a: Type, b: Type },
    NoOverloadFound { name: String },
    TypeMismatch { expected: Type, actual: Type },
    UndefinedLocal { name: Key },
    UndefinedFunction { name: String },
    Undefined { name: LocalDefKey },
}

impl TypeDiagnostic {
    pub fn message(&self) -> String {
        format!("{:?}", self)
    }
}

struct TypeResult {
    ty: Type,
    diagnostics: Vec<TypeDiagnostic>,
}

impl TypeResult {
    fn as_result(self) -> Result<Type, Vec<TypeDiagnostic>> {
        if self.diagnostics.is_empty() {
            Ok(self.ty)
        } else {
            Err(self.diagnostics)
        }
    }

    fn unwrap(&self) -> &Type {
        debug_assert!(self.diagnostics.is_empty());

        &self.ty
    }
}
