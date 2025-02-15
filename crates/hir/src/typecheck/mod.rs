//! Typechecking
//!
//! This module group consists of three main concepts:
//!
//! - Type definitions themselves
//! - Type checks for the user's explicit annotations
//! - Type inference for when the user does not annotate

mod check;
mod infer;
mod traits;
mod types;
mod widen;

use std::collections::HashMap;

use la_arena::{Arena, ArenaMap, Idx};
pub use types::{ArrayType, FuncSignature, FunctionType, Type};

use crate::diagnostic::{Diagnostic, TypeDiagnostic, TypeDiagnosticVariant};
use crate::type_expr::{TypeExpr, TypeSymbol};
use crate::{Context, ContextDisplay, Expr, Interner, ValueSymbol};

pub(crate) use self::check::check_expr;
pub(crate) use self::infer::{infer_expr, infer_module};

#[derive(Debug)]
pub struct TypeDatabase {
    /// Allocated Type variants
    types: Arena<Type>,

    /// Correctly resolved types (inferred or checked)
    /// mapped to the corresponding `Expr` index.
    expr_types: ArenaMap<Idx<Expr>, Idx<Type>>,

    /// Resolved and inferred `TypeExpr` with their corresponding
    /// `Type`
    /// For example in the following code:
    /// ```ignore
    /// let square = (a: Int) -> a * a
    /// //               ^^^
    /// type Point = ( x: Float, y: Float )
    /// //           ^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    /// The `Int` is a type expression that would be mapped to
    /// `Type::Int`.
    ///
    /// The RHS of `Point` is a type expression that is mapped to
    /// `Type::Record { ... }`
    type_expr_types: ArenaMap<Idx<TypeExpr>, Idx<Type>>,

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
    pub(crate) value_symbols: HashMap<ValueSymbol, Idx<Type>>,

    /// Types that have been assigned for local type variables.
    ///
    /// For example:
    /// ```ignore
    /// type Point = struct { x: Float, y: Float }
    /// //   ^^^^^
    /// ```
    ///
    /// `Point` local type definition is mapped here to `Type::Struct { ... }`
    type_symbols: HashMap<TypeSymbol, Idx<Type>>,

    /// Core types that are builtin to all programs
    pub(crate) core: CoreTypes,
    // TODO - keep a map/list of core symbols, like "false" and "true"?
    // pub(crate) core_symbols:
}

impl TypeDatabase {
    pub(crate) fn new(interner: &Interner) -> Self {
        let mut types = Arena::new();

        let unknown = types.alloc(Type::Unknown);
        let error = types.alloc(Type::Error);
        let top = types.alloc(Type::Top);
        let bottom = types.alloc(Type::Bottom);
        let unit = types.alloc(Type::Unit);
        let float = types.alloc(Type::Float);
        let int = types.alloc(Type::Int);
        let string = types.alloc(Type::String);
        let r#bool = types.alloc(Type::sum(vec![
            (interner.core_keys().r#false, unit),
            (interner.core_keys().r#true, unit),
        ]));
        let core = CoreTypes {
            unknown,
            error,
            top,
            bottom,
            unit,
            float,
            int,
            string,
            bool,
        };
        Self {
            types,
            core,
            expr_types: Default::default(),
            type_expr_types: Default::default(),
            value_symbols: Default::default(),
            type_symbols: Default::default(),
        }
    }
}

impl ContextDisplay for TypeDatabase {
    fn display(&self, context: &Context) -> String {
        let mut output = String::new();

        output.push_str("\nExpression types:\n");
        for (idx, ty) in self.expr_types.iter() {
            output.push_str(&format!(
                "{}: {}\n",
                idx.display(context),
                ty.display(context)
            ));
        }

        output.push_str("\nType Expression types:\n");
        for (idx, ty) in self.type_expr_types.iter() {
            output.push_str(&format!(
                "{}: {}\n",
                idx.display(context),
                ty.display(context)
            ));
        }

        output.push_str("\nLocal Defs:\n");
        for (key, ty) in self.value_symbols.iter() {
            output.push_str(&format!(
                "{}: {}\n",
                key.display(context),
                ty.display(context)
            ));
        }

        output.push_str("\nLocal Type Defs:\n");
        for (key, ty) in self.type_symbols.iter() {
            output.push_str(&format!(
                "{}: {}\n",
                key.display(context),
                ty.display(context)
            ));
        }

        output
    }
}

impl TypeDatabase {
    pub(crate) fn type_(&self, idx: Idx<Type>) -> &Type {
        &self.types[idx]
    }

    pub(super) fn get_expr_type(&self, idx: Idx<Expr>) -> Idx<Type> {
        self.expr_types[idx]
    }

    pub(super) fn set_expr_type(&mut self, idx: Idx<Expr>, ty: Idx<Type>) {
        self.expr_types.insert(idx, ty);
    }

    pub(super) fn get_type_expr_type(&self, idx: Idx<TypeExpr>) -> Idx<Type> {
        self.type_expr_types[idx]
    }

    pub(super) fn set_type_expr_type(&mut self, idx: Idx<TypeExpr>, ty: Idx<Type>) {
        self.type_expr_types.insert(idx, ty);
    }

    // todo - panics if called before type checking, move to another module?
    pub(super) fn get_value_symbol(&self, key: &ValueSymbol) -> Idx<Type> {
        self.value_symbols[key]
    }

    pub(super) fn insert_value_symbol(&mut self, key: ValueSymbol, ty: Idx<Type>) {
        self.value_symbols.insert(key, ty);
    }

    // todo - panics if called before type checking, move to another module?
    pub(super) fn get_type_symbol(&self, key: &TypeSymbol) -> Idx<Type> {
        self.type_symbols[key]
    }

    pub(super) fn insert_type_symbol(&mut self, key: TypeSymbol, ty: Idx<Type>) {
        self.type_symbols.insert(key, ty);
    }

    pub(super) fn alloc_type(&mut self, ty: Type) -> Idx<Type> {
        self.types.alloc(ty)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct TypeResult {
    ty: Idx<Type>,
    diagnostics: Vec<TypeDiagnostic>,
}

impl From<TypeResult> for Result<Idx<Type>, Vec<TypeDiagnostic>> {
    fn from(value: TypeResult) -> Self {
        if value.diagnostics.is_empty() {
            Ok(value.ty)
        } else {
            Err(value.diagnostics)
        }
    }
}

impl From<Idx<Type>> for TypeResult {
    fn from(ty: Idx<Type>) -> Self {
        TypeResult {
            ty,
            diagnostics: vec![],
        }
    }
}

impl From<&Idx<Type>> for TypeResult {
    fn from(ty: &Idx<Type>) -> Self {
        (*ty).into()
    }
}

impl TypeResult {
    fn new(type_database: &TypeDatabase) -> Self {
        Self {
            ty: type_database.core.unknown,
            diagnostics: vec![],
        }
    }

    fn from_diag(diagnostic: TypeDiagnostic, error_ty: Idx<Type>) -> Self {
        Self {
            ty: error_ty,
            diagnostics: vec![diagnostic],
        }
    }

    fn from_ty(ty: Idx<Type>) -> Self {
        Self {
            ty,
            diagnostics: vec![],
        }
    }

    pub fn is_ok(&self) -> bool {
        self.diagnostics.is_empty()
    }

    fn push_diag(&mut self, diagnostic: TypeDiagnostic) {
        self.diagnostics.push(diagnostic)
    }

    /// Checks the expression for the expected type, accumulating diagnostic(s)
    /// if not the expected type.
    // TODO: should this also set `self.ty` ?
    fn check(&mut self, expr: Idx<Expr>, expected: Idx<Type>, context: &mut Context) {
        self.push_result(check_expr(expr, expected, context))
    }

    pub fn push_result<T>(&mut self, result: Result<T, Vec<TypeDiagnostic>>) {
        if let Err(mut diagnostics) = result {
            self.diagnostics.append(&mut diagnostics);
        }
    }

    /// Chains two results together, applying the newer inferred type,
    /// or accumulating the diagnostics if these exist.
    pub fn chain(&mut self, mut other: TypeResult) {
        if other.is_ok() {
            self.ty = other.ty;
        } else {
            self.diagnostics.append(&mut other.diagnostics);
        }
    }

    pub fn diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics.into_iter().map(|d| d.into()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct CoreTypes {
    /// Sentinel for type has not yet been inferred
    pub unknown: Idx<Type>,

    /// Sentinel for error occurred during type inference
    pub error: Idx<Type>,

    /// Top type, all values are inhabitants
    pub top: Idx<Type>,

    /// Bottom type, has no inhabitants. Also known as `Never`
    pub bottom: Idx<Type>,

    /// Unit type, has only one inhabitant
    pub unit: Idx<Type>,

    /// Bool, union with variants `false | true`
    pub bool: Idx<Type>,

    /// Floating point number
    pub float: Idx<Type>,

    /// Integer number
    pub int: Idx<Type>,

    /// UTF-8 encoded string
    pub string: Idx<Type>,
}
