//! The Mid-Level Intermediate Representation (MIR) is a
//! control-flow graph (CFG).
//!
//! The primary inspiration of this representation is the [Rust MIR].
//!
//! A control-flow graph is primarily composed of [Basic Block]s that are
//! connected by edges. A Basic Block contains a list of [Statement]s and
//! is ended by a single [Terminator].
//!
//! Each Statement has only a single possible successor, which could be
//! either another Statement or the Terminator of that block if it was
//! the final Statement in the list. A Terminator may have multiple
//! successors. Going to another Basic Block is one of the kinds of
//! Terminator.
//!
//! [Rust MIR]: https://rustc-dev-guide.rust-lang.org/mir/index.html
//! [Basic Block]: crate::syntax::BasicBlock
//! [Statement]: crate::syntax::Statement
//! [Terminator]: crate::syntax::Terminator

mod builder;
mod construct;
mod display;
mod optimize;
mod predecessors;
mod scopes;
mod syntax;
#[cfg(test)]
mod tests;

use std::collections::HashMap;

pub use display::MirWrite;
use hir::{Context, ContextDisplay, Key};
use la_arena::Arena;
pub use syntax::{
    BasicBlock, BinOpKind, BlockParameters, BlockTarget, BranchIntTargets, Constant, FuncId,
    Function, Local, Operand, Place, Rvalue, Statement, Terminator, UnOp,
};

use crate::{builder::Builder, optimize::optimize};

pub fn construct(input: &str) -> (Module, Context) {
    let (hir_module, context) = hir::lower(input);
    if !context.diagnostics.is_empty() {
        for diagnostic in context.diagnostics.iter() {
            eprintln!("{}", diagnostic.display(&context));
        }
        assert_eq!(context.diagnostics, vec![]);
    }
    let builder = Builder::new(hir_module.id, &context);

    let mut module = builder.construct_module(&hir_module, &context);

    optimize(&mut module);
    (module, context)
}

pub fn construct_module<'hir>(
    hir_module: &'hir hir::Module,
    context: &'hir Context,
) -> (Module, &'hir Context) {
    assert_eq!(context.diagnostics, vec![]);
    let builder = Builder::new(hir_module.id, context);

    let module = builder.construct_module(hir_module, context);

    (module, context)
}

/// Data of the Mid Level Representation (MIR) for a module
///
/// This mainly contains the Control Flow Graph (CFG) and other
/// supplemental data.
#[derive(Debug)]
pub struct Module {
    /// Control Flow Graph (CFG) representation of functions in this module
    pub functions: Arena<Function>,

    pub function_names: HashMap<FuncId, Option<String>>,

    /// Map of functions that are entry points in this module
    ///
    /// It is valid for this to be empty - most modules would not have
    /// any entry points.
    pub entry_points: HashMap<Key, FuncId>,
    // TODO: consts, in normal mode you should be able to set constants
    // and call constant functions at the module top-level
}

impl Module {
    pub fn main(&self, context: &Context) -> Option<FuncId> {
        let main_key = context.interner.core_keys().main;
        self.entry_points.get(&main_key).copied()
    }
}
