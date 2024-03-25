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
mod scopes;
mod syntax;
#[cfg(test)]
mod tests;

use std::collections::HashMap;

pub use display::MirWrite;
use hir::{Context, Expr, Key};
use la_arena::Arena;
pub use syntax::{
    BasicBlock, BinOpKind, BlockParameters, Constant, FuncId, Function, Local, Operand, Place,
    Rvalue, Statement, SwitchIntTargets, Terminator, UnOp,
};
use util_macros::assert_matches;

use crate::builder::Builder;

pub fn construct(input: &str) -> (Module, Context) {
    let (hir_module, context) = hir::lower(input);
    assert_eq!(context.diagnostics, vec![]);
    let builder = Builder::new(hir_module.id, &context);

    let module = builder.construct_module(&hir_module, &context);
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

pub fn construct_script(input: &str) -> (Module, &hir::Context) {
    let (_, hir_context) = hir::lower_script(input);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles scripts as plugins
    // OK if it's restricted to CLI scripts only, probably
    let _: &'static _ = Box::leak(Box::new(hir_context));

    todo!("decide whether to abandon this entirely")
}

pub fn construct_function(input: &str) -> (Module, &hir::Context) {
    let (root, hir_context) = hir::lower_function(input);
    assert_eq!(hir_context.diagnostics, vec![]);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles functions as plugins
    // OK if it's restricted to tests only, probably
    let context: &'static _ = Box::leak(Box::new(hir_context));

    let mut builder = Builder::new(0, context);

    let root = context.expr(root);
    let func_group = assert_matches!(root, Expr::Function);
    let func = &func_group.overloads[0];

    builder.construct_function(func, None, None, context);
    (builder.build(), context)
}

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
