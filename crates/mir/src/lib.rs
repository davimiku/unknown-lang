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

pub use display::MirWrite;
use hir::{Context, Expr};
use la_arena::{Arena, Idx};
pub use syntax::{
    BasicBlock, BinOp, BlockParameters, Constant, Function, Local, Operand, Place, Rvalue,
    Statement, Terminator, UnOp,
};

use crate::builder::Builder;

pub fn construct(root: Idx<Expr>, context: &Context) -> (Program, &Context) {
    assert_eq!(context.diagnostics, vec![]);
    let mut builder = Builder::default();

    let root = context.expr(root);

    if let Expr::Module(exprs) = root {
        todo!("implement module constructing")
    } else if let Expr::Function(func) = root {
        builder.construct_function(func, context);
    } else {
        panic!("Compiler Bug (MIR): expected a Module or Function at the root")
    };

    let program = builder.build();
    (program, context)
}

pub fn construct_script(input: &str) -> (Program, &hir::Context) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Script);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles scripts as plugins
    // OK if it's restricted to CLI scripts only, probably
    let hir_context: &'static _ = Box::leak(Box::new(hir_context));

    construct(root, hir_context)
}

pub fn construct_function(input: &str) -> (Program, &hir::Context) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Function);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles functions as plugins
    // OK if it's restricted to tests only, probably
    let hir_context: &'static _ = Box::leak(Box::new(hir_context));

    construct(root, hir_context)
}

#[derive(Debug)]
pub struct Program {
    functions: Arena<Function>,
    // TODO: eventually there could be multiple entry points that
    // would be compiled to separate CLIF functions
    // entry_function: Idx<Function>,
    // TODO: consts, in normal mode you should be able to set constants
    // and call constant functions at the module top-level
}

impl Program {
    // FIXME: Assumes that the first function is main which is true for
    // "script mode" or "function mode" but not normal/module mode
    pub fn main(&self) -> &Function {
        self.functions.values().next().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct BlockQueueItem {
    to_build: Idx<BasicBlock>,
    block_expr: Idx<Expr>,
    assign_to: Option<Place>,
    jump_to: Option<Idx<BasicBlock>>,
}
