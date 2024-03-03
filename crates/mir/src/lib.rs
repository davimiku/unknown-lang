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

mod construct;
mod display;
mod syntax;
#[cfg(test)]
mod tests;

pub use display::MirWrite;
use hir::Expr;
use la_arena::{Arena, Idx};
pub use syntax::{
    BasicBlock, BinOp, BlockParameters, Constant, Function, Local, Operand, Place, Rvalue,
    Statement, Terminator, UnOp,
};

pub fn construct(root: Idx<Expr>, context: &hir::Context) -> (Program, &hir::Context) {
    assert_eq!(context.diagnostics, vec![]);
    let mut builder = Builder::default();

    let root = context.expr(root);

    if let Expr::Module(exprs) = root {
        todo!("implement module constructing")
    } else if let Expr::Function(func) = root {
        builder.construct_function(func, context);
    } else {
        panic!("Compiler Bug (MIR): expected a Module or Function at the root")
    }

    let program = builder.build();
    (program, context)
}

pub fn construct_script(input: &str) -> (Program, &hir::Context) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Script);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles scripts as plugins
    // OK if it's restricted to CLI scripts only, probably
    let hir_context = Box::leak(Box::new(hir_context));

    construct(root, hir_context)
}

pub fn construct_function(input: &str) -> (Program, &hir::Context) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Function);

    // I solemnly swear I am up to no good
    // FIXME: This definitely does not work in a language server or
    // compiler server that JIT compiles functions as plugins
    // OK if it's restricted to tests only, probably
    let hir_context: &'static mut hir::Context = Box::leak(Box::new(hir_context));

    construct(root, hir_context)
}

// TODO: decide if a Builder will be one-per-module? for later parallelization
// for "script mode" or "function mode" there would be no parallelization

#[derive(Debug)]
pub struct Builder {
    /// Functions that have been created by this Builder
    functions: Arena<Function>,

    /// Count of locals in the current function
    ///
    /// Locals include the return value, function parameters,
    /// user-defined locals, and compiler-created locals ("temps")
    local_count: u32,

    /// Current (mutable) scope level tracked by the builder throughout the function
    /// Zero is the top-level function scope
    scope_depth: u32,

    // invariant: the current_block must belong to the current_function
    current_function: Idx<Function>,
    current_block: Idx<BasicBlock>,

    /// Tracks whether the current statement being built should assign
    /// to the return value of the function being built
    /// TODO: check rustc, perhaps for a better way to track (enum?)
    tail_assign_place: Option<Place>,
}

impl Default for Builder {
    fn default() -> Self {
        let func = Function::default();
        let initial_block = func.entry_block();
        let mut functions = Arena::new();
        let initial_function = functions.alloc(func);
        Self {
            functions,
            current_function: initial_function,
            current_block: initial_block,
            local_count: 0,
            scope_depth: 0,
            tail_assign_place: None,
        }
    }
}

impl Builder {
    // TODO: if multiple builders will run in parallel, this can't return
    // the whole program, it would have to be Module or something like that
    pub fn build(self) -> Program {
        Program {
            functions: self.functions,
        }
    }

    pub fn function_mut(&mut self, idx: Idx<Function>) -> &mut Function {
        &mut self.functions[idx]
    }

    pub fn current_function(&self) -> &Function {
        &self.functions[self.current_function]
    }

    pub fn current_function_mut(&mut self) -> &mut Function {
        &mut self.functions[self.current_function]
    }

    pub fn block_mut(&mut self, idx: Idx<BasicBlock>) -> &mut BasicBlock {
        &mut self.current_function_mut().blocks[idx]
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        let current_block = self.current_block;
        &mut self.current_function_mut().blocks[current_block]
    }
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
