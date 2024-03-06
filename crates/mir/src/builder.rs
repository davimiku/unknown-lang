// TODO: decide if a Builder will be one-per-module? for later parallelization
// for "script mode" or "function mode" there would be no parallelization

use std::collections::VecDeque;

use la_arena::{Arena, ArenaMap, Idx};

use crate::{scopes::ScopesStack, BasicBlock, BlockQueueItem, Function, Local, Place, Program};

#[derive(Debug)]
pub struct Builder {
    /// Functions that have been created by this Builder
    functions: Arena<Function>,

    /// Blocks that have been created but yet to be constructed
    /// (block_to_construct, hir::Expr::Block, Option<goto_terminator_block>)
    ///
    /// ex. with if/else branch, we initialize the empty blocks
    /// for each branch, and supply the destination block where those
    /// branches will reconvene.
    // pub block_queue: VecDeque<BlockQueueItem>,
    pub block_var_defs: ArenaMap<Idx<Local>, Idx<BasicBlock>>,

    // invariant: current_block *must* belong to the current_function
    pub current_function: Idx<Function>,
    pub current_block: Idx<BasicBlock>,

    /// The final statement of the current block is constructed as
    /// an assignment to this Place.
    // pub assign_to: Option<Place>,

    /// When constructing a block with no natural terminator, this value will
    /// be used as the Jump terminator. For example, when the branches of if-else
    /// join back together.
    // pub jump_to: Option<Idx<BasicBlock>>,

    /// Tracks scopes while constructing the MIR. When a scope is entered,
    /// a new Scope is pushed on here. This tracks the current statement counter
    /// so that when a scope is popped back into an earlier scope, the statements
    /// can resume being constructed.
    pub scopes: ScopesStack,
}

impl Default for Builder {
    fn default() -> Self {
        let func = Function::default();
        let current_block = func.entry_block();
        let mut functions = Arena::new();
        let current_function = functions.alloc(func);
        Self {
            functions,
            current_function,
            current_block,
            // block_queue: Default::default(),
            block_var_defs: Default::default(),
            // assign_to: Default::default(),
            // jump_to: Default::default(),
            scopes: Default::default(),
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

    pub fn current_block(&self) -> &BasicBlock {
        &self.current_function().blocks[self.current_block]
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        let current_block = self.current_block;
        &mut self.current_function_mut().blocks[current_block]
    }
}
