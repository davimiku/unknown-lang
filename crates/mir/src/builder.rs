use std::collections::HashMap;

use hir::{Key, ValueSymbol};
use la_arena::{Arena, ArenaMap, Idx};

use crate::scopes::ScopesStack;
use crate::syntax::FuncId;
use crate::{BasicBlock, Function, Local, Module};

/// Builder constructs the Mid-Level Representation (MIR) for a **module**.
///
/// This MIR is a Control Flow Graph where nodes are represented by "Basic Blocks"
/// and edges are represented by the terminators that point between the "bottom"
/// of one Basic Block to the "top" of one or more other Basic Blocks.
#[derive(Debug)]
pub struct Builder {
    /// Functions that have been created by this Builder
    pub(crate) functions: Arena<Function>,

    pub(crate) entry_points: HashMap<Key, FuncId>,

    pub(crate) module_id: u32,
    /// Blocks that have been created but yet to be constructed
    /// (block_to_construct, hir::Expr::Block, Option<goto_terminator_block>)
    ///
    /// ex. with if/else branch, we initialize the empty blocks
    /// for each branch, and supply the destination block where those
    /// branches will reconvene.
    // pub block_queue: VecDeque<BlockQueueItem>,

    /// Variables that have been defined in this block
    ///
    /// This is used for determining the block parameters, which are all
    /// locals used in this block that *weren't* defined in this block
    pub block_var_defs: ArenaMap<Idx<Local>, Idx<BasicBlock>>,

    // invariant: current_block *must* belong to the current_function
    pub current_function: Idx<Function>,
    pub current_block: Idx<BasicBlock>,

    pub functions_map: HashMap<ValueSymbol, Vec<FuncId>>,

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
    /// can resume being constructed at the same point.
    pub scopes: ScopesStack,
}

impl Builder {
    pub(crate) fn new(module_id: u32, context: &hir::Context) -> Self {
        let mut functions = Arena::new();
        // {
        //     let mut print_string = Function::default();
        //     let key = context.interner.core_keys().print;
        //     let symbol = context.find_value(key).unwrap();
        //     print_string.name = Some((key, symbol));
        //     print_string.params = vec![];
        //     functions.alloc(print_string);
        // }

        let current_block = Idx::from_raw(u32::MAX.into());
        let current_function = Idx::from_raw(u32::MAX.into());
        Self {
            functions,
            module_id,
            current_function,
            current_block,
            entry_points: Default::default(),
            block_var_defs: Default::default(),
            functions_map: Default::default(),
            scopes: Default::default(),
        }
    }
}

impl Builder {
    pub fn build(self) -> Module {
        Module {
            functions: self.functions,
            entry_points: self.entry_points,
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
