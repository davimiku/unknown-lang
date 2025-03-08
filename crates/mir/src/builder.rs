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

    /// Reverse mapping from a FuncId back to its name, usually for debugging
    pub function_names: HashMap<FuncId, Option<String>>,

    /// Tracks the blocks that `break` statements need to jump to, outside the current loop
    /// The length of this stack also indicates the current loop depth
    pub breaks_stack: Vec<Idx<BasicBlock>>,

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
        let mut scopes = ScopesStack::default();
        // insert false=0, true=1  ??
        // let local: Local = Local::from(value);
        // scopes.insert_local(local, symbol);
        // scopes.insert_local(local, symbol);
        let builder = Self {
            functions,
            module_id,
            current_function,
            current_block,
            function_names: Default::default(),
            entry_points: Default::default(),
            block_var_defs: Default::default(),
            functions_map: Default::default(),
            scopes,
            breaks_stack: Default::default(),
        };

        // builder.construct_local(ty, symbol, Mutability::Not)

        builder
    }
}

impl Builder {
    pub fn build(self) -> Module {
        Module {
            functions: self.functions,
            function_names: self.function_names,
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

    pub fn block(&self, idx: Idx<BasicBlock>) -> &BasicBlock {
        &self.current_function().blocks[idx]
    }

    pub fn block_mut(&mut self, idx: Idx<BasicBlock>) -> &mut BasicBlock {
        &mut self.current_function_mut().blocks[idx]
    }

    /// Returns an iterator to block indexes without a lifetime tied to
    /// `self`, by copying the indexes
    pub fn block_indexes(&self) -> Vec<Idx<BasicBlock>> {
        self.current_function()
            .blocks
            .iter()
            .map(|(idx, _)| idx)
            .collect()
    }

    pub fn current_block(&self) -> &BasicBlock {
        &self.current_function().blocks[self.current_block]
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        let current_block = self.current_block;
        &mut self.current_function_mut().blocks[current_block]
    }
}
