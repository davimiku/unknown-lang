use std::collections::{HashMap, VecDeque};

use hir::{FunctionExpr, Key, Type, TypeSymbol, ValueSymbol};
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

    pub functions_map: HashMap<(ValueSymbol, u32), FuncId>,

    /// Function bodies are kept as templates until a concrete instance is created at a callsite
    pub parameterized_functions: HashMap<(ValueSymbol, u32), ParameterizedFunctionTemplate>,

    /// Instance cache and worklist for monomorphization
    pub parameterized_instances: HashMap<ParameterizedInstanceKey, FuncId>,
    pub pending_parameterized_instances: VecDeque<PendingGenericInstance>,
    pub next_function_index: u32,

    /// HIR type views specialized for the function currently being lowered.
    pub expr_type_overrides: HashMap<Idx<hir::Expr>, Idx<Type>>,
    pub call_type_arg_overrides: HashMap<Idx<hir::Expr>, Box<[Idx<Type>]>>,
    pub value_type_overrides: HashMap<ValueSymbol, Idx<Type>>,

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ParameterizedInstanceKey {
    pub symbol: ValueSymbol,
    pub signature_index: u32,
    pub type_args: Box<[Idx<Type>]>,
}

#[derive(Debug, Clone)]
pub(crate) struct ParameterizedFunctionTemplate {
    pub function: FunctionExpr,
    pub name: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct PendingGenericInstance {
    pub func_id: FuncId,
    pub function: FunctionExpr,
    pub name: Option<String>,
    pub substitutions: Box<[(TypeSymbol, Idx<Type>)]>,
}

impl Builder {
    pub(crate) fn new(module_id: u32, context: &hir::Context) -> Self {
        let current_block = Idx::from_raw(u32::MAX.into());
        let current_function = Idx::from_raw(u32::MAX.into());
        let builder = Self {
            module_id,
            current_function,
            current_block,
            functions: Default::default(),
            function_names: Default::default(),
            entry_points: Default::default(),
            block_var_defs: Default::default(),
            functions_map: Default::default(),
            parameterized_functions: Default::default(),
            parameterized_instances: Default::default(),
            pending_parameterized_instances: Default::default(),
            next_function_index: 0,
            expr_type_overrides: Default::default(),
            call_type_arg_overrides: Default::default(),
            value_type_overrides: Default::default(),
            scopes: Default::default(),
            breaks_stack: Default::default(),
        };

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
