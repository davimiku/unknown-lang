//! Scopes
//!
//! A scope controls the visibility of a definition. A definition may
//! be "in scope" if it exists in the current scope or a (recursive)
//! parent scope.
//!
//! Scopes are defined by the user, with the top-level scope being the
//! module scope. A module boundary is always an encapsulation boundary.

use std::collections::HashMap;

use id_tree::{InsertBehavior, Node, Tree};

use crate::interner::Key;
use crate::lowering_context::ContextDisplay;
use crate::type_expr::TypeSymbol;
use crate::{Context, ValueSymbol};

#[derive(Debug, Default)]
pub struct Scope {
    /// Associates the name with the unique id for the value
    ///
    /// Note: in the case of shadowed values within this scope, this
    /// map tracks the most recent entry of that name
    // TODO: should we use a map that efficiently can hold multiple values?
    // or maybe it holds a SmallVec ?
    values: HashMap<Key, ValueSymbol>,

    /// Associates the name with the unique id for the type
    types: HashMap<Key, TypeSymbol>,
}

impl ContextDisplay for Scope {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        s.push_str("local_values: {\n");
        for (key, local_key) in &self.values {
            s.push_str(&format!(
                "  {}: {}\n",
                context.lookup(*key),
                local_key.display(context)
            ));
        }
        s.push_str("}\n");
        s.push_str("local_types: {\n");
        for (key, local_key) in &self.types {
            s.push_str(&format!(
                "  {}: {}\n",
                context.lookup(*key),
                local_key.display(context)
            ));
        }
        s.push_str("}\n");

        s
    }
}

impl Scope {
    pub(crate) fn get_local(&self, key: Key) -> Option<ValueSymbol> {
        self.values.get(&key).copied()
    }

    pub(crate) fn get_local_type(&self, key: Key) -> Option<TypeSymbol> {
        self.types.get(&key).copied()
    }
}

/// Holds the tree of Scope structs for a given module
#[derive(Debug)]
pub(crate) struct ModuleScopes {
    /// Data of the scopes
    tree: id_tree::Tree<Scope>,

    /// Useful for iterating later
    root_id: id_tree::NodeId,

    /// Mutable id of the current node
    current_node_id: id_tree::NodeId,

    /// Running total of the values defined in this scope
    value_count: u32,

    /// Current id for defined value variables for the purpose
    /// of assigning a unique id to each type variable
    // value_ids: SymbolMap,

    /// Running total of the types defined in this scope
    type_count: u32,

    /// Current id for defined type variables for the purpose
    /// of assigning a unique id to each type variable
    // type_ids: SymbolMap,

    /// Unique id of the current module being compiled
    module_id: u32,
}

impl ContextDisplay for ModuleScopes {
    fn display(&self, context: &Context) -> String {
        let mut output = String::new();
        output.push_str(&format!("=== module {:?} ===\n", self.module_id));
        for node_id in self.tree.traverse_pre_order_ids(&self.root_id).unwrap() {
            output.push_str(&format!("===scope {:?} ===\n", node_id));

            let node = self.tree.get(&node_id).unwrap();
            output.push_str(&node.data().display(context));
            output.push('\n');
        }
        output
    }
}

impl ContextDisplay for Vec<ModuleScopes> {
    fn display(&self, context: &Context) -> String {
        let mut output = String::new();
        output.push_str("{{\n");
        for module_scopes in self {
            output.push_str(&module_scopes.display(context));
        }
        output.push_str("}}\n");
        output
    }
}

// Non-Mutating functions
impl ModuleScopes {
    /// Finds the symbol for the given name in the "value" namespace
    /// by recursively searching up from the current scope.
    pub(crate) fn find_value(&self, name: Key) -> Option<ValueSymbol> {
        self.current().get_local(name).or(self
            .tree
            .ancestors(&self.current_node_id)
            .unwrap()
            .find_map(|scope| scope.data().get_local(name)))
    }

    /// Finds the symbol for the given name in the "type" namespace
    /// by recursively searching up from the current scope.
    pub(crate) fn find_type(&self, name: Key) -> Option<TypeSymbol> {
        self.current().get_local_type(name).or(self
            .tree
            .ancestors(&self.current_node_id)
            .unwrap()
            .find_map(|scope| scope.data().get_local_type(name)))
    }

    fn current(&self) -> &Scope {
        self.tree.get(&self.current_node_id).unwrap().data()
    }
}

// Mutating functions
impl ModuleScopes {
    pub(crate) fn push(&mut self) {
        let new_node = Node::new(Scope::default());
        let new_id = self
            .tree
            .insert(new_node, InsertBehavior::UnderNode(&self.current_node_id))
            .unwrap();

        self.current_node_id = new_id;
    }

    pub(crate) fn pop(&mut self) {
        let current_node = self.tree.get(&self.current_node_id).unwrap();

        let parent_id = current_node.parent().unwrap();

        self.current_node_id = parent_id.clone();
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.tree.get_mut(&self.current_node_id).unwrap().data_mut()
    }

    /// Inserts a name for a variable def into the current scope and returns
    /// a key that uniquely identifies that particular variable.
    pub(crate) fn insert_value(&mut self, name: Key) -> ValueSymbol {
        let symbol = ValueSymbol::new(self.module_id, self.value_count);
        self.value_count += 1;

        self.current_mut().values.insert(name, symbol);
        symbol
    }

    pub(crate) fn insert_type(&mut self, name: Key) -> TypeSymbol {
        let symbol = TypeSymbol::new(self.module_id, self.type_count);
        self.type_count += 1;

        self.current_mut().types.insert(name, symbol);
        symbol
    }
}

impl ModuleScopes {
    pub(crate) fn new(module_id: u32) -> Self {
        let root = Node::new(Scope::default());
        let mut tree = Tree::new();
        let root = tree.insert(root, InsertBehavior::AsRoot).unwrap();
        Self {
            tree,
            root_id: root.clone(),
            current_node_id: root,
            value_count: 0,
            // value_ids: Default::default(),
            type_count: 0,
            // type_ids: Default::default(),
            module_id,
        }
    }
}
