//! Scopes
//!
//! A scope controls the visibility of a binding. A binding may
//! be "in scope" if it exists in the current scope or a (recursive)
//! parent scope.
//!
//! Scopes are defined by the user, with the top-level scope being the
//! module scope. A module boundary is always an encapsulation boundary.

use std::collections::HashMap;

use id_tree::{InsertBehavior, Node, Tree};

use crate::interner::Key;
use crate::type_expr::LocalTypeDefKey;
use crate::{Interner, LocalDefKey};

#[derive(Debug, Default)]
pub struct Scope {
    /// Associates a Name with the unique key for that local definition within the context
    ///
    /// TODO: handle multiple of the same Name in the same Scope (shadowed locals)
    local_defs: HashMap<Key, LocalDefKey>,

    local_type_defs: HashMap<Key, LocalTypeDefKey>,
}

impl Scope {
    fn display(&self, interner: &Interner) -> String {
        let mut s = String::new();
        s.push_str("local_defs: {\n");
        for (key, local_key) in &self.local_defs {
            s.push_str(&format!(
                "  {}: {}\n",
                interner.lookup(*key),
                local_key.display(interner)
            ));
        }
        s.push_str("}\n");
        s.push_str("local_type_defs: {\n");
        for (key, local_key) in &self.local_type_defs {
            s.push_str(&format!(
                "  {}: {}\n",
                interner.lookup(*key),
                local_key.display(interner)
            ));
        }
        s.push_str("}\n");

        s
    }
}

impl Scope {
    pub(crate) fn get_local(&self, key: Key) -> Option<LocalDefKey> {
        self.local_defs.get(&key).copied()
    }

    pub(crate) fn get_local_type(&self, key: Key) -> Option<LocalTypeDefKey> {
        self.local_type_defs.get(&key).copied()
    }
}

/// Holds the tree of Scope structs
#[derive(Debug)]
pub(crate) struct Scopes {
    /// Data of the scopes
    tree: id_tree::Tree<Scope>,

    /// Useful for iterating later
    root_id: id_tree::NodeId,

    /// Mutable id ofr
    current_node_id: id_tree::NodeId,

    /// Local definition counts
    // TODO: separate map for terms and types?
    local_counts: HashMap<Key, u32>,
}

impl Scopes {
    pub fn display(&self, interner: &Interner) -> String {
        let mut output = String::new();
        for node_id in self.tree.traverse_pre_order_ids(&self.root_id).unwrap() {
            output.push_str(&format!("===scope {:?}===\n", node_id));

            let node = self.tree.get(&node_id).unwrap();
            output.push_str(&node.data().display(interner));
            output.push('\n');
        }
        output
    }
}

// Non-Mutating functions
impl Scopes {
    pub(crate) fn find_local(&self, name: Key) -> Option<LocalDefKey> {
        self.current().get_local(name).or(self
            .tree
            .ancestors(&self.current_node_id)
            .unwrap()
            .find_map(|scope| scope.data().get_local(name)))
    }

    pub(crate) fn find_local_type(&self, name: Key) -> Option<LocalTypeDefKey> {
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
impl Scopes {
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

    /// Inserts a Name for a local def into the current scope and returns
    /// a key that uniquely identifies that particular local.
    pub(crate) fn insert_local(&mut self, name: Key) -> LocalDefKey {
        let new_count = self.increment_count(name);

        let key: LocalDefKey = (name, new_count - 1).into();
        self.current_mut().local_defs.insert(name, key);

        key
    }

    pub(crate) fn insert_local_type(&mut self, name: Key) -> LocalTypeDefKey {
        let new_count = self.increment_count(name);

        let key: LocalTypeDefKey = (name, new_count - 1).into();
        self.current_mut().local_type_defs.insert(name, key);

        key
    }

    /// Adds one to the count for a local and returns the **new** count.
    fn increment_count(&mut self, name: Key) -> u32 {
        let new_count = self
            .local_counts
            .entry(name)
            .and_modify(|count| *count += 1)
            .or_insert(1);

        *new_count
    }
}

impl Scopes {
    pub(crate) fn new() -> Self {
        let root = Node::new(Scope::default());
        let mut tree = Tree::new();
        let root = tree.insert(root, InsertBehavior::AsRoot).unwrap();
        Self {
            tree,
            root_id: root.clone(),
            current_node_id: root,
            local_counts: HashMap::default(),
        }
    }
}
