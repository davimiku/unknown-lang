//! Scopes
//!
//! A scope controls the visibility of a binding. A binding may
//! be "in scope" if it exists in the current scope or a (recursive)
//! parent scope.
//!
//! Scopes are defined by the user, with the top-level scope being the
//! module scope. A module boundary is always an encapsulation boundary.
//!
//! Scopes are composed of definitions for variables and types. Conceptually,
//! scopes exist as a tree, but for performance the internal implementation is
//! a "flattened" tree in the form of a Vec with indexes to point to the parent.
//!
//! TODO: Find a crate that implements this as a general
//! solution, which is likely to be more efficient and ergonomic than this.
//! ex. https://lib.rs/crates/id_tree or https://lib.rs/crates/indextree

use std::collections::HashMap;

use id_tree::{InsertBehavior::*, Node, Tree};

use crate::interner::Key;
use crate::LocalDefKey;

#[derive(Debug, Default)]
pub struct Scope {
    // module scope is the top level (child modules do not inherit scope from their parent)
    // 0 is used as a sentinel value, the top level scope has a parent_idx of 0
    // TODO: consider using a space-optimized Option<usize> (usize::MAX is the None value) such as
    // https://docs.rs/optional/latest/optional/
    // pub(crate) parent_idx: usize,
    /// Associates a Name with the unique key for that local definition within the context
    ///
    /// TODO: handle multiple of the same Name in the same Scope (shadowed locals)
    name_counts: HashMap<Key, LocalDefKey>,
}

impl Scope {
    pub(crate) fn get_local(&self, key: Key) -> Option<LocalDefKey> {
        self.name_counts.get(&key).copied()
    }
}

/// Holds the tree of Scope structs, stored in a Vec internally.
#[derive(Debug)]
pub(crate) struct Scopes {
    tree: id_tree::Tree<Scope>,

    current_node_id: id_tree::NodeId,

    /// Local definition counts
    pub(crate) local_counts: HashMap<Key, u32>,
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

    fn current(&self) -> &Scope {
        self.tree.get(&self.current_node_id).unwrap().data()
    }

    // fn old_current_mut(&mut self) -> &mut Scope {
    //     &mut self.scopes[self.current_idx]
    // }

    // fn find_scope_of(&self, name: &Name) -> Option<&Scope> {
    //     self.into_iter().find(|scope| scope.has_local(name))
    // }

    // fn get(&self, idx: usize) -> Option<&Scope> {
    //     self.scopes.get(idx)
    // }

    // unsafe fn get_unchecked(&self, idx: usize) -> &Scope {
    //     self.scopes.get_unchecked(idx)
    // }

    // pub(crate) fn get_parent_idx(&self, idx: usize) -> usize {
    //     let scope = &self.scopes[idx];

    //     scope.parent_idx
    // }

    // Creates a ScopesIter starting at the given idx.
    // pub(crate) fn iter_from(&self, idx: usize) -> ScopesIter {
    //     ScopesIter {
    //         curr: idx,
    //         scopes: self,
    //     }
    // }
}

// Mutating functions
impl Scopes {
    pub(crate) fn push(&mut self) {
        let new_node = Node::new(Scope::default());
        let new_id = self
            .tree
            .insert(new_node, UnderNode(&self.current_node_id))
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
    pub(crate) fn insert_local_def(&mut self, name: Key) -> LocalDefKey {
        let new_count = self.increment_count(name);

        let key: LocalDefKey = (name, new_count - 1).into();
        self.current_mut().name_counts.insert(name, key);

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

impl Default for Scopes {
    fn default() -> Self {
        // let root =
        let root = Node::new(Scope::default());
        let mut tree = Tree::new();
        let root = tree.insert(root, AsRoot).unwrap();
        Self {
            tree,
            current_node_id: root,
            // scopes: vec![Scope::default()],
            // current_idx: 0,
            local_counts: HashMap::default(),
        }
    }
}

// impl<'a> IntoIterator for &'a Scopes {
//     type Item = &'a Scope;

//     type IntoIter = ScopesIter<'a>;

//     fn into_iter(self) -> Self::IntoIter {
//         ScopesIter {
//             curr: self.current_idx,
//             scopes: self,
//         }
//     }
// }

// An iterator for scopes that walks upwards through
// each parent until it reaches the top.
// pub(crate) struct ScopesIter<'a> {
//     curr: usize,
//     scopes: &'a Scopes,
// }

// impl<'a> Iterator for ScopesIter<'a> {
//     type Item = &'a Scope;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.curr == 0 {
//             // sentinel value: reached the top level scope
//             return None;
//         }
//         let scope = self.scopes.get(self.curr).expect("valid current index");

//         self.curr = scope.parent_idx;

//         Some(scope)
//     }
// }
