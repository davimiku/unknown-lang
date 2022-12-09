//! Scopes
//!
//! A scope controls the visibility of a binding. A binding may
//! be "in scope" if it exists in the current scope or a (recursive)
//! parent scope.
//!
//! Scopes are defined by the user, with the top-level scope being the
//! module scope. Even if a module is a child module, it does not inherit
//! the scope of its parent modules.
//!
//! Scopes are composed of definitions for variables and types. Conceptually,
//! scopes exist as a tree, but for performance the internal implementation is
//! a "flattened" tree in the form of a Vec with indexes to point to the parent.

use std::collections::HashMap;

use la_arena::Idx;

use crate::{LetBinding, Type};

#[derive(Debug, Default)]
pub(crate) struct Scope {
    // module scope is the top level (child modules do not inherit scope from their parent)
    // 0 is used as a sentinel value, the top level scope has a parent_idx of 0
    pub(crate) parent_idx: usize,

    local_defs: HashMap<String, Idx<LetBinding>>,

    type_defs: HashMap<String, Idx<Type>>,
}

impl Scope {
    pub(crate) fn get_local(&self, key: &str) -> Option<Idx<LetBinding>> {
        self.local_defs.get(key).copied()
    }

    pub(crate) fn get_type(&self, key: &str) -> Option<Idx<Type>> {
        self.type_defs.get(key).copied()
    }
}

#[derive(Debug)]
pub(crate) struct Scopes(Vec<Scope>);

impl Scopes {
    pub(crate) fn get(&self, idx: usize) -> Option<&Scope> {
        self.0.get(idx)
    }

    pub(crate) fn get_parent_idx(&self, idx: usize) -> usize {
        let scope = &self.0[idx];

        scope.parent_idx
    }

    /// Creates a ScopesIter starting at the given idx.
    pub(crate) fn iter_from(&self, idx: usize) -> ScopesIter {
        ScopesIter {
            curr: idx,
            scopes: self,
        }
    }

    /// Adds a new scope
    ///
    /// Returns the index of the added scope
    pub(crate) fn push(&mut self, parent_idx: usize) -> usize {
        self.0.push(Scope {
            parent_idx,
            ..Default::default()
        });

        self.0.len() - 1
    }

    pub(crate) fn insert_local_def(
        &mut self,
        scope_idx: usize,
        name: String,
        local_def_idx: Idx<LetBinding>,
    ) {
        let scope = &mut self.0[scope_idx];

        scope.local_defs.insert(name, local_def_idx);
    }
}

impl Default for Scopes {
    fn default() -> Self {
        // Begins with the sentinel at index 0
        Self(vec![Scope::default()])
    }
}

/// An iterator for scopes that walks upwards through
/// each parent until it reaches the top.
pub(crate) struct ScopesIter<'a> {
    curr: usize,
    scopes: &'a Scopes,
}

impl<'a> Iterator for ScopesIter<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr == 0 {
            // sentinel value: reached the top level scope
            return None;
        }
        let scope = self.scopes.get(self.curr).expect("valid current index");

        self.curr = scope.parent_idx;

        Some(scope)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_scopes() -> Scopes {
        let mut scopes = Scopes::default();

        // 0 (sentinel)
        // 1 (module)
        // | -> 2 -> 3
        // | -> 4
        // | -> 5 -> 6 -> 7
        scopes.push(0); // 1
        scopes.push(1); // 2
        scopes.push(2); // 3
        scopes.push(1); // 4
        scopes.push(1); // 5
        scopes.push(5); // 6
        scopes.push(6); // 7
        scopes
    }

    #[test]
    fn scopes_iter() {
        let scopes = make_scopes();

        // (starting index, expected parent indexes)
        let cases: Vec<(usize, Vec<usize>)> = vec![
            //
            (3, vec![2, 1, 0]),
            (4, vec![1, 0]),
            (7, vec![6, 5, 1, 0]),
        ];

        for (start, expected) in cases {
            let actual: Vec<_> = scopes
                .iter_from(start)
                .map(|scope| scope.parent_idx)
                .collect();

            assert_eq!(actual, expected);
        }
    }
}
