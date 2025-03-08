use la_arena::{ArenaMap, Idx};

use crate::BasicBlock;

#[derive(Debug, Clone)]
pub struct Predecessors {
    inner: ArenaMap<Idx<BasicBlock>, Vec<Idx<BasicBlock>>>,
}

impl Default for Predecessors {
    fn default() -> Self {
        let mut inner = ArenaMap::new();
        inner.insert(Idx::from_raw(0.into()), vec![]);
        Self { inner }
    }
}

impl Predecessors {
    pub fn add(&mut self, predecessor: Idx<BasicBlock>, successor: Idx<BasicBlock>) {
        self.inner
            .entry(successor)
            .and_modify(|predecessors| predecessors.push(predecessor))
            .or_insert_with(|| vec![predecessor]);
    }

    pub fn get(&self, key: Idx<BasicBlock>) -> &[Idx<BasicBlock>] {
        &self.inner[key]
    }

    /// Returns whether a block has predecessors
    ///
    /// The entry block is considered as having predecessors, because the entry
    /// block receives its parameters from the call arguments
    pub fn has_predecessors(&self, block: Idx<BasicBlock>) -> bool {
        if block.into_raw().into_u32() == 0 {
            true
        } else {
            self.inner
                .get(block)
                .is_some_and(|predecessors| !predecessors.is_empty())
        }
    }

    /// Returns all blocks with no predecessors
    ///
    /// This will never include the entry block, the entry block is considered
    /// as having predecessors, because the entry block receives its parameters
    /// from the call arguments
    pub fn with_no_predecessors(&self) -> Vec<Idx<BasicBlock>> {
        self.inner
            .iter()
            .filter_map(|(idx, ..)| self.has_predecessors(idx).then_some(idx))
            .collect()
    }
}
