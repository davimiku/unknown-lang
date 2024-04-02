use crate::Module;

pub fn optimize(module: &mut Module) {
    // FUTURE:
    // optimizations that always happen
    remove_unreachable_blocks(module);

    // FUTURE:
    // optimizations for some opt level (2 levels, default and debug?)
    // add coverage markers?
    // add debug info?
}

/// Removes blocks that are unreachable
///
/// An unreachable block is defined as a block that no other blocks
/// have a Terminator with a "target" to this block. In other words,
/// the block has no predecessors.
fn remove_unreachable_blocks(module: &mut Module) {
    // TODO: is this unnecessary because we already have the predecessor Vec
    // and can just ignore the blocks without predecessors?
}
