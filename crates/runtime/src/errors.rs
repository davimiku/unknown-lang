#[derive(Debug)]
pub enum AllocError {
    /// Allocation was requested, but the heap is at its maximum
    // not implemented yet - but intention is OOM to be recoverable
    OutOfMemory,

    /// Allocation size requested is too small or too large
    InvalidSize,

    /// Requested bytes are not word aligned
    Misalignment,
}
