//! Type wrappers used in the VM and the bytecode
//! to isolate and ensure correctness of byte mucking.

pub mod string;
#[cfg(test)]
mod tests;
pub mod words;

/// VM representation of a language Bool
pub type VMBool = u32;

/// VM representation of a language Int
pub type VMInt = i32;

/// VM representation of a language Float
pub type VMFloat = f64;