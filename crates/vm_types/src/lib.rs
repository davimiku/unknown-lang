//! Type wrappers used in the VM and the bytecode
//! to isolate and ensure correctness of byte mucking.

#[cfg(test)]
mod tests;
pub mod words;

pub use words::word_size_of;
use words::Word;

pub trait FromWordVec {
    fn from_vec(source: Vec<Word>) -> Self;
}

impl FromWordVec for () {
    fn from_vec(_: Vec<Word>) -> Self {}
}

/// VM representation of a language Bool
pub type VMBool = u64;

impl FromWordVec for VMBool {
    fn from_vec(source: Vec<Word>) -> Self {
        source[0].into()
    }
}

/// VM representation of a language Int
pub type VMInt = i64;

impl FromWordVec for VMInt {
    fn from_vec(source: Vec<Word>) -> Self {
        source[0].into()
    }
}

/// VM representation of a language Float
pub type VMFloat = f64;

impl FromWordVec for VMFloat {
    fn from_vec(source: Vec<Word>) -> Self {
        source[0].into()
    }
}
