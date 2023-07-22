//! Type wrappers used in the VM and the bytecode
//! to isolate and ensure correctness of byte mucking.

#[cfg(test)]
mod tests;
pub mod words;

use std::rc::Rc;

pub use words::word_size_of;
use words::{DWord, Word};

/// VM representation of a language Bool
pub type VMBool = u64;

/// VM representation of a language Int
pub type VMInt = i64;

/// VM representation of a language Float
pub type VMFloat = f64;

/// VM Representation of a language Array
pub struct VMArray {
    /// Pointer to the array data
    pub ptr: Rc<Vec<Word>>,

    /// Length of the array, in elements
    pub len: u32,

    /// Size of each element, in words
    pub el_size: u32,
}

impl From<VMArray> for DWord {
    fn from(value: VMArray) -> Self {
        let ptr = Rc::into_raw(value.ptr) as u64;
        let len_size: Word = (value.len, value.el_size).into();
        let words: [Word; 2] = [ptr.into(), len_size];
        words.into()
    }
}

impl From<DWord> for VMArray {
    fn from(value: DWord) -> Self {
        let [ptr, len_size]: [Word; 2] = value.into();
        let (len, el_size) = len_size.into();
        let ptr = usize::from_le_bytes(ptr.into()) as *const Vec<Word>;
        let ptr = unsafe { Rc::from_raw(ptr) };

        VMArray { ptr, len, el_size }
    }
}
