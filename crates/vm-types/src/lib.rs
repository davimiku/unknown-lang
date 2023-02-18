//! Type wrappers used in the VM and the bytecode
//! to isolate and ensure correctness of byte mucking.

#[cfg(test)]
mod tests;
pub mod words;
pub mod xstring;

pub type XBool = u32;
pub type XInt = i32;
pub type XFloat = f64;
