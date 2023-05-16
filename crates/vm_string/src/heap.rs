use std::fmt::Debug;
use std::rc::Rc;

use crate::{as_str::AsStr, VMString};

#[derive(PartialEq)]
pub struct HeapVMString {
    /// Reference counted string data
    pub(crate) ptr: Rc<String>,
}

// Constructors

impl From<[u8; 8]> for HeapVMString {
    fn from(value: [u8; 8]) -> Self {
        let ptr = usize::from_le_bytes(value) as *const String;

        // Safety: ptr was created from Rc::into_raw originally
        let ptr = unsafe { Rc::from_raw(ptr) };

        Self { ptr }
    }
}

pub(crate) fn new_heap_string<S>(s: S) -> VMString
where
    S: Into<String>,
{
    let s = s.into();
    HeapVMString { ptr: Rc::new(s) }.into()
}

impl From<HeapVMString> for VMString {
    fn from(value: HeapVMString) -> Self {
        VMString::Heap(value)
    }
}

// impl Clone for HeapVMString {
//     fn clone(&self) -> Self {
//         let len = self.len;
//         let raw_ptr = std::ptr::addr_of!(self.ptr);

//         // Safety: Necessary to avoid undefined behavior of directly
//         // casting an unaligned pointer from the packed struct.
//         let ptr = unsafe { std::ptr::read_unaligned(raw_ptr) };
//         let ptr = ptr.clone();
//         Self { len, ptr }
//     }
// }

// Non-Mutating Function

impl HeapVMString {
    #[inline]
    pub(super) fn length(&self) -> u32 {
        self.ptr.len() as u32
    }

    pub(super) fn as_bytes(&self) -> &[u8] {
        self.ptr.as_bytes()
    }
}

impl AsStr for HeapVMString {
    fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).unwrap()
    }
}

impl Debug for HeapVMString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// Destructors

impl From<HeapVMString> for [u8; 8] {
    fn from(value: HeapVMString) -> Self {
        let ptr_bytes: [u8; 8] = (Rc::into_raw(value.ptr) as usize).to_le_bytes();
        ptr_bytes
    }
}
