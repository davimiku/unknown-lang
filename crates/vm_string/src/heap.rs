use std::fmt::Debug;
use std::rc::Rc;

use crate::{as_str::AsStr, VMString};

#[derive(Clone, PartialEq)]
pub struct HeapVMString {
    /// Reference counted string data
    pub(crate) ptr: Rc<String>,
}

// Constructors

impl HeapVMString {
    /// Creates an instance from raw bytes that had been previously copied.
    ///
    /// This assumes that there is at least one strong count to this allocation
    /// already, and will increase the strong count unlike `from_raw`.
    pub fn from_copy(value: [u8; 8]) -> Self {
        let ptr = usize::from_le_bytes(value) as *const String;

        // Safety: ptr is a bit-for-bit copy of a ptr originally created by `Rc::into_raw`
        unsafe { Rc::increment_strong_count(ptr) };

        // Safety: strong count has been increased to account for the fact that this will
        // be another Rc to an existing allocation.
        let ptr = unsafe { Rc::from_raw(ptr) };

        Self { ptr }
    }

    /// Creates an instance from raw_bytes that had been previously created
    /// by calling `into_raw`.
    ///
    /// This does not increase the strong count of this allocation, unlike `from_copy`.
    pub fn from_raw(value: [u8; 8]) -> Self {
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
