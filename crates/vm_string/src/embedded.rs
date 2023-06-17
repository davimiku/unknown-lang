use std::fmt::Debug;

use crate::{as_str::AsStr, VMString};

/// Maximum number of bytes that can be represented in a string
/// without heap allocation.
pub const MAX_EMBEDDED_LENGTH: usize = 7;

pub(crate) type EmbeddedBytes = [u8; MAX_EMBEDDED_LENGTH + 1];

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub struct EmbeddedVMString {
    /// Bytes of the string, where the first byte is the length
    pub(crate) bytes: EmbeddedBytes,
}

impl From<[u8; 8]> for EmbeddedVMString {
    fn from(bytes: [u8; 8]) -> Self {
        Self { bytes }
    }
}

pub(crate) fn new_embedded_string(len: u32, data: [u8; MAX_EMBEDDED_LENGTH]) -> VMString {
    let len = len as u8;
    let mut bytes = [0; 8];
    bytes[0] = len;
    unsafe {
        let dest = bytes.as_mut_ptr() as *mut u8;
        std::ptr::copy_nonoverlapping(data.as_ptr(), dest.add(1), len as usize);
    }

    EmbeddedVMString { bytes }.into()
}

impl EmbeddedVMString {
    #[inline]
    pub(super) fn length(&self) -> u32 {
        self.bytes[0].into()
    }

    pub(super) fn as_bytes(&self) -> &[u8] {
        // Safety: `self.len` is the exact length of the valid data in the slice
        unsafe { self.bytes.get_unchecked(1..(self.length() as usize + 1)) }
    }
}

impl AsStr for EmbeddedVMString {
    fn as_str(&self) -> &str {
        // Safety: bytes are always valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }
}

impl Debug for EmbeddedVMString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl From<EmbeddedVMString> for VMString {
    fn from(value: EmbeddedVMString) -> Self {
        VMString::Embedded(value)
    }
}

impl From<EmbeddedVMString> for [u8; 8] {
    fn from(value: EmbeddedVMString) -> Self {
        value.bytes
    }
}
