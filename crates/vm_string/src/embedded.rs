use std::fmt::Debug;

use bytemuck::cast;

use crate::{as_str::AsStr, VMString};

/// Maximum number of bytes that can be represented in a string
/// without heap allocation.
pub const MAX_EMBEDDED_LENGTH: usize = 8;

pub(crate) type EmbeddedBytes = [u8; MAX_EMBEDDED_LENGTH];

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub struct EmbeddedVMString {
    /// Length of the string
    pub(crate) len: u32,

    /// Bytes of the string
    pub(crate) bytes: EmbeddedBytes,
}

impl From<[u8; 12]> for EmbeddedVMString {
    fn from(value: [u8; 12]) -> Self {
        let split: [[u8; 4]; 3] = cast(value);
        let len = u32::from_le_bytes(split[0]);
        let bytes = cast([split[1], split[2]]);

        Self { len, bytes }
    }
}

pub(crate) fn new_embedded_string(len: u32, bytes: EmbeddedBytes) -> VMString {
    EmbeddedVMString { len, bytes }.into()
}

impl EmbeddedVMString {
    #[inline]
    pub(super) fn length(&self) -> u32 {
        self.len
    }

    pub(super) fn as_bytes(&self) -> &[u8] {
        // Safety: `self.len` is the exact length of the valid data in the slice
        unsafe { self.bytes.get_unchecked(0..(self.len as usize)) }
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

impl From<EmbeddedVMString> for [u8; 12] {
    fn from(value: EmbeddedVMString) -> Self {
        let len_bytes: [u8; 4] = value.len.to_le_bytes();
        let data_bytes: [[u8; 4]; 2] = cast(value.bytes);
        bytemuck::cast([len_bytes, data_bytes[0], data_bytes[1]])
    }
}
