use std::{fmt, rc::Rc};

use as_str::AsStr;
use bytemuck::cast;
use embedded::{new_embedded_string, EmbeddedVMString, MAX_EMBEDDED_LENGTH};
use heap::{new_heap_string, HeapVMString};
use vm_types::words::DWord;

mod add;
mod as_str;
mod embedded;
mod heap;
#[cfg(test)]
mod tests;

/// Representation of a String value in the bytecode, independent of its allocation
#[repr(C)]
#[derive(PartialEq)]
pub enum VMString {
    /// Strings below a certain size are stored directly on the stack
    Embedded(EmbeddedVMString),

    /// Strings are allocated on the heap and reference counted
    Heap(HeapVMString),
}

// TODO: is there a way to derive this?
const EMBEDDED_DISCRIMINANT: u32 = 0;
const HEAP_DISCRIMINANT: u32 = 1;

type VMStringBytes = [u8; 16];
type TagBytes = [u8; 4];

// Constructors

impl VMString {
    /// Allocates a dynamically generated String.
    ///
    /// If the string is small, it is allocated inline. For larger string,
    /// the data is allocated on the heap.
    pub fn new<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        let s: String = s.into();
        let len = s.len();
        if len <= MAX_EMBEDDED_LENGTH {
            let mut data = [0; MAX_EMBEDDED_LENGTH];

            let src = s.as_bytes().as_ptr();
            let dst = data.as_mut_ptr();

            // Safety: `src` and `dst` are aligned and non-overlapping
            // `src` is valid for reads of `len` bytes and `dst` is valid for
            // writes of `len` bytes.
            unsafe { std::ptr::copy_nonoverlapping(src, dst, len) }

            new_embedded_string(len as u32, data)
        } else {
            new_heap_string(s)
        }
    }

    /// Creates an instance of Self from raw bytes that were copied from
    /// another Self instance.
    ///
    /// If the String data is heap-allocated, this reuses the same allocation
    /// by incrementing the reference count rather than allocating more data.
    pub fn from_copy(value: VMStringBytes) -> Self {
        // Safety: same size types
        // TODO: non unsafe way to do the same thing?
        let (tag, payload): (TagBytes, [u8; 12]) = unsafe { std::mem::transmute(value) };
        let tag = u32::from_le_bytes(tag);

        match tag {
            HEAP_DISCRIMINANT => {
                // Safety: same size types
                // TODO: non unsafe way to do the same thing?
                let (_padding, ptr_bytes): ([u8; 4], [u8; 8]) =
                    unsafe { std::mem::transmute(payload) };

                VMString::Heap(HeapVMString::from_copy(ptr_bytes))
            }
            EMBEDDED_DISCRIMINANT => VMString::Embedded(payload.into()),

            _ => unreachable!(),
        }
    }

    /// Creates an instance of Self from raw bytes that were created by
    /// the original instance calling `into_raw`.
    ///
    /// Ensures that its own allocation is re-used and the reference counts are
    /// not incremented.
    pub fn from_raw(value: VMStringBytes) -> Self {
        // Safety: same size types
        // TODO: non unsafe way to do the same thing?
        let (tag, payload): (TagBytes, [u8; 12]) = unsafe { std::mem::transmute(value) };
        let tag = u32::from_le_bytes(tag);

        match tag {
            HEAP_DISCRIMINANT => {
                // Safety: same size types
                // TODO: non unsafe way to do the same thing?
                let (_padding, ptr_bytes): ([u8; 4], [u8; 8]) =
                    unsafe { std::mem::transmute(payload) };

                VMString::Heap(HeapVMString::from_raw(ptr_bytes))
            }
            EMBEDDED_DISCRIMINANT => VMString::Embedded(payload.into()),

            _ => unreachable!(),
        }
    }
}

fn split_bytes(bytes: VMStringBytes) -> (u32, u32, [u8; 8]) {
    //                                   tag  len?  data
    todo!()
}

// Non-Mutating Functions

impl VMString {
    pub fn length(&self) -> u32 {
        match self {
            VMString::Heap(s) => s.length(),
            VMString::Embedded(s) => s.length(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.length() == 0
    }

    fn discriminant(&self) -> u32 {
        // SAFETY: Because `Self` is marked `repr(C)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u32` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u32>() }
    }

    fn as_bytes(&self) -> &[u8] {
        match self {
            VMString::Heap(s) => s.as_bytes(),
            VMString::Embedded(s) => s.as_bytes(),
        }
    }
}

impl AsStr for VMString {
    fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).unwrap()
    }
}

impl fmt::Debug for VMString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Heap(s) => f.debug_tuple("Heap").field(s).finish(),
            Self::Embedded(s) => f.debug_tuple("Embedded").field(s).finish(),
        }
    }
}

impl fmt::Display for VMString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// Destructors

impl From<VMString> for VMStringBytes {
    fn from(value: VMString) -> Self {
        let tag: TagBytes = value.discriminant().to_le_bytes();

        // TODO: better name for tag_extra?
        // for embedded strings we store the length
        // for heap allocated strings it is just padding so that the Rc ptr is aligned
        let (tag_extra, payload): ([u8; 4], [u8; 8]) = match value {
            VMString::Embedded(s) => (s.len.to_le_bytes(), s.bytes),
            VMString::Heap(s) => (
                0_u32.to_le_bytes(),
                (Rc::into_raw(s.ptr) as usize).to_le_bytes(),
            ),
        };
        let payload_split: [[u8; 4]; 2] = cast(payload);
        cast([tag, tag_extra, payload_split[0], payload_split[1]])
    }
}

impl From<VMString> for DWord {
    fn from(value: VMString) -> Self {
        let bytes: VMStringBytes = value.into();
        bytes.into()
    }
}
