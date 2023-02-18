use std::array;

use bytemuck::cast;

use crate::words::{QWord, Word};

/// Representation of a String value in the bytecode, independent of its allocation
// TODO: this was designed to fit in 16 bytes. The same data in an `enum` with repr(C)
// uses 24 bytes (padding after the tag and after the len)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XString {
    /// Describes how the string is allocated
    tag: AllocationStrategy, // TODO: do we want it to be pub?

    /// Byte length of the string
    len: u32,

    /// Location of the string, which may be a pointer, index, or other based
    /// on how the string is allocated.
    loc: u64,
}

impl XString {
    // TODO: Can this take a Gc pointer or how does that work?
    pub fn new_allocated(len: u32, ptr: *const u8) -> Self {
        Self {
            tag: AllocationStrategy::Heap,
            len,
            loc: ptr as u64,
        }
    }

    pub fn new_constant(len: u32, idx: usize) -> Self {
        Self {
            tag: AllocationStrategy::ConstantsPool,
            len,
            loc: idx as u64,
        }
    }
}

impl XString {
    // Non-Mutating functions
    #[inline]
    pub fn length(&self) -> u32 {
        self.len
    }

    // TODO: bad name?
    #[inline]
    pub fn disassemble(&self) -> DisassembledXString {
        let Self { tag, len, loc } = self;
        match tag {
            AllocationStrategy::Heap => DisassembledXString::Heap {
                len: *len,
                ptr: *loc as *const u8,
            },
            AllocationStrategy::ConstantsPool => DisassembledXString::ConstantsPool {
                len: *len,
                start: *loc as usize,
            },
            AllocationStrategy::Embedded => todo!(),
        }
    }

    #[inline]
    pub fn to_bytes(self) -> [u8; 16] {
        let loc: [u32; 2] = cast(self.loc);
        cast([self.tag as u32, self.len, loc[0], loc[1]])
    }
}

impl From<XString> for QWord {
    fn from(source: XString) -> Self {
        source.to_bytes().into()
    }
}

impl From<QWord> for XString {
    fn from(source: QWord) -> Self {
        // Safety: The QWord used must be created by the corresponding From trait
        unsafe { std::mem::transmute(source) }
    }
}

impl IntoIterator for XString {
    type Item = Word;
    type IntoIter = array::IntoIter<Self::Item, 4>;

    fn into_iter(self) -> Self::IntoIter {
        // TODO: profile and add `unsafe` as needed for perf
        let XString { tag, len, loc } = self;
        let tag: Word = (tag as u32).into();
        let len: Word = len.into();
        let loc: [[u8; 4]; 2] = cast(loc.to_le_bytes());
        let loc: [Word; 2] = [loc[0].into(), loc[1].into()];

        let words = [tag, len, loc[0], loc[1]];
        words.into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
enum AllocationStrategy {
    /// The string is allocated on the heap, and its lifetime must be managed
    // TODO: split this into heap allocations with a statically known lifetime
    // that will have Drop calls automatically inserted vs. a heap allocation
    // where the lifetime is managed by the GC
    Heap = 1, // For easier debugging to not have zero bytes, can be removed later

    /// The String is known at compiletime and is allocated in the constants
    /// section of the bytecode. The lifetime of the string data is static
    /// for the duration of the program and is owned by the bytecode itself.
    ConstantsPool,

    /// The bytes of the String are embedded within this struct itself
    /// Up to 11 bytes? 4 bytes for tag and 1 byte for len?
    /// Could go up to 14 bytes with unsafe pack the padding after a u8 tag
    Embedded,
}

pub enum DisassembledXString {
    Heap { len: u32, ptr: *const u8 },

    ConstantsPool { len: u32, start: usize },
}
