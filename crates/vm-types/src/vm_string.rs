use crate::words::QWord;
use bytemuck::cast;
use std::mem::MaybeUninit;

/// Representation of a String value in the bytecode, independent of its allocation
// TODO: this was designed to fit in 16 bytes. The same data in an `enum` with repr(C)
// uses 24 bytes (padding after the tag and after the len)
#[repr(C)]
#[derive(Clone, Copy)]
pub struct VMString {
    tag: AllocationStrategy,

    data: VMStringData,
}

#[repr(C)]
#[derive(Clone, Copy)]
union VMStringData {
    heap: HeapVMString,

    constant: ConstantVMString,

    embedded: EmbeddedVMString,
}

impl VMString {
    pub fn new_allocated(len: u32, ptr: *const u8) -> Self {
        let data = VMStringData {
            heap: HeapVMString { len, ptr },
        };
        Self {
            tag: AllocationStrategy::Heap,
            data,
        }
    }

    pub fn new_constant(len: u32, idx: usize) -> Self {
        let data = VMStringData {
            constant: ConstantVMString {
                len,
                start_idx: idx,
            },
        };
        Self {
            tag: AllocationStrategy::Constants,
            data,
        }
    }
}

// Non-Mutating functions
impl VMString {
    /// Returns the bytes length of the string
    #[inline]
    pub fn length(&self) -> u32 {
        use AllocationStrategy::*;
        unsafe {
            match self {
                Self { tag: Heap, data } => data.heap.len,
                Self {
                    tag: Constants,
                    data,
                } => data.constant.len,
                Self {
                    tag: Embedded,
                    data,
                } => data.embedded.len.into(),
            }
        }
    }

    /// Returns a Rust &str for this String
    #[inline]
    pub fn deref<'a>(&'a self, constants: &'a [u8]) -> &'a str {
        use AllocationStrategy::*;
        let bytes = unsafe {
            match self {
                Self { tag: Heap, data } => data.heap.bytes(),
                Self {
                    tag: Constants,
                    data,
                } => data.constant.bytes(constants),
                Self {
                    tag: Embedded,
                    data,
                } => data.embedded.bytes(),
            }
        };

        // TODO: `unsafe` version for release builds
        std::str::from_utf8(bytes).expect("valid UTF-8 bytes")
    }

    /// Decomposes the struct into an array of bytes
    #[inline]
    fn to_bytes(self) -> [u8; 16] {
        use AllocationStrategy::*;
        let tag_bytes: [u8; 4] = cast(self.tag as u32);
        let data_bytes: [u8; 12] = unsafe {
            match self {
                Self { tag: Heap, data } => data.heap.into(),
                Self {
                    tag: Constants,
                    data,
                } => data.constant.into(),
                Self {
                    tag: Embedded,
                    data,
                } => data.embedded.into(),
            }
        };
        unsafe {
            let mut uninit = MaybeUninit::<[u8; 16]>::uninit();
            let start = uninit.as_mut_ptr() as *mut u8;

            (start.add(0) as *mut [u8; 4]).write(tag_bytes);
            (start.add(tag_bytes.len()) as *mut [u8; 12]).write(data_bytes);

            uninit.assume_init()
        }
    }
}

impl From<VMString> for QWord {
    fn from(source: VMString) -> Self {
        source.to_bytes().into()
    }
}

impl From<VMString> for [u8; 16] {
    fn from(source: VMString) -> Self {
        source.to_bytes()
    }
}

impl From<QWord> for VMString {
    fn from(source: QWord) -> Self {
        // Safety: The QWord used must be created by the corresponding From trait
        unsafe { std::mem::transmute(source) }
    }
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct HeapVMString {
    /// Length of the string
    len: u32,

    /// Pointer to the first byte of the string
    ptr: *const u8,
}

impl From<HeapVMString> for [u8; 12] {
    fn from(value: HeapVMString) -> Self {
        todo!()
    }
}

impl HeapVMString {
    fn bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len as usize) }
    }
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct ConstantVMString {
    /// Length of the string
    len: u32,

    /// Index of the first byte of the string in the constants pool
    start_idx: usize,
}

impl ConstantVMString {
    fn bytes<'a>(&self, constants: &'a [u8]) -> &'a [u8] {
        let start = self.start_idx;
        let end = start + (self.len as usize);

        // TODO: could add an `unsafe` unchecked version for release builds
        constants.get(start..end).expect("valid constants index")
    }
}

impl From<ConstantVMString> for [u8; 12] {
    fn from(value: ConstantVMString) -> Self {
        let start_split: [u32; 2] = cast(value.start_idx);

        cast([value.len, start_split[0], start_split[1]])
    }
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct EmbeddedVMString {
    /// Length of the string
    len: u8,

    /// Bytes of the string
    data: [u8; 11], // TODO: make the tag u8 and this can fit 14 bytes?
}

impl EmbeddedVMString {
    fn bytes(&self) -> &[u8] {
        let end = self.len as usize;
        &self.data.get(0..end).expect("valid slice index")
    }
}

impl From<EmbeddedVMString> for [u8; 12] {
    fn from(value: EmbeddedVMString) -> Self {
        todo!()
    }
}

// impl IntoIterator for VMString {
//     type Item = Word;
//     type IntoIter = array::IntoIter<Self::Item, 4>;

//     fn into_iter(self) -> Self::IntoIter {
//         // TODO: profile and add `unsafe` as needed for perf
//         let VMString { tag, len, loc } = self;
//         let tag: Word = (tag as u32).into();
//         let len: Word = len.into();
//         let loc: [[u8; 4]; 2] = cast(loc.to_le_bytes());
//         let loc: [Word; 2] = [loc[0].into(), loc[1].into()];

//         let words = [tag, len, loc[0], loc[1]];
//         words.into_iter()
//     }
// }

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
    Constants,

    /// The bytes of the String are embedded within this struct itself
    /// Up to 11 bytes? 4 bytes for tag and 1 byte for len?
    /// Could go up to 14 bytes with unsafe pack the padding after a u8 tag
    Embedded,
}

pub enum DisassembledVMString {
    Heap { len: u32, ptr: *const u8 },

    ConstantsPool { len: u32, start: usize },
}
