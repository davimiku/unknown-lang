use crate::words::QWord;
use bytemuck::cast;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::rc::Rc;
/// Representation of a String value in the bytecode, independent of its allocation
// TODO: this was designed to fit in 16 bytes. The same data in an `enum` with repr(C)
// uses 24 bytes (padding after the tag and after the len)
#[repr(C)]
pub struct VMString2 {
    tag: AllocationStrategy,

    data: VMStringData,
}

#[repr(C)]
pub enum VMString {
    Heap(HeapVMString),

    Constant(ConstantVMString),

    Embedded(EmbeddedVMString),
}

#[repr(C)]
union VMStringData {
    heap: ManuallyDrop<HeapVMString>,

    constant: ConstantVMString,

    embedded: EmbeddedVMString,
}

type RawVMStringData = [u8; 12];

impl VMString {
    pub fn new_allocated(s: String) -> Self {
        Self::Heap(HeapVMString {
            len: s.len() as u32,
            ptr: Rc::new(s),
        })
    }

    pub fn new_constant(len: u32, start_idx: usize) -> Self {
        Self::Constant(ConstantVMString { len, start_idx })
    }

    pub fn new_embedded(len: u8, data: [u8; MAX_EMBEDDED_LENGTH]) -> Self {
        Self::Embedded(EmbeddedVMString { len, data })
    }
}

// Non-Mutating functions
impl VMString {
    /// Returns the bytes length of the string
    #[inline]
    pub fn length(&self) -> u32 {
        match self {
            VMString::Heap(s) => s.len,
            VMString::Constant(s) => s.len,
            VMString::Embedded(s) => s.len as u32,
        }
    }

    /// Returns a Rust String for this VMString by cloning
    // TODO: improve perf by avoiding String cloning
    #[inline]
    pub fn to_string(&self, constants: &[u8]) -> String {
        match self {
            VMString::Heap(s) => s.dereference().deref().clone(),
            VMString::Constant(s) => {
                String::from_utf8(s.bytes(constants).to_vec()).expect("valid UTF-8 bytes")
            }
            VMString::Embedded(s) => {
                String::from_utf8(s.bytes().to_vec()).expect("valid UTF-8 bytes")
            }
        }
    }

    fn discriminant(&self) -> u32 {
        // SAFETY: Because `Self` is marked `repr(C)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u32` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u32>() }
    }

    // Decomposes the struct into an array of bytes
    // #[inline]
    // fn to_bytes(self) -> [u8; 16] {
    //     match self {
    //         VMString::Heap(s) => todo!(),
    //         VMString::Constant(s) => todo!(),
    //         VMString::Embedded(s) => todo!(),
    //     }
    //     use AllocationStrategy::*;
    //     let tag_bytes: [u8; 4] = cast(self.tag as u32);
    //     let data_bytes: RawVMStringData = unsafe {
    //         match self {
    //             Self { tag: Heap, data } => data.heap.into(),
    //             Self {
    //                 tag: Constants,
    //                 data,
    //             } => data.constant.into(),
    //             Self {
    //                 tag: Embedded,
    //                 data,
    //             } => data.embedded.into(),
    //         }
    //     };
    //     unsafe {
    //         let mut uninit = MaybeUninit::<[u8; 16]>::uninit();
    //         let start = uninit.as_mut_ptr() as *mut u8;

    //         (start.add(0) as *mut [u8; 4]).write(tag_bytes);
    //         (start.add(tag_bytes.len()) as *mut RawVMStringData).write(data_bytes);

    //         uninit.assume_init()
    //     }
    // }
}

impl From<VMString> for QWord {
    fn from(source: VMString) -> Self {
        let bytes: [u8; 16] = source.into();
        bytes.into()
    }
}

impl From<VMString> for [u8; 16] {
    fn from(source: VMString) -> Self {
        let tag: [u8; 4] = source.discriminant().to_le_bytes();
        let data: RawVMStringData = match source {
            VMString::Heap(s) => s.into(),
            VMString::Constant(s) => s.into(),
            VMString::Embedded(s) => s.into(),
        };
        unsafe {
            let mut result = std::mem::MaybeUninit::uninit();
            let dest = result.as_mut_ptr() as *mut u8;
            std::ptr::copy_nonoverlapping(tag.as_ptr(), dest, tag.len());
            std::ptr::copy_nonoverlapping(data.as_ptr(), dest.add(tag.len()), data.len());
            result.assume_init()
        }
    }
}

impl From<QWord> for VMString {
    fn from(source: QWord) -> Self {
        let bytes: [u8; 16] = source.into();

        bytes.into()
    }
}

impl From<[u8; 16]> for VMString {
    fn from(bytes: [u8; 16]) -> Self {
        // TODO: need to construct the Rc using Rc::from_raw so that it gets
        // dropped whenever this VMString is dropped

        // 1. read first 4 bytes as the tag/discriminant
        // 2. call
        // VMString::new_allocated(s);
        // VMString::new_constant(len, start_idx);
        // VMString::new_embedded(len, data);
        todo!()
    }
}

#[repr(packed, C)]
pub struct HeapVMString {
    /// Length of the string
    len: u32,

    /// Reference counted string data
    ptr: Rc<String>,
}

impl HeapVMString {
    fn dereference(&self) -> Rc<String> {
        // Safety: Necessary to avoid undefined behavior of directly
        // casting an unaligned pointer from the packed struct.
        let raw_ptr = std::ptr::addr_of!(self.ptr);

        unsafe { std::ptr::read_unaligned(raw_ptr) }
    }
}

impl From<HeapVMString> for RawVMStringData {
    fn from(value: HeapVMString) -> Self {
        let ptr = Rc::into_raw(value.ptr);
        let ptr_split: [u32; 2] = cast(ptr as usize);
        cast([value.len, ptr_split[0], ptr_split[1]])
    }
}

impl From<RawVMStringData> for HeapVMString {
    fn from(value: RawVMStringData) -> Self {
        todo!()
    }
}

#[repr(packed, C)]
#[derive(Debug, Clone, Copy)]
pub struct ConstantVMString {
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

impl From<ConstantVMString> for RawVMStringData {
    fn from(value: ConstantVMString) -> Self {
        let start_split: [u32; 2] = cast(value.start_idx);

        cast([value.len, start_split[0], start_split[1]])
    }
}

impl From<RawVMStringData> for ConstantVMString {
    fn from(value: RawVMStringData) -> Self {
        todo!()
    }
}

// TODO: make the tag u8 and this can fit 14 bytes?
// 1 byte tag, 1 byte length, 14 bytes data
// or use the same strategy as compact_str to pack the length
// in the last byte
pub const MAX_EMBEDDED_LENGTH: usize = 11;

#[repr(packed, C)]
#[derive(Debug, Clone, Copy)]
pub struct EmbeddedVMString {
    /// Length of the string
    len: u8,

    /// Bytes of the string
    data: [u8; MAX_EMBEDDED_LENGTH],
}

impl EmbeddedVMString {
    fn bytes(&self) -> &[u8] {
        let end = self.len as usize;
        self.data.get(0..end).expect("valid slice index")
    }
}

impl From<EmbeddedVMString> for RawVMStringData {
    fn from(value: EmbeddedVMString) -> Self {
        let mut uninit = MaybeUninit::<RawVMStringData>::uninit();
        let start = uninit.as_mut_ptr() as *mut u8;

        // Safety: EmbeddedVMString is 12 bytes and repr(packed, C),
        // so these bytes can be written to an array.
        unsafe {
            (start.add(0) as *mut [u8; 1]).write([value.len]);
            (start.add(1) as *mut [u8; MAX_EMBEDDED_LENGTH]).write(value.data);

            uninit.assume_init()
        }
    }
}

impl From<RawVMStringData> for EmbeddedVMString {
    fn from(value: RawVMStringData) -> Self {
        todo!()
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
