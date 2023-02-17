use crate::Word;

/// Representation of a String value in the bytecode, independent of its allocation
// TODO: this was built to fit in 16 bytes with an explicit tag. Can that be done with an enum?
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XString {
    /// Location of the first byte of the string, could be a pointer or an index
    pub loc: u64,

    /// Length of the string
    pub len: u32,

    /// Stores how the string was allocated
    pub tag: AllocationStrategy,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum AllocationStrategy {
    /// The string is allocated on the heap, and its lifetime must be managed
    // TODO: split this into heap allocations with a statically known lifetime
    // that will have Drop calls automatically inserted vs. a heap allocation
    // where the lifetime is managed by the GC
    Heap = 1, // For easier debugging to not have zero bytes, can be removed later

    /// The String is known at compiletime and is allocated in the constants
    /// section of the bytecode. The lifetime of the string data is static
    /// for the duration of the program and is owned by the bytecode itself.
    ConstantsPool,
}

impl TryFrom<u32> for AllocationStrategy {
    type Error = ();

    fn try_from(v: u32) -> Result<Self, Self::Error> {
        use AllocationStrategy::*;
        match v {
            x if x == Heap as u32 => Ok(Heap),
            x if x == ConstantsPool as u32 => Ok(ConstantsPool),
            _ => Err(()),
        }
    }
}

impl AllocationStrategy {
    pub fn from_u32(val: u32) -> Self {
        val.try_into().expect("valid u32 enum value")
    }
}

// Constructors
impl XString {
    pub fn from_words(words: [Word; 4]) -> Self {
        let [loc, len_and_tag]: [u64; 2] = bytemuck::cast(words);
        let [len, tag]: [u32; 2] = bytemuck::cast(len_and_tag);

        // Safety: Only valid enum values are written to the bytecode
        let tag = AllocationStrategy::from_u32(tag);
        XString { loc, len, tag }
    }

    pub fn from_bytes(bytes: [u8; 16]) -> Self {
        let words = bytemuck::cast(bytes);

        XString::from_words(words)
    }
}

// Non-Mutating functions
impl XString {
    #[inline]
    pub fn length(&self) -> u32 {
        self.len
    }

    #[inline]
    pub fn allocation(&self) -> AllocationStrategy {
        self.tag
    }
}

// Deconstructors
impl XString {
    /// Decomposes the struct into words/slots for the stack
    pub fn to_words(self) -> [Word; 4] {
        let len_and_tag = bytemuck::cast::<[u32; 2], [Word; 2]>([self.len, self.tag as u32]);
        let loc = bytemuck::cast::<u64, [Word; 2]>(self.loc);

        bytemuck::cast([loc, len_and_tag])
    }

    /// Decomposes the struct into bytes for the stack
    pub fn to_bytes(self) -> [u8; 16] {
        let words = self.to_words();

        bytemuck::cast(words)
    }
}
