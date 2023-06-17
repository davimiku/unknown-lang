use std::fmt::{self, Display};
use std::mem;
use std::ops::Range;

use text_size::TextRange;

use crate::{code::Code, BytecodeRead, InvalidOpError, Op};

#[derive(Debug, Default)]
pub struct ProgramChunk {
    // TODO: ensure this is immutable/readonly so it doesn't reallocate
    // because we take raw pointers from it
    // wrap in a Rc? Or take a shared reference to it somewhere?
    pub functions: Vec<FunctionChunk>,
}

impl ProgramChunk {
    pub fn new(functions: Vec<FunctionChunk>) -> Option<Self> {
        (!functions.is_empty()).then_some(Self { functions })
    }
}

impl Display for ProgramChunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Program:\n\n")?;
        for func in self.functions.iter() {
            f.write_fmt(format_args!("{func}"))?;
        }
        f.write_str("\n")
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FunctionChunk {
    /// bytecode (ops and operands) with text ranges corresponding to those ops
    code: Code,

    /// Total "slots" / words used by parameters of this function
    pub parameter_slots: u16,

    /// constants pool that are not encoded as operands, ex. strings
    ///
    /// The first byte is the length of the name of the function, and the next
    /// `len` bytes are the function name.
    // TODO: encapsulate this in a struct?
    pub(crate) constants: Vec<u8>,
}

// Constructors
impl FunctionChunk {
    pub(crate) fn new(name: &str, parameter_slots: u16) -> Self {
        let mut chunk = Self {
            parameter_slots,
            ..Self::default()
        };

        chunk.add_name_to_constants(name);
        chunk
    }

    pub(crate) fn append(&mut self, source: Code) {
        self.code.append(source);
    }
}

// Mutating functions

impl FunctionChunk {
    fn add_name_to_constants(&mut self, name: &str) {
        let len = name.len();
        self.constants.push(len as u8);

        if len > 0 {
            self.constants.extend_from_slice(name.as_bytes());
        }
    }

    pub(crate) fn write_op(&mut self, op: Op, range: TextRange) {
        self.code.bytes.push(op as u8);
        self.code.ranges.push(range);
    }

    pub fn write_ret(&mut self, range: TextRange) {
        self.write_op(Op::Return, range);
    }

    pub fn write_noop(&mut self, range: TextRange) {
        self.write_op(Op::Noop, range);
    }

    pub fn shrink_to_fit(&mut self) {
        self.code.shrink_to_fit();
        self.constants.shrink_to_fit();
    }
}

// Non-Mutating functions
impl FunctionChunk {
    /// Gets the Op at the given index.
    #[inline]
    pub fn get_op(&self, i: usize) -> Result<Op, InvalidOpError> {
        self.code.bytes[i].try_into()
    }

    /// Reads bytes from the bytecode and returns as T.
    ///
    /// Panics if there are not enough bytes to read.
    // TODO: mutate `offset` in this function so the caller
    // doesn't need to remember to mutate it
    #[inline]
    pub fn read<T>(&self, offset: usize) -> T
    where
        T: BytecodeRead,
    {
        assert!(offset + mem::size_of::<T>() - 1 < self.code.bytes.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }

    /// Gets the name of the function from the bytecode
    pub fn name(&self) -> &str {
        // TODO: need to have compile error for functions with name >255 characters
        let len = self.constants[0];
        if len == 0 {
            return "";
        }

        let bytes = &self.constants[1..((len + 1) as usize)];

        // Safety: bytes were valid UTF-8 when Self was created
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }

    pub fn borrow_constants(&self) -> &[u8] {
        &self.constants
    }

    /// Gets a slice of bytes from the constants pool at the given index
    ///
    /// Panics: if the index is out-of-bounds
    pub fn constants_slice(&self, index: Range<usize>) -> &[u8] {
        let result = self.constants.get(index.clone());
        match result {
            Some(s) => s,
            None => {
                println!("\n\n{index:?}");
                panic!("Invalid slice index")
            }
        }
    }

    /// Gets a slice of bytes from the constants pool at the given index
    ///
    /// # Safety
    ///
    /// Triggers undefined behavior if the index is out-of-bounds
    pub unsafe fn constants_slice_unchecked(&self, index: Range<usize>) -> &[u8] {
        self.constants.get_unchecked(index)
    }

    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: BytecodeRead,
    {
        self.code
            .bytes
            .as_ptr()
            .add(offset)
            .cast::<T>()
            .read_unaligned()
    }
}

// Functions for debugging the Chunk
#[cfg(debug_assertions)]
impl fmt::Display for FunctionChunk {
    /// Prints the Chunk in a disassembled format for human-reading.
    ///
    /// This format is not stable and should not be depended on for
    /// any kind of machine parsing.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut name = self.name();
        if name.is_empty() {
            name = "<anonymous>";
        }
        let param_size = self.parameter_slots;

        writeln!(f, "{name}: param size = {param_size}")?;
        let mut offset = 0;
        let mut op_idx = 0;
        while offset < self.code.bytes.len() {
            let op = self.code.bytes[offset];
            let op: Op = op.try_into().expect("byte should be convertible to Op");
            let range = self.code.ranges[op_idx];

            let line = format!("{:?}:{:?}", range.start(), range.end());
            write!(f, "{offset:04}  {line}  ")?;
            offset = op.disassemble(self, offset);
            writeln!(f)?;

            op_idx += 1;
        }
        writeln!(f)
    }
}
