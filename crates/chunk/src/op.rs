use std::{
    convert::TryFrom,
    fmt,
    mem::{self, size_of},
};

use crate::Chunk;

/// The opcodes of the virtual machine (VM)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Op {
    /// Push `true` value on the stack.
    ///
    /// Operands:
    ///
    /// Stack: **=>** `true`
    PushTrue,

    /// Push `false` value on the stack.
    ///
    /// Operands:
    ///
    /// Stack: **=>** `false`
    PushFalse,

    /// Push i64 value on the stack.
    ///
    /// Operands: value: `i64`
    ///
    /// Stack: **=>** value
    PushInt,

    /// Push `f64` value on the stack.
    ///
    /// Operands: value: `f64`
    ///
    /// Stack: **=>** value
    PushFloat,

    /// Push `String` value on the stack.
    ///
    /// Operands: value: `(ptr: u64, len: i64)`
    ///
    /// Stack: **=>** value
    PushString,

    /// Binary `+` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs + rhs)
    AddInt,

    /// Binary `-` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs - rhs)
    SubInt,

    /// Binary `*` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs * rhs)
    MulInt,

    /// Binary `/` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs / rhs)
    DivInt,

    /// Binary `%` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs % rhs)
    RemInt,

    /// Binary `+` operator for Float.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs + rhs)
    AddFloat,

    /// Binary `-` operator for Float.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs - rhs)
    SubFloat,

    /// Binary `*` operator for Float.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs * rhs)
    MulFloat,

    /// Binary `/` operator for Float.
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** (lhs / rhs)
    DivFloat,

    /// Binary `+` operator for String.
    ///
    /// Operands: mode: `u8`
    ///
    /// Stack: lhs, rhs **=>** "{lhs}{rhs}"
    ConcatString,

    // push local?
    // pop local?
    /// Call to a built-in function
    /// Needs index of the built-in
    /// TODO: how to indicate args?
    Builtin,

    /// Return from a function
    Ret,

    /// No operation
    ///
    /// Operands:
    ///
    /// Stack: **=>**
    Noop,
}

impl Op {
    /// Creates an Op from a byte (`u8`)
    ///
    /// # Safety
    ///
    /// Does not check if the byte is valid.
    /// You should use the TryFrom conversion to safely
    /// convert from byte to Op.
    unsafe fn from_raw(value: u8) -> Self {
        mem::transmute(value)
    }

    /// Returns the size (in bytes) of the operand for each Op
    pub fn operand_size(&self) -> usize {
        let word = 8;
        match self {
            Op::PushInt => word,
            Op::PushFloat => word,
            Op::PushString => word * 2,
            Op::Builtin => todo!(),

            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidOpError(u8);

impl fmt::Display for InvalidOpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid opcode: {:#04x}", self.0)
    }
}

impl TryFrom<u8> for Op {
    type Error = InvalidOpError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > Self::Noop as u8 {
            Err(InvalidOpError(value))
        } else {
            let op = unsafe { Self::from_raw(value) };

            Ok(op)
        }
    }
}

impl Op {
    /// Prints a dissembled form of the instruction.
    ///
    /// Returns the new offset to use for the next instruction.
    pub fn disassemble(&self, chunk: &Chunk, offset: usize) -> usize {
        print!("{self:?}    ");

        let mut offset = offset + size_of::<Op>();
        match self {
            Op::PushInt => {
                let int = chunk.read_int(offset);
                offset += size_of::<i64>();

                print!("{int}");
            }
            Op::PushFloat => {
                let float = chunk.read_float(offset);
                offset += size_of::<f64>();

                print!("{float}");
            }
            Op::PushString => {
                let (idx, len) = chunk.read_str(offset);
                offset += size_of::<(u64, u64)>();

                let s = chunk.get_str_constant(idx as usize, len as usize);
                print!("\"{s}\"");
            }
            Op::ConcatString => {
                //
            }
            _ => {}
        };

        offset
    }
}
