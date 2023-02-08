use std::convert::TryFrom;
use std::fmt;
use std::mem;
use std::mem::size_of;

use crate::Float;
use crate::Int;

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
    /// Stack: **=>** i64
    PushInt,

    /// Push `f64` value on the stack.
    ///
    /// Operands: value: `f64`
    ///
    /// Stack: **=>** f64
    PushFloat,

    /// Push `String` value on the stack.
    ///
    /// Operands: value: `(ptr: u64, len: i64)`
    ///
    /// Stack: **=>** String
    PushString,

    /// Pops 1 "slot" off the stack
    ///
    /// Operands:
    ///
    /// Stack: Word **=>**
    Pop1,

    /// Pops 2 "slots" off the stack
    ///
    /// Operands:
    ///
    /// Stack: Word2 **=>**
    Pop2,

    /// Pops N "slots" off the stack determined by the operand.
    ///
    /// Operands: num_slots: u16
    ///
    /// Stack: N*Word **=>**
    PopN,

    /// Gets a local of 1 slot size at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: **=>** value
    GetLocal,

    /// Gets a local of 2 slot size at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: **=>** value
    GetLocal2,

    /// Gets a local of N slot size at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///           slot_size  : u16
    ///
    /// Stack: **=>** value
    GetLocalN,

    /// Peeks the top 1 slot size from the stack and sets
    /// that value into the stack at the given offset.
    /// The peeked value is not popped off the stack?
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: value **=>** value
    SetLocal,

    /// Sets a local of 2 slot size to the given offset
    /// The value to set is at the top of the stack, and
    /// is not popped off the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: value **=>** value
    SetLocal2,

    /// Sets a local of N slot size to the given offset
    /// The value to set is at the top of the stack, and
    /// is not popped off the stack.
    ///
    /// Operands: slot_offset: u16
    ///           slot_size  : u16
    ///
    /// Stack: value **=>** value
    SetLocalN,

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

    /// Unary `-` operator for Int.
    ///
    /// Operands:
    ///
    /// Stack: **=>**
    NegateInt,

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

    /// Unary `-` operator for Float.
    ///
    /// Operands:
    ///
    /// Stack: **=>**
    NegateFloat,

    /// Binary `+` operator for String.
    ///
    /// Operands: mode: `u8`
    ///
    /// Stack: lhs, rhs **=>** "{lhs}{rhs}"
    ConcatString,

    /// Call to a built-in function
    ///
    /// Operands: built-in index: `u8`
    ///
    Builtin,

    /// Unconditional forwards jump by the given offset
    ///
    /// Operands: jump amount: `u32`
    ///
    /// Stack: **=>**
    Jump,

    /// Jumps forwards by the given offset if the top of
    /// the stack is false.
    ///
    /// Operands: jump amount: `u32`
    ///
    /// Stack: cond **=>**
    JumpIfFalse,

    // push local?
    // pop local?
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
        match self {
            Op::PushInt => size_of::<Int>(),
            Op::PushFloat => size_of::<Float>(),
            Op::PushString => size_of::<(u64, u64)>(),
            Op::GetLocal | Op::GetLocal2 => size_of::<u16>(),
            Op::SetLocal | Op::SetLocal2 => size_of::<u16>(),
            Op::PopN => size_of::<u16>(),
            Op::Builtin => size_of::<u8>(),
            Op::Jump | Op::JumpIfFalse => size_of::<u32>(),

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
