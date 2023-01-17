use std::convert::TryFrom;
use std::fmt;
use std::mem;
use std::mem::size_of;

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
            Op::PushInt => size_of::<u64>(),
            Op::PushFloat => size_of::<f64>(),
            Op::PushString => size_of::<(u64, u64)>(),
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
