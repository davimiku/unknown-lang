use std::convert::TryFrom;
use std::fmt;
use std::mem;

/// The opcodes (operation codes) of the virtual machine (VM)
///
/// Each operation code represents a runtime operation that the VM
/// can perform.
/// Most operations use values in the runtime stack. Some operations
/// also have operands directly encoded in the bytecode after the op code.
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
    /// Operands: value: PushStringOperand
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

    /// Pops 4 "slots" off the stack
    ///
    /// Operands:
    ///
    /// Stack: Word4 **=>**
    Pop4,

    /// Pops N "slots" off the stack determined by the operand.
    ///
    /// Operands: num_slots: u16
    ///
    /// Stack: N*Word **=>**
    PopN,

    /// Pops a VM string off the stack.
    ///
    /// Operands:
    ///
    /// Stack: VMString **=>**
    PopString,

    /// Pops an object from the stack that is memory-managed with
    /// reference counting (Rc).
    ///
    /// Operands:
    ///
    /// Stack: Rc<T> **=>**
    PopRc,

    /// Pops an object from the stack that is memory-managed with
    /// garbage collection (Gc).
    ///
    /// Operands:
    ///
    /// Stack: Gc<T> **=>**
    PopGc,

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

    /// Gets a local of 4 slot size at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: **=>** value
    GetLocal4,

    /// Gets a local of N slots at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///           num_slots  : u16
    ///
    /// Stack: **=>** value
    GetLocalN,

    /// Gets a VM String at the given offset
    /// and pushes it onto the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: **=>** String value
    GetLocalString,

    /// Peeks the top 1 slot size from the stack and sets
    /// that value into the stack at the given offset.
    /// The peeked value is not popped off the stack.
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

    /// Sets a local of 4 slot size to the given offset
    /// The value to set is at the top of the stack, and
    /// is not popped off the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: value **=>** value
    SetLocal4,

    /// Sets a local of N slots to the given offset.
    /// The value to set is at the top of the stack, and
    /// is not popped off the stack.
    ///
    /// Operands: slot_offset: u16
    ///           num_slots  : u16
    ///
    /// Stack: value **=>** value
    SetLocalN,

    /// Sets a VM String to the given offset
    /// The value to set is at the top of the stack, and
    /// is not popped off the stack.
    ///
    /// Operands: slot_offset: u16
    ///
    /// Stack: value **=>** value
    SetLocalString,

    /// Unary `!` operator for Bool.
    ///
    /// Operands:
    ///
    /// Stack: **=>**
    NotBool,

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
    /// Stack: Int **=>** -Int
    NegateInt,

    /// Binary `==` operator for Int
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs == rhs
    EqInt,

    /// Binary `!=` operator for Int
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs != rhs
    NeInt,

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
    /// Stack: Float **=>** -Float
    NegateFloat,

    /// Binary `==` operator for Float
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs == rhs
    EqFloat,

    /// Binary `!=` operator for Float
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs != rhs
    NeFloat,

    /// Coerces the top value of the stack into a String
    ///
    /// Operands: type of top value: `u8`
    ///
    /// Stack: (value) **=>** (string value)
    IntoString,

    /// Binary `++` operator for `String`.
    ///
    /// Stack: lhs, rhs **=>** "{lhs}{rhs}"
    ConcatString,

    /// Binary `==` operator for String
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs == rhs
    EqString,

    /// Binary `!=` operator for String
    ///
    /// Operands:
    ///
    /// Stack: lhs, rhs **=>** lhs != rhs
    NeString,

    /// Pushes the pointer for a local function that is known
    /// at compile time.
    ///
    /// Operands: local function index: `u32`
    ///
    /// Stack: **=>** function ptr
    PushLocalFunc,

    /// Calls a plain function (no closure)
    ///
    /// Operands: word size of return value: `u16`
    ///
    /// Stack: function_ptr **=>** <new call frame>
    CallFunction,

    /// Call to a built-in function
    ///
    /// Operands: built-in index: `u8`
    ///
    CallBuiltin,

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

    /// Return from a function
    ///
    /// Stack: function_ptr **=>** <previous call frame>
    Return,

    /// No operation
    /// This must always be the last variant of this enum,
    /// since it is a marker for valid conversion from u8.
    ///
    /// Operands:
    ///
    /// Stack: **=>**
    Noop,
}

impl From<Op> for u8 {
    fn from(value: Op) -> Self {
        value as u8
    }
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

#[derive(Debug)]
#[repr(C)]
pub struct PushStringOperand {
    pub len: u32,
    pub offset: u32,
}

impl PushStringOperand {
    pub(crate) fn to_bytes(&self) -> [u8; 8] {
        bytemuck::cast([self.len, self.offset])
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum IntoStringOperand {
    Bool,
    Float,
    Int,
}

#[derive(Debug)]
#[repr(u8)]
pub enum ReturnType {
    /// 1 Word
    Word,

    /// 2 Words
    DWord,

    /// 3 Words
    QWord,

    /// Variable (N) amount of Words
    ///
    /// The next byte must contain the number of words to return
    WordN,

    String,
    Array,
    Rc,
    Gc,

    None,
}

impl ReturnType {
    unsafe fn from_raw(value: u8) -> Self {
        mem::transmute(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidReturnTypeError(u8);

impl fmt::Display for InvalidReturnTypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid return type: {:#04x}", self.0)
    }
}

impl TryFrom<u8> for ReturnType {
    type Error = InvalidReturnTypeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > Self::None as u8 {
            Err(InvalidReturnTypeError(value))
        } else {
            let op = unsafe { Self::from_raw(value) };

            Ok(op)
        }
    }
}
