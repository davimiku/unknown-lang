use builtins::{PRINT_BOOL, PRINT_FLOAT, PRINT_INT, PRINT_STRING};
use codegen::{Chunk, InvalidOpError, Op, Readable};
use stack::Stack;
use std::mem::size_of;
use std::ops::{Add, Mul, Sub};
use vm_types::words::Word;
use vm_types::xstring::{DisassembledXString, XString};
use vm_types::{XFloat, XInt};

mod builtins;
mod macros;
mod stack;

pub fn run(chunk: Chunk) -> InterpretResult {
    let mut vm = VM::new(chunk);
    vm.interpret()
}

#[derive(Debug, Default)]
pub struct VM {
    /// Chunk of bytecode to run
    chunk: Chunk,

    /// Instruction Pointer
    ip: usize, // TODO: review what the book says about a pointer being faster, needs unsafe?

    /// Program stack containing values
    stack: Stack,
}

impl VM {
    pub(crate) fn new(chunk: Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Default::default(),
        }
    }

    fn interpret(&mut self) -> InterpretResult {
        // TODO: setup actions?
        self.run()
        // TODO: cleanup actions?
    }

    /// Reads bytes from the bytecode and returns as T.
    /// Increments current instruction pointer accordingly.
    ///
    /// Panics if there are not enough bytes to read.
    #[inline]
    fn read<T: Readable>(&mut self) -> T {
        let value = self.chunk.read::<T>(self.ip);
        self.ip += size_of::<T>();

        value
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.read::<u8>()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let op = self.chunk.get_op(self.ip)?;
            self.ip += 1;

            use Op::*;
            match op {
                PushInt => {
                    let constant = self.read::<XInt>();
                    self.stack.push_int(constant);
                }
                PushFloat => {
                    let constant = self.read::<XFloat>();
                    self.stack.push_float(constant);
                }
                PushString => {
                    let s = self.read::<XString>();
                    self.stack.push_string(s);
                }
                PushTrue => {
                    self.stack.push_bool(true);
                }
                PushFalse => {
                    self.stack.push_bool(false);
                }
                Pop1 => {
                    self.stack.pop_word();
                }
                Pop2 => {
                    self.stack.pop_dword();
                }
                Pop4 => {
                    self.stack.pop_qword();
                }
                PopN => {
                    let num_slots = self.read_byte();
                    self.stack.pop_n_dynamic(num_slots);
                }
                GetLocal => {
                    let slot_offset = self.read::<u16>() as usize;
                    let val = self.stack.peek_word_at(slot_offset);
                    self.stack.push_word(*val);
                }
                GetLocal2 => {
                    let slot_offset = self.read::<u16>() as usize;
                    let vals = self.stack.peek_dword_at(slot_offset);

                    self.stack.push_dword(*vals);
                }
                GetLocal4 => {
                    let slot_offset = self.read::<u16>() as usize;
                    let vals = self.stack.peek_qword_at(slot_offset);

                    self.stack.push_qword(*vals);
                }
                GetLocalN => todo!(),
                SetLocal => {
                    let slot_offset = self.read::<u16>() as usize;
                    let word = self.stack.peek_word();
                    self.stack.set_word_at(*word, slot_offset);
                }
                SetLocal2 => {
                    let slot_offset = self.read::<u16>() as usize;
                    let val = self.stack.peek_dword();
                    let words: [Word; 2] = (*val).into();

                    for (i, word) in words.iter().enumerate() {
                        self.stack.set_word_at(*word, slot_offset + i);
                    }
                }
                SetLocal4 => {
                    let slot_offset = self.read::<u16>() as usize;
                    let val = self.stack.peek_qword();
                    let words: [Word; 4] = (*val).into();

                    for (i, word) in words.iter().enumerate() {
                        self.stack.set_word_at(*word, slot_offset + i);
                    }
                }
                SetLocalN => {
                    let slot_offset = self.read::<u16>();
                    let num_slots = self.read::<u16>();

                    // let val = self.stack.peek_n(num_slots.into());

                    todo!()
                }
                NotBool => {
                    let top = self.stack.peek_bool();
                    if top == 0 {
                        self.stack.replace_top_word(1)
                    } else {
                        self.stack.replace_top_word(0)
                    }
                }
                AddInt => int_bin_op!(self, add),
                SubInt => int_bin_op!(self, sub),
                MulInt => int_bin_op!(self, mul),
                DivInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    if b == 0 {
                        return Err(RuntimeError::RangeError);
                    }

                    let res = a / b;
                    self.stack.push_int(res);
                }
                NegateInt => {
                    let top = self.stack.peek_int();
                    self.stack.replace_top_word(-top);
                }
                RemInt => todo!(),
                AddFloat => float_bin_op!(self, add),
                SubFloat => float_bin_op!(self, sub),
                MulFloat => float_bin_op!(self, mul),
                DivFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    if b == 0.0 {
                        return Err(RuntimeError::DivideByZero);
                    }

                    let res = a / b;
                    self.stack.push_float(res);
                }
                NegateFloat => {
                    let top = self.stack.peek_float();
                    self.stack.replace_top_dword(-top);
                }
                ConcatString => {
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    let b = self.deref_string(b);
                    let a = self.deref_string(a);

                    // TODO: optimize out extra heap allocation, create function that
                    // copies both slices of bytes to a new pointer without intermediate Rust String
                    let c = a.to_owned() + b;
                    let (ptr, len) = self.alloc_string(&c);

                    let c = XString::new_allocated(len as u32, ptr);

                    self.stack.push_string(c);
                }
                Builtin => {
                    let builtin_idx = self.read_byte();
                    self.exec_builtin(builtin_idx);
                }
                Jump => {
                    let offset = self.read::<u32>();
                    self.ip += offset as usize;
                }
                JumpIfFalse => {
                    let offset = self.read::<u32>();
                    let condition = self.stack.pop_bool();
                    if condition == 0 {
                        self.ip += offset as usize;
                    }
                }
                Ret => {
                    // TODO: better output for debugging?

                    // TODO: return the top value of the stack maybe?
                    break Ok(());
                }
                Noop => {}
            }
        }
    }

    /// Allocates memory for a string and returns a raw pointer
    ///
    /// It is the caller's responsibility to free the memory for this string
    pub fn alloc_string(&self, s: &str) -> (*const u8, usize) {
        let count = s.len();
        let src = s.as_ptr();

        let layout = std::alloc::Layout::array::<u8>(count).expect("valid layout");
        unsafe {
            let dst = std::alloc::alloc(layout);
            std::ptr::copy_nonoverlapping(src, dst, count);

            (dst, count)
        }
    }

    /// Dereferences the UTF-8 string contents from the allocation
    pub fn deref_string(&self, s: XString) -> &str {
        let d = s.disassemble();
        let string_bytes: &[u8] = match d {
            DisassembledXString::Heap { len, ptr } => unsafe {
                std::slice::from_raw_parts(ptr, len as usize)
            },
            DisassembledXString::ConstantsPool { len, start } => {
                let end = start + (len as usize);

                // Safety: The compiler must have correctly allocated valid UTF-8
                // bytes and correctly generated the bytecode string during codegen
                #[cfg(not(debug_assertions))]
                {
                    self.chunk.constants_slice_unchecked(start..end)
                }

                #[cfg(debug_assertions)]
                {
                    self.chunk.constants_slice(start..end)
                }
            }
        };

        #[cfg(not(debug_assertions))]
        {
            std::str::from_utf8_unchecked(string_bytes)
        }

        #[cfg(debug_assertions)]
        {
            std::str::from_utf8(string_bytes).expect("bytes should be valid UTF-8")
        }
    }

    fn exec_builtin(&mut self, i: u8) {
        // TODO: move builtins to an array/map or something to not hard-code
        // TODO: define builtins as an enum, convert the u8 to the enum and match
        // Potentially define as "metadata":
        //  - num slots to pop from the stack and pass to the builtin
        //  - num slots to expect returned by the builtin to push back to the stack
        //
        // So builtins shouldn't need access to the stack at all but they will need
        // shared access to the chunk to deref constants (ex. strings)?
        match i {
            0 => unreachable!("no builtin at the zero byte"),
            PRINT_STRING => self.print_string(),
            PRINT_INT => self.print_int(),
            PRINT_FLOAT => self.print_float(),
            PRINT_BOOL => self.print_bool(),
            _ => {}
        }
    }
}

pub type InterpretResult = Result<(), RuntimeError>;

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    InvalidOpError(InvalidOpError),
    RangeError,
    DivideByZero,
}

impl From<InvalidOpError> for RuntimeError {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}
