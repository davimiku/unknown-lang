use builtins::{PRINT_BOOL, PRINT_FLOAT, PRINT_INT, PRINT_STR, PRINT_STR_CONSTANT};
use codegen::{Chunk, Float, Int, InvalidOpError, Op, Readable};
use stack::Stack;
use std::mem::size_of;
use std::ops::{Add, Mul, Sub};

mod builtins;
mod macros;
mod stack;

pub fn run(chunk: &Chunk) -> InterpretResult {
    let mut vm = VM::new(chunk);
    vm.interpret()
}

#[derive(Debug)]
pub(crate) struct VM<'a> {
    /// Chunk of bytecode to run
    chunk: &'a Chunk,

    /// Instruction Pointer
    ip: usize, // TODO: review what the book says about a pointer being faster, needs unsafe?

    /// Program stack containing values
    stack: Stack,
}

impl<'a> VM<'a> {
    pub(crate) fn new(chunk: &'a Chunk) -> Self {
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

    /// Reads the stack representation of a string from the
    /// current offset in the bytecode and increments the
    /// instruction pointer.
    ///
    /// Returns the bytes read from the bytecode as (u64, u64)
    #[inline]
    fn read_str(&mut self) -> (u64, u64) {
        self.read::<(u64, u64)>()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let op = self.chunk.get_op(self.ip)?;
            self.ip += 1;

            // #[cfg(test)]
            // self.debug_print(op);

            use Op::*;
            match op {
                PushInt => {
                    let constant = self.read::<Int>();
                    self.stack.push_int(constant);
                }
                PushFloat => {
                    let constant = self.read::<Float>();
                    self.stack.push_float(constant);
                }
                PushString => {
                    let (idx, len) = self.read_str();
                    self.stack.push_str_constant(idx, len);
                }
                PushTrue => {
                    self.stack.push_bool(true);
                }
                PushFalse => {
                    self.stack.push_bool(false);
                }
                Pop1 => {
                    self.stack.pop();
                }
                Pop2 => {
                    self.stack.pop_two();
                }
                PopN => {
                    let num_slots = self.read_byte();
                    self.stack.pop_n(num_slots);
                }
                GetLocal => {
                    let slot_index = self.read::<u16>();
                    let val = self.stack.peek_at(slot_index as usize);
                    self.stack.push(*val);
                }
                GetLocal2 => todo!(),
                SetLocal => {
                    let slot_index = self.read::<u16>();
                    let val = self.stack.peek();
                    self.stack.set(slot_index as usize, val);
                }
                SetLocal2 => todo!(),

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
                    self.stack.replace_top_two((-top).to_ne_bytes());
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
                RemInt => todo!(),
                ConcatString => todo!(),
                NegateInt => {
                    let top = self.stack.peek_int();
                    self.stack.replace_top((-top).to_ne_bytes());
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
                    if !condition {
                        self.ip += offset as usize;
                    }
                }
                Ret => {
                    // TODO: better output for debugging?
                    // dbg!(self.stack.pop::<[u8; 8]>());

                    // TODO: return the top value of the stack maybe?
                    break Ok(());
                }
                Noop => {}
            }
        }
    }

    fn exec_builtin(&mut self, i: u8) {
        // TODO: move builtins to an array/map or something to not hard-code
        match i {
            PRINT_STR_CONSTANT => self.print_str_constant(),
            PRINT_STR => {} // self.print_str
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
