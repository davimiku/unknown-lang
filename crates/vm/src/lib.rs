use builtins::{LEN_STRING, PRINT_BOOL, PRINT_FLOAT, PRINT_INT, PRINT_STRING};
use memory_recycling::Gc;
use stack::Stack;
use std::mem::size_of;
use std::ops::{Add, Mul, Sub};
use std::rc::Rc;
use vm_boxed_types::VMFunction;
use vm_codegen::{BytecodeRead, FunctionChunk, InvalidOpError, Op, ProgramChunk};
use vm_types::string::{VMString, MAX_EMBEDDED_LENGTH};
use vm_types::words::Word;
use vm_types::{FromWordVec, VMFloat, VMInt};

mod builtins;
mod macros;
mod stack;

const MAX_FRAMES: usize = 64;

pub fn run(chunk: ProgramChunk) -> InterpretResult<()> {
    let mut vm = VM::new(chunk);
    vm.run()
}

/// Runs the VM and returns the remaining value from the stack.
///
/// The caller must provide the expected result because stack
/// values may be different sizes.
pub fn run_and_return<T>(chunk: ProgramChunk) -> InterpretResult<Vec<Word>>
where
    T: FromWordVec,
{
    let mut vm = VM::new(chunk);
    let result = vm.run();

    let words_to_pop = vm_types::word_size_of::<T>();
    let popped_words = vm.stack.pop_n_as_vec(words_to_pop);

    result.map(|_| popped_words)
}

#[derive(Debug)]
pub struct VM<'a> {
    /// Program stack containing values
    stack: Stack,

    /// Current call frames
    frames: Vec<CallFrame<'a>>,

    interner: lasso::Rodeo,
}

impl VM<'_> {
    pub(crate) fn new(program_chunk: ProgramChunk) -> Self {
        let mut vm = VM {
            // chunk: main_chunk,
            stack: Stack::default(),
            frames: Vec::with_capacity(MAX_FRAMES),
            interner: lasso::Rodeo::default(),
        };
        vm.push_frame(&program_chunk.main());

        vm
    }

    fn push_frame(&mut self, chunk: &FunctionChunk) {
        let frameCount = self.frames.len();
        let function = self.alloc_function(chunk);
        let frame = CallFrame {
            function: &function,
            ip: todo!(),
            slots: todo!(),
        };

        self.frames.push(frame);
    }

    fn run(&mut self) -> InterpretResult<()> {
        let mut frameCount = self.frames.len();
        let mut frame = self.frames[frameCount - 1];
        loop {
            let op = frame.function.chunk.get_op(frame.ip)?;
            frame.ip += 1;

            use Op::*;
            match op {
                PushInt => {
                    let constant = self.read::<VMInt>();
                    self.stack.push_int(constant);
                }
                PushFloat => {
                    let constant = self.read::<VMFloat>();
                    self.stack.push_float(constant);
                }
                PushString => {
                    let s = self.read::<VMString>();
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
                    let num_slots = self.read::<u8>();
                    self.stack.pop_n_dynamic(num_slots);
                }
                PopString => {
                    let _ = self.stack.pop_string();
                    // VMString drops, including its internal Rc for Rc-managed strings
                }
                PopRc => {
                    let raw_ptr = self.stack.pop_ptr::<u8>();
                    // Safety: ptr was created by Rc::into_raw originally
                    let _ = unsafe { Rc::from_raw(raw_ptr) };
                    // Rc gets dropped and reduces strong count
                }
                PopGc => {
                    let raw_ptr = self.stack.pop_ptr::<u8>();

                    // Safety: ptr was created by Gc::into_raw originally
                    let _ = unsafe { Gc::from_raw(raw_ptr) };
                    // Gc gets dropped and reduces count
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
                    let _true = 1;
                    let _false = 0;
                    match top {
                        b if b == _false => self.stack.replace_top_word(_true),
                        b if b == _true => self.stack.replace_top_word(_false),
                        _ => unreachable!("invalid stack value for Bool"),
                    }
                }
                AddInt => int_bin_op!(self, add),
                SubInt => int_bin_op!(self, sub),
                MulInt => int_bin_op!(self, mul),
                DivInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    if b == 0 {
                        return Err(RuntimePanic::RangeError);
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
                        return Err(RuntimePanic::DivideByZero);
                    }

                    let res = a / b;
                    self.stack.push_float(res);
                }
                NegateFloat => {
                    let top = self.stack.peek_float();
                    self.stack.replace_top_word(-top);
                }
                ConcatString => {
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    let b = self.deref_string(&b);
                    let a = self.deref_string(&a);

                    // TODO: optimize out extra heap allocation, create function that
                    // copies both slices of bytes to a new pointer without intermediate Rust String
                    let concatenated = self.alloc_string(format!("{a}{b}"));

                    self.stack.push_string(concatenated);
                }
                Call => {
                    let num_words_args = self.read::<u8>();

                    todo!()
                }
                Builtin => {
                    let builtin_idx = self.read::<u8>();
                    self.call_builtin(builtin_idx);
                }
                Jump => {
                    let offset = self.read::<u32>();
                    frame.ip += offset as usize;
                }
                JumpIfFalse => {
                    let offset = self.read::<u32>();
                    let condition = self.stack.pop_bool();
                    if condition == 0 {
                        frame.ip += offset as usize;
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

    /// Reads bytes from the bytecode and returns as T.
    /// Increments current instruction pointer accordingly.
    ///
    /// Panics if there are not enough bytes to read.
    // TODO: possibly take `ip` as a mutable reference parameter so
    // it is removed from the VM struct?
    #[inline]
    fn read<T: BytecodeRead>(&mut self) -> T {
        let mut frame = self.frames[self.frames.len() - 1];
        let value = frame.function.chunk.read::<T>(frame.ip);
        frame.ip += size_of::<T>();

        value
    }

    /// Allocates a dynamically generated String.
    ///
    /// If the string is small, it is allocated inline. For larger string,
    /// the data is allocated on the heap.
    ///
    /// It is the caller's responsibility to free the memory for this string.
    /// Normally, this will be handled by the garbage collector or with statically
    /// determined `drop` operations.
    fn alloc_string(&self, s: String) -> VMString {
        let len = s.len();
        if len <= MAX_EMBEDDED_LENGTH {
            let len = len as u8;
            let mut data = [0; MAX_EMBEDDED_LENGTH];
            s.bytes()
                .zip(data.iter_mut())
                .for_each(|(byte, data_ptr)| *data_ptr = byte);

            VMString::new_embedded(len, data)
        } else {
            // let src = s.as_ptr();

            // let layout = std::alloc::Layout::array::<u8>(len).expect("valid layout");
            // let ptr = unsafe {
            //     let dst = std::alloc::alloc(layout);
            //     std::ptr::copy_nonoverlapping(src, dst, len);

            //     dst
            // };
            VMString::new_allocated(s)
        }
    }

    /// Dereferences the UTF-8 string contents from the allocation
    pub(crate) fn deref_string(&self, s: &VMString) -> String {
        let mut frame = &self.frames[self.frames.len() - 1];

        let constants = frame.function.chunk.borrow_constants();
        s.to_string(constants)
    }

    /// Allocates a function object on the heap
    fn alloc_function<'a>(&'a mut self, chunk: &'a FunctionChunk) -> VMFunction {
        let name = self.interner.get_or_intern(chunk.name());

        VMFunction {
            parameter_slots: 0,
            chunk,
            name: Some(name),
        }
    }

    fn call_function(&mut self, chunk: &FunctionChunk) {
        self.push_frame(chunk)
        /*
           CallFrame* frame = &vm.frames[vm.frameCount++];
           frame->function = function;
           frame->ip = function->chunk.code;
           frame->slots = vm.stackTop - argCount - 1;
           return true;
        */
    }

    fn call_builtin(&mut self, i: u8) {
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
            LEN_STRING => self.len_string(),
            _ => {}
        }
    }
}

pub type InterpretResult<T> = Result<T, RuntimePanic>;

#[derive(Debug, Clone, Copy)]
pub struct CallFrame<'a> {
    function: &'a VMFunction<'a>,

    // Review what the book said about using a pointer is faster?
    ip: usize,

    slots: &'a [Word],
}

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimePanic {
    InvalidOpError(InvalidOpError),
    RangeError,
    DivideByZero,
}

impl From<InvalidOpError> for RuntimePanic {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}
