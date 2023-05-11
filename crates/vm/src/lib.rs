use builtins::{
    len_string, print_bool, print_float, print_int, print_string, LEN_STRING, PRINT_BOOL,
    PRINT_FLOAT, PRINT_INT, PRINT_STRING,
};
use exitcode::ExitCode;
use stack::Stack;
use std::mem::{size_of, MaybeUninit};
use std::ops::{Add, Mul, Sub};
use std::rc::Rc;
use vm_boxed_types::VMFunction;
use vm_codegen::{BytecodeRead, FunctionChunk, InvalidOpError, Op, ProgramChunk, ReturnType};
use vm_types::string::{VMString, MAX_EMBEDDED_LENGTH};
use vm_types::words::Word;
use vm_types::{FromWordVec, VMBool, VMFloat, VMInt};

mod builtins;
mod macros;
mod stack;

const MAX_FRAMES: usize = 64;
const FUNC_PTR_SIZE: usize = 1;

const BOOL_TRUE: VMBool = 1;
const BOOL_FALSE: VMBool = 0;

pub fn run(chunk: ProgramChunk) -> InterpretResult<ExitCode> {
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
pub struct VM {
    /// Program stack containing values
    stack: Stack,

    /// Active call frames
    frames: [CallFrame; MAX_FRAMES],

    /// Current number of active call frames
    frame_count: usize,

    /// Program bytecode, where each function has a chunk
    ///
    // TODO: remove indirection of Rc if possible
    functions: Vec<Rc<FunctionChunk>>,

    interner: lasso::Rodeo,
}

#[inline]
fn init_frames() -> [CallFrame; MAX_FRAMES] {
    // TODO: use a crate/macro for array initialization
    let mut uninit_frames: [MaybeUninit<CallFrame>; MAX_FRAMES] =
        unsafe { MaybeUninit::uninit().assume_init() };

    for frame in &mut uninit_frames[..] {
        unsafe {
            std::ptr::write(frame.as_mut_ptr(), Default::default());
        }
    }

    unsafe { std::mem::transmute::<_, [CallFrame; MAX_FRAMES]>(uninit_frames) }
}

impl VM {
    pub(crate) fn new(program_chunk: ProgramChunk) -> Self {
        let frames = init_frames();
        let functions = program_chunk.functions.into_iter().map(Rc::new).collect();
        let mut vm = VM {
            stack: Stack::default(),
            frames,
            frame_count: 0,
            functions,
            interner: lasso::Rodeo::default(),
        };

        vm.push_frame_main();

        vm
    }

    fn run(&mut self) -> InterpretResult<ExitCode> {
        let mut frame = &mut self.frames[self.frame_count - 1];
        loop {
            let op = frame.function.chunk.get_op(frame.ip)?;
            frame.ip += 1;
            dbg!(&op);

            use Op::*;
            match op {
                PushInt => {
                    let constant = frame.read::<VMInt>();
                    self.stack.push_int(constant);
                }
                PushFloat => {
                    let constant = frame.read::<VMFloat>();
                    self.stack.push_float(constant);
                }
                PushString => {
                    let s = frame.read::<VMString>();
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
                    let num_slots = frame.read::<u8>();
                    self.stack.pop_n_discard(num_slots);
                }
                PopString => {
                    let _ = self.stack.pop_string();
                    // VMString drops, including its internal Rc for Rc-managed strings
                }
                PopRc => {
                    let _ = self.stack.pop_rc::<u8>();
                    // Rc drops, reduces strong count
                }
                PopGc => {
                    self.stack.pop_gc::<u8>();
                    // Gc drops, possibly marking for later cleanup
                }
                GetLocal => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_word_at(slot_offset);
                    self.stack.push_word(*val);
                }
                GetLocal2 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let vals = self.stack.peek_dword_at(slot_offset);

                    self.stack.push_dword(*vals);
                }
                GetLocal4 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let vals = self.stack.peek_qword_at(slot_offset);

                    self.stack.push_qword(*vals);
                }
                GetLocalN => todo!(),
                SetLocal => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let word = self.stack.peek_word();
                    self.stack.set_word_at(*word, slot_offset);
                }
                SetLocal2 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_dword();
                    let words: [Word; 2] = (*val).into();

                    for (i, word) in words.iter().enumerate() {
                        self.stack.set_word_at(*word, slot_offset + i);
                    }
                }
                SetLocal4 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_qword();
                    let words: [Word; 4] = (*val).into();

                    for (i, word) in words.iter().enumerate() {
                        self.stack.set_word_at(*word, slot_offset + i);
                    }
                }
                SetLocalN => {
                    let slot_offset = frame.read::<u16>();
                    let num_slots = frame.read::<u16>();

                    // let val = self.stack.peek_n(num_slots.into());

                    todo!()
                }
                PushLocalFunc => {
                    let function_idx = frame.read::<u32>() as usize;
                    let function = self.functions[function_idx].clone();

                    self.stack.push_rc(function);
                }
                NotBool => {
                    let top = self.stack.peek_bool();

                    match top {
                        b if b == BOOL_FALSE => self.stack.replace_top_word(BOOL_TRUE),
                        b if b == BOOL_TRUE => self.stack.replace_top_word(BOOL_FALSE),
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
                        return Err(Panic::DivideByZero);
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
                        return Err(Panic::DivideByZero);
                    }

                    let res = a / b;
                    self.stack.push_float(res);
                }
                NegateFloat => {
                    let top = self.stack.peek_float();
                    self.stack.replace_top_word(-top);
                }
                ConcatString => {
                    dbg!(&self.stack);
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    let b = frame.deref_string(&b);
                    let a = frame.deref_string(&a);

                    // TODO: optimize out extra heap allocation, create function that
                    // copies both slices of bytes to a new pointer without intermediate Rust String
                    let concatenated = alloc_string(format!("{a}{b}"));

                    self.stack.push_string(concatenated);
                }
                CallFunction => {
                    let return_slots = frame.read::<u16>() as usize;
                    let func_ptr: Rc<FunctionChunk> = self.stack.pop_rc();
                    let return_address = frame.ip;
                    let parameter_slots: usize = func_ptr.parameter_slots.into();

                    dbg!(&self.stack);
                    self.stack.shift_at_end(parameter_slots + 1);
                    dbg!(&self.stack);

                    let func_ptr_index = self.stack.len() - parameter_slots - 1;
                    let raw_ptr = Rc::into_raw(func_ptr.clone());

                    self.stack
                        .set_word_at((raw_ptr as u64).into(), func_ptr_index);
                    dbg!(&self.stack);
                    self.push_frame(func_ptr, return_slots, return_address);

                    frame = &mut self.frames[self.frame_count - 1];
                }
                CallBuiltin => {
                    let builtin_idx = frame.read::<u8>();
                    match builtin_idx {
                        0 => unreachable!("no builtin at the zero byte"),
                        PRINT_STRING => {
                            let s = self.stack.pop_string();

                            let s = frame.deref_string(&s);
                            print_string(s);
                        }
                        PRINT_INT => {
                            let int = self.stack.pop_int();
                            print_int(int);
                        }
                        PRINT_FLOAT => {
                            let float = self.stack.pop_float();
                            print_float(float)
                        }
                        PRINT_BOOL => {
                            let b = self.stack.pop_bool();
                            print_bool(b)
                        }
                        LEN_STRING => {
                            let s = self.stack.pop_string();

                            let len = len_string(s);
                            self.stack.push_int(len as VMInt);
                        }
                        _ => {}
                    }
                    // TODO: remove when reworking how builtins are called
                    // self.call_builtin(builtin_idx);
                }
                Jump => {
                    let offset = frame.read::<u32>();
                    frame.ip += offset as usize;
                }
                JumpIfFalse => {
                    let offset = frame.read::<u32>();
                    let condition = self.stack.pop_bool();
                    if condition == 0 {
                        frame.ip += offset as usize;
                    }
                }
                Return => {
                    // function pointer
                    dbg!(&self.stack);
                    let _ = self.stack.pop_rc::<u8>();
                    dbg!(&self.stack);

                    let return_address = frame.return_address;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        return Ok(self.stack.pop_int() as ExitCode);
                    }

                    frame = &mut self.frames[self.frame_count - 1];
                    frame.ip = return_address;
                    self.stack.offset = frame.stack_offset;
                }
                Noop => {}
            }
            dbg!(&self.stack);
        }
    }

    fn push_frame(
        &mut self,
        function: Rc<FunctionChunk>,
        return_slots: usize,
        return_address: usize,
    ) {
        let name = self.interner.get_or_intern(function.name());

        let args_size = function.parameter_slots as usize;
        self.stack.offset = self.stack.len() - return_slots - FUNC_PTR_SIZE - args_size;

        let function = VMFunction {
            parameter_slots: 0,
            chunk: function,
            name: Some(name),
        };
        let new_frame_idx = self.frame_count;

        self.frames[new_frame_idx] = CallFrame {
            function,
            ip: 0, // TODO: change if this becomes a pointer
            return_address,
            stack_offset: self.stack.offset,
        };
        self.frame_count += 1;
    }

    fn push_frame_main(&mut self) {
        let main = self.functions.last().unwrap().clone();

        self.stack.push_int(0); // placeholder for return code
        self.stack.push_rc(main.clone());

        // TODO: replace with real data of CLI args - []String args
        self.stack.push_int(0xDEADBEEF_i64); // first word of []String
        self.stack.push_int(0xFEEDC0DE_i64); // second word of []String

        self.push_frame(main, 1, 0);
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
        // and the builtins would need a slice that was popped from the stack and
        // also potentially return a slice to be pushed into the stack
        let frame = &mut self.frames[self.frame_count - 1];

        match i {
            0 => unreachable!("no builtin at the zero byte"),
            PRINT_STRING => {
                let s = self.stack.pop_string();

                let s = frame.deref_string(&s);
                print_string(s);
            }
            PRINT_INT => {
                let int = self.stack.pop_int();
                print_int(int);
            }

            PRINT_FLOAT => {
                let float = self.stack.pop_float();
                print_float(float)
            }
            PRINT_BOOL => {
                let b = self.stack.pop_bool();
                print_bool(b)
            }
            LEN_STRING => {
                let s = self.stack.pop_string();

                let len = len_string(s);
                self.stack.push_int(len as VMInt);
            }
            _ => {}
        }
    }
}

/// Allocates a dynamically generated String.
///
/// If the string is small, it is allocated inline. For larger string,
/// the data is allocated on the heap.
///
/// It is the caller's responsibility to free the memory for this string.
/// Normally, this will be handled by the garbage collector or with reference
/// counting.
fn alloc_string(s: String) -> VMString {
    let len = s.len();
    if len <= MAX_EMBEDDED_LENGTH {
        let len = len as u32;
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

pub type InterpretResult<T> = Result<T, Panic>;

#[derive(Debug, Clone, Default)]
pub struct CallFrame {
    /// The function and its metadata for this call frame
    function: VMFunction,

    // Review what the book said about using a pointer is faster?
    /// Instruction Pointer
    ///
    /// Points to the current bytecode Op, is mutated as bytecode is executed
    ip: usize,

    /// Instruction of the calling function to return to
    return_address: usize,

    /// The stack offset used in this frame
    // TODO: need this or is there a cleverer way?
    stack_offset: usize,
}

impl CallFrame {
    #[inline]
    fn read<T: BytecodeRead>(&mut self) -> T {
        let value = self.function.chunk.read::<T>(self.ip);
        self.ip += size_of::<T>();

        value
    }

    pub(crate) fn deref_string(&self, s: &VMString) -> String {
        let constants = self.function.chunk.borrow_constants();
        s.to_string(constants)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Panic {
    /// Bytecode encountered an invalid operation.
    /// Always indicates a compiler bug.
    InvalidOpError(InvalidOpError),

    /// Out of range access or out of bounds index
    RangeError,

    /// Division by zero
    DivideByZero,
}

impl From<InvalidOpError> for Panic {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}
