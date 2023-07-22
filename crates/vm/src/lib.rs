use arrayvec::ArrayVec;
use builtins::{
    len_string, print_bool, print_float, print_int, print_string, LEN_STRING, PRINT_BOOL,
    PRINT_FLOAT, PRINT_INT, PRINT_STRING,
};
use exitcode::ExitCode;
use stack::Stack;
use std::mem::size_of;
use std::ops::{Add, Mul, Sub};
use std::rc::Rc;
use vm_boxed_types::VMFunction;
use vm_codegen::{
    AllocArrayOperand, BytecodeRead, FunctionChunk, IntoStringOperand, InvalidOpError, Op,
    ProgramChunk, PushStringOperand,
};
use vm_string::VMString;
use vm_types::words::{DWord, Word};
use vm_types::{word_size_of, VMArray, VMBool, VMFloat, VMInt};

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

#[derive(Debug)]
pub struct VM {
    /// Program stack containing values
    stack: Stack,

    /// Active call frames
    frames: ArrayVec<CallFrame, MAX_FRAMES>,

    /// Program bytecode, where each function has a chunk
    functions: Vec<FunctionChunk>,

    interner: lasso::Rodeo,
}

impl VM {
    pub(crate) fn new(program_chunk: ProgramChunk) -> Self {
        let mut vm = VM {
            stack: Stack::default(),
            frames: ArrayVec::new(),
            functions: program_chunk.functions,
            interner: lasso::Rodeo::default(),
        };

        vm.push_frame_main()
            .expect("main call frame to be successfully initialized");

        vm
    }

    fn run(&mut self) -> InterpretResult<ExitCode> {
        // ensures self.functions isn't mutated
        // TODO: decide if making self.functions a slice is worth the syntax cost of lifetime annotations
        let _ = &self.functions;
        // TODO: unwrap_unchecked should be fine because frames starts with len==1 (`main`)
        let mut frame = self.frames.last_mut().unwrap();
        loop {
            // Safety: VM owns FunctionChunk memory and lifetime is longer than this pointer
            let chunk = unsafe { &*frame.function.chunk };
            let op = chunk.get_op(frame.ip)?;
            frame.ip += 1;

            // #[cfg(debug_stack)]
            // dbg!(op);

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
                    let PushStringOperand {
                        bytes_len: len,
                        offset,
                    } = frame.read::<PushStringOperand>();
                    let start = offset as usize;
                    let end = (offset + len) as usize;
                    let constants =
                        unsafe { frame.function.chunk.as_ref().unwrap().borrow_constants() };
                    let bytes = &constants[start..end];

                    let string = unsafe { String::from_utf8_unchecked(bytes.to_owned()) };

                    self.stack.push_string(VMString::new(string));
                }
                AllocArray => {
                    let AllocArrayOperand { len, el_size } = frame.read::<AllocArrayOperand>();

                    if len == 0 {
                        self.stack.push_dword(DWord::default());
                    } else {
                        let values = self.stack.pop_n_as_vec(len as usize * el_size as usize);
                        let ptr = Rc::new(values);
                        let array = VMArray { ptr, len, el_size };

                        self.stack.push_array(array);
                    }
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
                    let num_slots = frame.read::<u16>();
                    self.stack.pop_n_discard(num_slots as usize);
                }
                PopString => {
                    let _ = self.stack.pop_string();
                    // VMString drops, including its internal Rc for Rc-managed strings
                }
                PopArray => {
                    let _ = self.stack.pop_array();
                    // VMArray drops, including its internal Rc for the data
                }
                PopRc => {
                    let _ = self.stack.pop_rc::<u8>();
                    // Rc drops, reduces strong count
                }
                PopGc => {
                    let _ = self.stack.pop_gc::<u8>();
                    // Gc drops, possibly marking for later cleanup
                }
                GetLocal => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_word_at(slot_offset);
                    self.stack.push_word(*val);
                }
                GetLocal2 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_dword_at(slot_offset);

                    self.stack.push_dword(*val);
                }
                GetLocal4 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_qword_at(slot_offset);

                    self.stack.push_qword(*val);
                }
                GetLocalN => todo!(),
                GetLocalString => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = *self.stack.peek_dword_at(slot_offset);
                    let string = VMString::from_copy(val.into());

                    self.stack.push_string(string);
                }
                GetArrayIndex => {
                    let index = self.stack.pop_int();
                    if index < 0 {
                        return Err(Panic::IndexError);
                    }
                    let index = index as usize;
                    let VMArray { ptr, len, el_size } = self.stack.pop_array();
                    if index >= len as usize {
                        return Err(Panic::IndexError);
                    }
                    let el_size = el_size as usize;

                    let start = index * el_size;
                    let end = start + el_size;

                    self.stack.push_slice(&ptr[start..end]);
                }
                SetLocal => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let word = self.stack.peek_word();
                    self.stack.set_word_at(*word, slot_offset);
                }
                SetLocal2 => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = self.stack.peek_dword();

                    self.stack.set_dword_at(*val, slot_offset);
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
                    let _slot_offset = frame.read::<u16>();
                    let _num_slots = frame.read::<u16>();

                    // let val = self.stack.peek_n(num_slots.into());

                    todo!()
                }
                SetLocalString => {
                    let slot_offset = frame.read::<u16>() as usize;
                    let val = *self.stack.peek_dword();
                    let string = VMString::from_copy(val.into());

                    self.stack.set_dword_at(string.into(), slot_offset);
                }
                SetArrayIndex => {}
                PushLocalFunc => {
                    let idx = frame.read::<u32>() as usize;
                    let function = &self.functions[idx] as *const FunctionChunk;

                    self.stack.push_raw_ptr(function);
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
                EqInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();

                    self.stack.push_bool(a == b);
                }
                NeInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();

                    self.stack.push_bool(a != b);
                }
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
                EqFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();

                    self.stack.push_bool(a == b);
                }
                NeFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();

                    self.stack.push_bool(a != b);
                }
                IntoString => {
                    let kind = frame.read::<IntoStringOperand>();
                    match kind {
                        IntoStringOperand::Bool => {
                            let b = self.stack.pop_bool();
                            if b == 0 {
                                self.stack.push_string(VMString::new("false".to_string()));
                            } else {
                                self.stack.push_string(VMString::new("true".to_string()));
                            }
                        }
                        IntoStringOperand::Float => {
                            let float = self.stack.pop_float();
                            // TODO: consider using the `ryu` crate
                            self.stack.push_string(VMString::new(float.to_string()));
                        }
                        IntoStringOperand::Int => {
                            let int = self.stack.pop_int();
                            self.stack.push_string(VMString::new(int.to_string()));
                        }
                        IntoStringOperand::Array => {
                            todo!()
                        }
                    }
                }
                ConcatString => {
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    self.stack.push_string(a + b);
                }
                EqString => {
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    self.stack.push_bool(a == b);
                }
                NeString => {
                    let b = self.stack.pop_string();
                    let a = self.stack.pop_string();

                    self.stack.push_bool(a != b);
                }
                CallFunction => {
                    let return_slots = frame.read::<u16>() as usize;
                    let func_ptr = self.stack.pop_func_ptr();
                    let return_address = frame.ip;
                    // Safety: VM owns FunctionChunk memory and lifetime is longer than this pointer
                    let parameter_slots: usize = unsafe { (*func_ptr).parameter_slots.into() };

                    self.stack.shift_at_end(parameter_slots + FUNC_PTR_SIZE);

                    let func_ptr_index = self.stack.len() - parameter_slots - FUNC_PTR_SIZE;

                    self.stack
                        .set_word_at((func_ptr as u64).into(), func_ptr_index);

                    self.push_frame(func_ptr, return_slots, return_address)?;

                    // Safety: a frame was just pushed, so there is a last frame to unwrap
                    frame = unsafe { self.frames.last_mut().unwrap_unchecked() };
                }
                CallBuiltin => {
                    let builtin_idx = frame.read::<u8>();
                    match builtin_idx {
                        0 => unreachable!("no builtin at the zero byte"),
                        PRINT_STRING => {
                            let s = self.stack.pop_string();

                            print_string(s.to_string());
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
                    let _ = self.stack.pop_func_ptr();

                    let return_address = frame.return_address;
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(self.stack.pop_int() as ExitCode);
                    }

                    // Safety: self.frames was just checked to be not empty
                    frame = unsafe { self.frames.last_mut().unwrap_unchecked() };
                    frame.ip = return_address;
                    self.stack.offset = frame.stack_offset;
                }
                Noop => {}
            }

            // #[cfg(debug_stack)]
            // dbg!(&self.stack);
        }
    }

    fn push_frame(
        &mut self,
        function: *const FunctionChunk,
        return_slots: usize,
        return_address: usize,
    ) -> Result<(), Panic> {
        // Safety: VM owns FunctionChunk memory and lifetime is longer than this pointer
        let function = unsafe { &*function };

        let name = self.interner.get_or_intern(function.name());
        let args_size = function.parameter_slots as usize;

        self.stack.offset = self.stack.len() - return_slots - FUNC_PTR_SIZE - args_size;

        let function = VMFunction {
            parameter_slots: 0,
            chunk: function,
            name: Some(name),
        };

        self.frames
            .try_push(CallFrame {
                function,
                ip: 0, // TODO: change if this becomes a pointer
                return_address,
                stack_offset: self.stack.offset,
            })
            .map_err(|_| Panic::CallFrameOverflowError)
    }

    fn push_frame_main(&mut self) -> Result<(), Panic> {
        let main = self.functions.last().unwrap();

        self.stack.push_int(0); // placeholder for return code
        self.stack.push_raw_ptr(main as *const FunctionChunk);

        // TODO: replace with real data of CLI args - []String args
        self.stack.push_int(0xDEADBEEF_i64); // first word of []String
        self.stack.push_int(0xFEEDC0DE_i64); // second word of []String

        self.push_frame(main, word_size_of::<VMInt>(), 0)
    }

    fn _call_builtin(&mut self, i: u8) {
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

        match i {
            0 => unreachable!("no builtin at the zero byte"),
            PRINT_STRING => {
                let s = self.stack.pop_string();

                print_string(s.to_string());
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
        // Safety: VM owns FunctionChunk memory and lifetime is longer than this pointer
        let chunk = unsafe { &*self.function.chunk };

        let value = chunk.read::<T>(self.ip);
        self.ip += size_of::<T>();

        value
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Panic {
    /// Bytecode encountered an invalid operation.
    /// Always indicates a compiler bug.
    InvalidOpError(InvalidOpError),

    /// The number of Call Frames exceeded the available amount
    CallFrameOverflowError,

    /// Out of bounds index
    IndexError,

    /// Division by zero
    DivideByZero,
}

impl From<InvalidOpError> for Panic {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}
