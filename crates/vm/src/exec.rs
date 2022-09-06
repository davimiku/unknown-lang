use std::ops::{Add, Mul, Sub};

use chunk::{Chunk, InvalidOpError, Op, ValueStack};

use crate::{arithmetic, pop_two};

#[derive(Debug)]
pub(crate) struct VM<'a> {
    /// Chunk of bytecode to run
    chunk: &'a Chunk<'a>,

    /// Instruction Pointer
    ip: usize, // TODO: review what the book says about a pointer being faster, needs unsafe?

    /// Program stack containing values
    stack: ValueStack,
}

impl<'a> VM<'a> {
    pub(crate) fn new(chunk: &'a Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Default::default(),
        }
    }

    pub(crate) fn interpret(&mut self) -> InterpretResult {
        // the book separates interpret from run, we may not need to
        self.run()
    }

    #[cfg(debug_assertions)]
    fn debug_print(&self, op: Op) {
        {
            print!("     ");
            println!("{:?}", self.stack);
            println!();
            println!("{}", op.disassemble(self.chunk));
        }
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! int_arithmetic {
            ($F: path) => {
                arithmetic!(self, i64, $F);
            };
        }
        macro_rules! float_arithmetic {
            ($F: path) => {
                arithmetic!(self, f64, $F);
            };
        }

        loop {
            let op = self.chunk.get_op(self.ip);

            if let Err(err) = op {
                return Err(InterpretError::RuntimeError(err.into()));
            }
            let op = op.unwrap();
            self.ip += 1;

            #[cfg(test)]
            self.debug_print(op);

            match op {
                Op::PushInt => {
                    // TODO: move read_int to self which increments the self.ip
                    let constant = self.chunk.read_int(self.ip);
                    println!("PushInt({}, {})", self.ip, constant);
                    self.stack.push_int(constant);
                    self.ip += 8;
                }
                Op::PushFloat => {
                    // TODO: move read_float to self which increments the self.ip
                    let constant = self.chunk.read_float(self.ip);
                    println!("PushFloat({}, {})", self.ip, constant);
                    self.stack.push_float(constant);
                    self.ip += 8;
                }
                Op::PushString => {
                    todo!();
                    // let constant = self.chunk.get_str(*idx);
                    // println!("SConstant({}, {})", *idx, constant);
                    // self.stack.push_str(constant);
                }
                Op::AddFloat => {
                    float_arithmetic!(Add::add);
                }
                Op::SubFloat => {
                    float_arithmetic!(Sub::sub);
                }
                Op::MulFloat => {
                    float_arithmetic!(Mul::mul);
                }
                Op::DivFloat => {
                    let (a, b) = pop_two!(self, f64);
                    if b == 0.0 {
                        return Err(InterpretError::RuntimeError(RuntimeError::RangeError));
                    }

                    let res = (a / b).to_le_bytes();
                    self.stack.push(res);
                }
                Op::AddInt => {
                    int_arithmetic!(Add::add);
                }
                Op::SubInt => {
                    int_arithmetic!(Sub::sub);
                }
                Op::MulInt => {
                    int_arithmetic!(Mul::mul);
                }
                Op::DivInt => {
                    let (a, b) = pop_two!(self, i64);
                    if b == 0 {
                        return Err(InterpretError::RuntimeError(RuntimeError::RangeError));
                    }

                    let res = (a / b).to_le_bytes();
                    self.stack.push(res);
                }
                Op::Builtin => {
                    todo!();
                    // self.exec_builtin(*i)
                }
                Op::Ret => {
                    // TODO: better output for debugging?
                    // dbg!(self.stack.pop::<[u8; 8]>());

                    // TODO: return the top value of the stack maybe?
                    break Ok(());
                }
                _ => unimplemented!("unimplemented Op {}", op as u8),
            }
        }
    }

    fn exec_builtin(&mut self, i: u32) {
        // TODO: move builtins to an array/map or something to not hard-code
        if i == 0 {
            // print
            // FIXME: this *only* works for constants and the Op being right after the print call
            // instead, it should pop a StrValue from the stack, follow the pointer, and print that
            // self.ip += 1;
            // let op = self.chunk.get_op(self.ip)?;
            // print(op, self.chunk)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum InterpretError {
    CompileError,
    RuntimeError(RuntimeError),
}

pub type InterpretResult = Result<(), InterpretError>;

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    InvalidOpError(InvalidOpError),
    RangeError,
}

impl From<RuntimeError> for InterpretError {
    fn from(e: RuntimeError) -> Self {
        Self::RuntimeError(e)
    }
}

impl From<InvalidOpError> for RuntimeError {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}

#[cfg(test)]
mod tests {

    use chunk::{Chunk, Op};

    use super::VM;

    #[test]
    fn int_add() {
        let mut chunk = Chunk::new();

        let a = 2;
        let b = 5;

        chunk.write_int_constant(a, 0);
        chunk.disassemble("first int");
        chunk.write_int_constant(b, 0);
        chunk.disassemble("second int");

        chunk.write_op(Op::AddInt, 0);
        chunk.disassemble("AddInt");

        chunk.write_op(Op::Ret, 1);
        chunk.disassemble("Ret");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.peek_int(), 7);
    }
}
