use std::ops::{Add, Div, Mul, Sub};

use chunk::{Chunk, Op, ValueStack};

use crate::{arithmetic, builtins::print};

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

    #[cfg(test)]
    fn debug_print(&self, op: &Op) {
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

        let result = loop {
            let op = self.chunk.get_op(self.ip);

            #[cfg(test)]
            self.debug_print(op);

            match op {
                Op::IConstant(idx) => {
                    let constant = self.chunk.get_int(*idx);
                    println!("IConstant({}, {})", *idx, constant);
                    self.stack.push_int(constant);
                }
                Op::FConstant(idx) => {
                    let constant = self.chunk.get_float(*idx);
                    println!("FConstant({}, {})", *idx, constant);
                    self.stack.push_float(constant);
                }
                Op::SConstant(idx) => {
                    let constant = self.chunk.get_str(*idx);
                    println!("SConstant({}, {})", *idx, constant);
                    self.stack.push_str(constant);
                }
                Op::FAdd => {
                    float_arithmetic!(Add::add);
                }
                Op::FSub => {
                    float_arithmetic!(Sub::sub);
                }
                Op::FMul => {
                    float_arithmetic!(Mul::mul);
                }
                Op::FDiv => {
                    float_arithmetic!(Div::div);
                }
                Op::IAdd => {
                    int_arithmetic!(Add::add);
                }
                Op::ISub => {
                    int_arithmetic!(Sub::sub);
                }
                Op::IMul => {
                    int_arithmetic!(Mul::mul);
                }
                Op::IDiv => {
                    int_arithmetic!(Div::div);
                }
                Op::Builtin(i) => self.exec_builtin(*i),
                Op::Ret => {
                    // TODO: better output for debugging?
                    // dbg!(self.stack.pop::<[u8; 8]>());

                    break InterpretResult::Ok;
                }
                _ => unimplemented!(),
            }

            self.ip += 1;
        };

        result
    }

    fn exec_builtin(&mut self, i: u32) {
        // TODO: move builtins to an array/map or something to not hard-code
        if i == 0 {
            // print
            // FIXME: this *only* works for constants and the Op being right after the print call
            // instead, it should pop a StrValue from the stack, follow the pointer, and print that
            self.ip += 1;
            let op = self.chunk.get_op(self.ip).clone();
            print(op, self.chunk)
        }
    }
}

pub(crate) enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
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
        chunk.write_int_constant(b, 0);
        chunk.write(Op::IAdd, 0);
        chunk.write(Op::Ret, 1);

        let mut vm = VM::new(&chunk);
        vm.interpret();

        assert_eq!(vm.stack.peek_int(), 7);
    }
}
