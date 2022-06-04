use crate::chunk::{Chunk, Op};
use crate::value::ValueStack;

pub(crate) struct VM<'a> {
    /// Chunk of bytecode to run
    chunk: &'a Chunk,

    /// Instruction Pointer
    ip: usize, // TODO: review what the book says about a pointer being faster, needs unsafe?

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

    fn run(&mut self) -> InterpretResult {
        let result = loop {
            let op = self.chunk.get_op(self.ip);

            // debug prints
            #[cfg(test)]
            {
                print!("     ");
                for i in 0..self.stack.current {
                    print!("[ ");
                    print!("{}", self.stack.values[i]);
                    print!(" ]")
                }
                print!("\n");
                println!("{}", op.disassemble(self.chunk));
            }

            match op {
                Op::Constant(i) => {
                    let constant = self.chunk.get_constant(*i);
                    self.stack.push(constant);
                }
                // TODO: possibly use macros
                Op::FAdd => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a + b);
                }
                Op::FSubtract => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a - b);
                }
                Op::FMultiply => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a * b);
                }
                Op::FDivide => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a / b);
                }
                Op::Negate => {
                    let value = self.stack.pop();
                    self.stack.push(-value);
                }
                Op::Return => {
                    println!("{}", self.stack.pop());

                    break InterpretResult::Ok;
                }
            }

            self.ip += 1;
        };

        result
    }
}

pub(crate) enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
