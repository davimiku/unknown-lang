use chunk::{Chunk, Op, ValueStack};

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

    #[cfg(test)]
    fn debug_print(&self, op: &Op) {
        {
            print!("     ");
            for i in 0..self.stack.current {
                print!("[ ");
                print!("{:?}", self.stack.peek_at(i));
                print!(" ]")
            }
            println!();
            println!("{}", op.disassemble(self.chunk));
        }
    }

    fn run(&mut self) -> InterpretResult {
        let result = loop {
            let op = self.chunk.get_op(self.ip);

            // #[cfg(test)]
            // self.debug_print(op);

            match op {
                Op::Constant(i) => {
                    let constant = self.chunk.get_int(*i);
                    self.stack.push(constant.to_le_bytes());
                }
                Op::FConstant(i) => {
                    let constant = self.chunk.get_float(*i);
                    println!("FConstant({}, {})", *i, constant);
                    self.stack.push(constant.to_le_bytes());
                }
                // TODO: investigate macros to reduce boilerplate
                Op::FAdd => {
                    let b: [u8; 8] = self.stack.pop();
                    let b = f64::from_le_bytes(b);

                    let a: [u8; 8] = self.stack.pop();
                    let a = f64::from_le_bytes(a);

                    self.stack.push((a + b).to_le_bytes());
                }
                Op::FSub => {
                    let b: [u8; 8] = self.stack.pop();
                    let b = f64::from_le_bytes(b);

                    let a: [u8; 8] = self.stack.pop();
                    let a = f64::from_le_bytes(a);

                    self.stack.push((a - b).to_le_bytes());
                }
                Op::FMul => {
                    let b: [u8; 8] = self.stack.pop();
                    let b = f64::from_le_bytes(b);

                    let a: [u8; 8] = self.stack.pop();
                    let a = f64::from_le_bytes(a);

                    self.stack.push((a * b).to_le_bytes());
                }
                Op::FDiv => {
                    let b: [u8; 8] = self.stack.pop();
                    let b = f64::from_le_bytes(b);

                    let a: [u8; 8] = self.stack.pop();
                    let a = f64::from_le_bytes(a);

                    // TODO: runtime error for 0 denominator

                    self.stack.push((a / b).to_le_bytes());
                }
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
}

// TODO: how to use this inside a function
#[macro_export]
macro_rules! pop_floats {
    () => {
        let b: [u8; 8] = self.stack.pop();
        let b = f64::from_le_bytes(b);

        let a: [u8; 8] = self.stack.pop();
        let a = f64::from_le_bytes(a);
    };
}

pub(crate) enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
