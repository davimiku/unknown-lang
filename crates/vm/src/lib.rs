use chunk::{Chunk, InvalidOpError, Op, ValueStack};
use std::mem;
use std::ops::{Add, Mul, Sub};

mod builtins;
mod macros;

// temporary for testing
const PRINT_STR_CONSTANT: u8 = 0;
const PRINT_STR: u8 = 1;
const PRINT_INT: u8 = 2;
const PRINT_FLOAT: u8 = 3;

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
            println!("{}", op.disassemble(self.chunk, self.ip));
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.ip);
        self.ip += mem::size_of::<u8>();

        byte
    }

    /// Reads an integer from the current offset in the bytecode
    /// and increments the instruction pointer.
    fn read_int(&mut self) -> i64 {
        let int = self.chunk.read_int(self.ip);
        self.ip += mem::size_of::<i64>();

        int
    }

    /// Reads a float from the current offset in the bytecode
    /// and increments the instruction pointer.
    fn read_float(&mut self) -> f64 {
        let float = self.chunk.read_float(self.ip);
        self.ip += mem::size_of::<f64>();

        float
    }

    /// Reads the stack representation of a string from the
    /// current offset in the bytecode and increments the
    /// instruction pointer.
    ///
    /// Returns the bytes read from the bytecode as (u64, u64)
    fn read_str(&mut self) -> (u64, u64) {
        let (ptr, len) = self.chunk.read_str(self.ip);
        self.ip += mem::size_of::<(u64, u64)>();

        (ptr, len)
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let op = self.chunk.get_op(self.ip);

            if let Err(err) = op {
                return Err(err.into());
            }
            let op = op.unwrap();
            self.ip += 1;

            // #[cfg(test)]
            // self.debug_print(op);

            match op {
                Op::PushInt => {
                    let constant = self.read_int();
                    self.stack.push_int(constant);
                }
                Op::PushFloat => {
                    let constant = self.read_float();
                    self.stack.push_float(constant);
                }
                Op::PushString => {
                    let (idx, len) = self.read_str();
                    self.stack.push_str_constant(idx, len);
                }
                Op::AddFloat => float_bin_op!(self, add),
                Op::SubFloat => float_bin_op!(self, sub),
                Op::MulFloat => float_bin_op!(self, mul),
                Op::DivFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    if b == 0.0 {
                        return Err(RuntimeError::RangeError);
                    }

                    let res = a / b;
                    self.stack.push_float(res);
                }
                Op::AddInt => int_bin_op!(self, add),
                Op::SubInt => int_bin_op!(self, sub),
                Op::MulInt => int_bin_op!(self, mul),
                Op::DivInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    if b == 0 {
                        return Err(RuntimeError::RangeError);
                    }

                    let res = a / b;
                    self.stack.push_int(res);
                }
                Op::Builtin => {
                    let builtin_idx = self.read_byte();
                    self.exec_builtin(builtin_idx);
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

    fn exec_builtin(&mut self, i: u8) {
        // TODO: move builtins to an array/map or something to not hard-code
        match i {
            0 => self.print_str_constant(),
            1 => {} // self.print_str
            2 => self.print_int(),
            3 => self.print_float(),
            _ => {}
        }
    }
}

pub type InterpretResult = Result<(), RuntimeError>;

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    InvalidOpError(InvalidOpError),
    RangeError,
}

impl From<InvalidOpError> for RuntimeError {
    fn from(e: InvalidOpError) -> Self {
        Self::InvalidOpError(e)
    }
}

#[cfg(test)]
mod tests {
    use chunk::{Chunk, Op};

    use hir::{BinaryOp, Database, Expr, Type};

    use super::*;

    #[test]
    fn test_manual_add_instructions() {
        let mut chunk = Chunk::new();
        let message = "hello world";

        let a = 4;
        let b = 5;

        chunk.write_int_constant(a, 0);
        chunk.write_int_constant(b, 0);
        chunk.write_op(Op::AddInt, 0);
        chunk.write_string_constant(message, 1);
        chunk.write_op(Op::Ret, 2);
        chunk.disassemble("Ret");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.pop_int(), message.len() as i64);
        assert_eq!(vm.stack.pop_int(), 0); // index of message
        assert_eq!(vm.stack.pop_int(), a + b);
    }

    #[test]
    fn test_int_add() {
        let mut chunk = Chunk::new();
        let mut database = Database::default();

        let a = 4;
        let b = 5;

        let lhs = database.alloc_expr(Expr::IntLiteral(a), None);
        let rhs = database.alloc_expr(Expr::IntLiteral(b), None);

        let expr = Expr::Binary {
            op: BinaryOp::Add,
            lhs,
            lhs_type: Type::IntLiteral(a),
            rhs,
            rhs_type: Type::IntLiteral(b),
        };

        chunk.write_expr(&expr, &database);
        chunk.write_op(Op::Ret, 2);
        chunk.disassemble("IntAdd");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.pop_int(), a + b);
    }

    #[test]
    fn test_float_add() {
        let mut chunk = Chunk::new();
        let mut database = Database::default();

        let a = 4.4;
        let b = 5.5;

        let lhs = database.alloc_expr(Expr::FloatLiteral(a), None);
        let rhs = database.alloc_expr(Expr::FloatLiteral(b), None);

        let expr = Expr::Binary {
            op: BinaryOp::Add,
            lhs,
            lhs_type: Type::FloatLiteral(a),
            rhs,
            rhs_type: Type::FloatLiteral(b),
        };

        chunk.write_expr(&expr, &database);
        chunk.write_op(Op::Ret, 2);
        chunk.disassemble("FloatAdd");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.pop_float(), a + b);
    }

    #[test]
    fn test_print_int() {
        // "print 1"
        let mut chunk = Chunk::new();

        chunk.write_int_constant(1, 0);
        chunk.write_builtin(PRINT_INT, 0);
        // write arity / number of args
        // push function object?
        chunk.write_op(Op::Ret, 1);

        chunk.disassemble("print 1");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_print_float() {
        // "print 1.23"
        let mut chunk = Chunk::new();

        chunk.write_float_constant(1.23, 0);
        chunk.write_builtin(PRINT_FLOAT, 0);
        // write arity / number of args
        // push function object?

        chunk.write_op(Op::Ret, 1);
        chunk.disassemble("print 1.23");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_print_int_add() {
        // "print (1 + 2)"
        let mut chunk = Chunk::new();

        chunk.write_int_constant(1, 123);
        chunk.write_int_constant(2, 123);
        chunk.write_op(Op::AddInt, 123);

        chunk.write_builtin(PRINT_INT, 123);
        // write arity (operand)
        // push function object
        chunk.write_op(Op::Ret, 123);

        chunk.disassemble("print (1 + 2)");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_sandbox_run() {
        let mut chunk = Chunk::new();

        chunk.write_float_constant(1.2, 123);
        chunk.write_float_constant(3.4, 123);
        chunk.write_op(Op::AddFloat, 123);

        chunk.write_float_constant(5.6, 123);
        chunk.write_op(Op::DivFloat, 123);

        chunk.write_float_constant(1.0, 123);
        chunk.write_op(Op::SubFloat, 123);

        chunk.write_float_constant(20.0, 123);
        chunk.write_op(Op::MulFloat, 123);

        chunk.write_op(Op::Ret, 123);

        chunk.disassemble("test chunk");

        super::run(&chunk).unwrap();
    }
}
