use codegen::{Chunk, InvalidOpError, Op, Readable};
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

    pub(crate) fn interpret(&mut self) -> InterpretResult {
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
                    let constant = self.read::<i64>();
                    self.stack.push_int(constant);
                }
                PushFloat => {
                    let constant = self.read::<f64>();
                    self.stack.push_float(constant);
                }
                PushString => {
                    let (idx, len) = self.read_str();
                    self.stack.push_str_constant(idx, len);
                }
                AddFloat => float_bin_op!(self, add),
                SubFloat => float_bin_op!(self, sub),
                MulFloat => float_bin_op!(self, mul),
                DivFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    if b == 0.0 {
                        return Err(RuntimeError::RangeError);
                    }

                    let res = a / b;
                    self.stack.push_float(res);
                }
                NegateFloat => {
                    let top = self.stack.peek_float();
                    self.stack.replace_top((-top).to_le_bytes());
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
                    self.stack.replace_top((-top).to_le_bytes());
                }
                Builtin => {
                    let builtin_idx = self.read_byte();
                    self.exec_builtin(builtin_idx);
                }
                Ret => {
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

    const PRINT_STR_CONSTANT: u8 = 0;
    const PRINT_STR: u8 = 1;
    const PRINT_INT: u8 = 2;
    const PRINT_FLOAT: u8 = 3;
    const PRINT_BOOL: u8 = 4;

    use codegen::{Chunk, Op};

    use hir::{BinaryExpr, BinaryOp, Context, Expr};

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
    #[ignore = "failing because typecheck isn't implemented"]
    fn test_int_add() {
        let mut chunk = Chunk::new();
        let mut context = Context::default();

        let a = 4;
        let b = 5;

        let lhs = context.alloc_expr(Expr::IntLiteral(a), None);
        let rhs = context.alloc_expr(Expr::IntLiteral(b), None);
        let op = BinaryOp::Add;

        let expr = Expr::Binary(BinaryExpr { op, lhs, rhs });
        chunk.write_expr(&expr, &context);

        chunk.write_op(Op::Ret, 2);
        chunk.disassemble("IntAdd");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.pop_int(), a + b);
    }

    #[test]
    #[ignore = "failing because typecheck isn't implemented"]
    fn test_float_add() {
        let mut chunk = Chunk::new();
        let mut context = Context::default();

        let a = 4.4;
        let b = 5.5;

        let lhs = context.alloc_expr(Expr::FloatLiteral(a), None);
        let rhs = context.alloc_expr(Expr::FloatLiteral(b), None);
        let op = BinaryOp::Add;

        let expr = Expr::Binary(BinaryExpr { op, lhs, rhs });
        chunk.write_expr(&expr, &context);

        chunk.write_op(Op::Ret, 2);
        chunk.disassemble("FloatAdd");

        let mut vm = VM::new(&chunk);
        vm.interpret().unwrap();

        assert_eq!(vm.stack.pop_float(), a + b);
    }

    #[test]
    fn test_print_int() {
        // `print 1`
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
        // `print 1.23`
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
    fn test_print_string() {
        // `print "hello"`
        let mut chunk = Chunk::new();

        chunk.write_string_constant("hello", 0);
        chunk.write_builtin(PRINT_STR_CONSTANT, 0);

        chunk.write_op(Op::Ret, 1);
        chunk.disassemble("print \"hello\"");

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
    fn test_int_negate() {
        // "print -1"
        // normally the -1 would be part of the IntLiteral but this is a test

        let mut chunk = Chunk::new();

        chunk.write_int_constant(12345, 123);
        chunk.write_op(Op::NegateInt, 123);

        chunk.write_builtin(PRINT_INT, 123);
        // write arity (operand)
        // push function object
        chunk.write_op(Op::Ret, 123);

        chunk.disassemble("print -12345");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_float_negate() {
        // "print -1"
        // normally the -1 would be part of the IntLiteral but this is a test

        let mut chunk = Chunk::new();

        chunk.write_float_constant(12.345, 123);
        chunk.write_op(Op::NegateFloat, 123);

        chunk.write_builtin(PRINT_FLOAT, 123);
        // write arity (operand)
        // push function object
        chunk.write_op(Op::Ret, 123);

        chunk.disassemble("print -12.345");

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
