//! Takes an HIR node + maps of values & types, produces a
//! bytecode chunk (vector of bytes)
pub use op::{InvalidOpError, Op};

use std::convert::TryInto;
use std::mem::size_of;
use std::str;

use hir::{
    BinaryExpr, BinaryOp, BlockExpr, Context, Expr, LocalDef, Stmt, Type, UnaryExpr, UnaryOp,
};

mod disassemble;
mod op;

/// A VM "word" is 8 bytes (64 bits)
type Word = [u8; 8];

#[derive(Debug, Default)]
pub struct Chunk {
    /// bytecode contained in the chunk
    code: Vec<u8>,

    /// string constants in the "data" section
    str_constants: Vec<u8>,

    /// tracking of the source code lines corresponding to op codes
    // FIXME: the book author calls this a "braindead" approach.
    // Find a better way to store source code location for bytecodes
    lines: Vec<u32>,
}

// TODO: make all pub(crate)
impl Chunk {
    pub fn new() -> Chunk {
        Default::default()
    }
}

// Functions to write to the Chunk
impl Chunk {
    pub fn write_stmt(&mut self, stmt: Stmt, context: &Context) {
        match stmt {
            Stmt::VariableDef(local_def) => todo!(),
            Stmt::Expr(idx) => {
                let expr = context.expr(idx);
                self.write_expr(expr, context)
            }
        }
    }

    pub fn write_expr(&mut self, expr: &Expr, context: &Context) {
        use Expr::*;

        match expr {
            BoolLiteral(b) => self.write_bool_constant(*b, 1),
            FloatLiteral(f) => self.write_float_constant(*f, 1),
            IntLiteral(i) => self.write_int_constant(*i, 1),
            StringLiteral(s) => self.write_string_constant(s, 1),

            Binary(expr) => self.write_binary_expr(expr, context),
            Unary(expr) => self.write_unary_expr(expr, context),
            VariableRef { name } => todo!(),
            Call { path, args } => todo!(),
            Function {
                params,
                body,
                return_type_annotation,
            } => todo!(),

            Block(BlockExpr { stmts }) => todo!(),

            // This should be unreachable, codegen should never start if lowering failed
            // TODO: add some machinery for some debug output. It should still panic
            Empty => unreachable!(),
        }
    }

    fn write_binary_expr(&mut self, expr: &BinaryExpr, context: &Context) {
        let BinaryExpr { op, lhs, rhs } = expr;
        let lhs_type = context.type_of(*lhs);
        let lhs = context.expr(*lhs);
        let rhs = context.expr(*rhs);

        self.write_expr(lhs, context);
        self.write_expr(rhs, context);

        use BinaryOp::*;
        use Type::*;
        match op {
            Add => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::AddFloat, 1),
                Int | IntLiteral(_) => self.write_op(Op::AddInt, 1),
                _ => unreachable!(),
            },
            Sub => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::SubFloat, 1),
                Int | IntLiteral(_) => self.write_op(Op::SubInt, 1),
                _ => unreachable!(),
            },
            Mul => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::MulFloat, 1),
                Int | IntLiteral(_) => self.write_op(Op::MulInt, 1),
                _ => unreachable!(),
            },
            Div => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::DivFloat, 1),
                Int | IntLiteral(_) => self.write_op(Op::DivInt, 1),
                _ => unreachable!(),
            },
            Rem => todo!(),
            Exp => todo!(),
            Path => todo!(),
        }
    }

    fn write_unary_expr(&mut self, expr: &UnaryExpr, context: &Context) {
        let UnaryExpr { op, expr: idx } = expr;
        let expr_type = context.type_of(*idx);
        let expr = context.expr(*idx);

        self.write_expr(expr, context);

        use Type::*;
        use UnaryOp::*;
        match op {
            Neg => match expr_type {
                Float => self.write_op(Op::NegateFloat, 1),
                Int => self.write_op(Op::NegateInt, 1),
                _ => unreachable!(),
            },
            Not => todo!(),
        }
    }

    // fn write_block_expr(&mut self, context: &)

    fn write_local_def(&mut self, local_def: &LocalDef) {
        let name = local_def.name();
        let value = local_def.value;
    }

    pub fn write_op(&mut self, op: Op, line: u32) {
        self.code.push(op as u8);
        self.lines.push(line);
    }

    fn write_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        self.code.extend(bytes);
    }

    pub fn write_int_constant(&mut self, value: i64, line: u32) {
        self.write_op(Op::PushInt, line);

        let bytes = value.to_le_bytes();
        self.write_bytes(&bytes)
    }

    pub fn write_float_constant(&mut self, value: f64, line: u32) {
        self.write_op(Op::PushFloat, line);

        let bytes = value.to_le_bytes();
        self.write_bytes(&bytes)
    }

    pub fn write_bool_constant(&mut self, value: bool, line: u32) {
        match value {
            true => self.write_op(Op::PushTrue, line),
            false => self.write_op(Op::PushFalse, line),
        }
    }

    pub fn write_string_constant(&mut self, value: &str, line: u32) {
        let idx: Word = self.str_constants.len().to_le_bytes();
        let len: Word = value.len().to_le_bytes();

        let bytes = value.as_bytes();
        self.str_constants.extend(bytes);

        self.write_op(Op::PushString, line);
        self.write_bytes(&idx);
        self.write_bytes(&len);
    }

    pub fn write_builtin(&mut self, builtin_idx: u8, line: u32) {
        self.write_op(Op::Builtin, line);
        self.write_byte(builtin_idx);
    }

    pub fn shrink_to_fit(&mut self) {
        self.code.shrink_to_fit();
        self.lines.shrink_to_fit();
        self.str_constants.shrink_to_fit();
    }
}

// Functions to read from the Chunk.
// TODO: Modify or implement `unsafe` functions without bounds checking
// for release builds after further testing.
impl Chunk {
    /// Gets the Op at the given index.
    #[inline]
    pub fn get_op(&self, i: usize) -> Result<Op, InvalidOpError> {
        self.code[i].try_into()
    }

    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        self.code.as_ptr().add(offset).cast::<T>().read_unaligned()
    }

    /// Reads bytes from the bytecode and returns as T.
    ///
    /// Panics if there are not enough bytes to read.
    #[inline]
    pub fn read<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        assert!(offset + size_of::<T>() - 1 < self.code.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }

    /// Reads the stack representation of a String `(u64, u64)`
    /// from the bytecode starting at the given offset.
    ///
    /// String constants: represents (idx, len) in the string
    /// constants vector (`Vec<u8>`).
    ///
    /// Dynamic strings: represents (ptr, len) for the heap-allocated
    /// bytes.
    ///
    /// Panics if there are not enough bytes to read from the offset.
    pub fn read_str(&self, offset: usize) -> (u64, u64) {
        let (first, second) = self.read::<(u64, u64)>(offset);

        (first, second)
    }

    /// Given the stack representation of a String constant,
    /// return the corresponding String from the constants pool.
    pub fn get_str_constant(&self, idx: u64, len: u64) -> &str {
        let start = idx as usize;
        let end = (idx + len) as usize;
        let bytes = self
            .str_constants
            .get(start..end)
            .expect("should be enough bytes to slice");

        // Safety: The compiler must have only allocated valid UTF-8
        // bytes during codegen.
        #[cfg(not(debug_assertions))]
        unsafe {
            str::from_utf8_unchecked(bytes)
        }

        #[cfg(debug_assertions)]
        str::from_utf8(bytes).expect("bytes should be valid UTF-8")
    }
}

// Functions for debugging the Chunk
#[cfg(debug_assertions)]
impl Chunk {
    /// Prints the Chunk in a disassembled format
    /// for human-reading.
    ///
    /// This format is not stable and should not be depended on.
    // TODO: Vertically align each column
    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        let mut op_idx = 0;
        while offset < self.code.len() {
            let op = self.code[offset];
            let op: Op = op.try_into().expect("byte should be convertible to Op");
            let line = self.lines[op_idx];

            let line = if offset > 0 && line == self.lines[op_idx - 1] {
                "   | ".to_string()
            } else {
                format!(" {line:04}")
            };
            print!("{offset:03} {line}  ");
            offset = op.disassemble(self, offset);
            println!();

            op_idx += 1;
        }
        println!();
    }
}

/// Types implementing this trait may be read from the bytecode.
///
/// # Safety
///
/// Types implementing this trait must not implement Drop.
pub unsafe trait Readable {}

unsafe impl Readable for u8 {}
unsafe impl Readable for i64 {}
unsafe impl Readable for f64 {}
unsafe impl Readable for (u64, u64) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_byte() {
        let mut chunk = Chunk::new();

        let input = 255;

        chunk.write_byte(input);
        let actual = chunk.read::<u8>(0);

        assert_eq!(input, actual);
    }

    #[test]
    fn test_read_int_constant() {
        let mut chunk = Chunk::new();

        let input = 8_000_000_000;

        chunk.write_int_constant(input, 1);
        let actual = chunk.read::<i64>(1);

        assert_eq!(input, actual);
    }

    #[test]
    fn test_read_float_constant() {
        let mut chunk = Chunk::new();

        let input = 8_000_000_000.0;

        chunk.write_float_constant(input, 1);
        let actual = chunk.read::<f64>(1);

        assert_eq!(input, actual);
    }
}
