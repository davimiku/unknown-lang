// Takes an HIR node + maps of values & types, produces a
// bytecode chunk (vector of bytes)
pub use op::{InvalidOpError, Op};
pub use stack::ValueStack;

use std::str;
use std::{convert::TryInto, mem};

use hir::{BinaryOp, Database, Expr, LocalDef, Stmt, Type};

mod disassemble;
mod op;
mod stack;

/// A VM "word" is 8 bytes (64 bits)
type Word = [u8; 8];

/// A VM "double word" is 16 bytes (128 bits)
type DWord = [u8; 16];

/// A VM "quad word" is 32 bytes (256 bits)
type QWord = [u8; 32];

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
    pub fn write_stmt(&mut self, stmt: Stmt, database: &Database) {
        match stmt {
            Stmt::VariableDef(local_def) => todo!(),
            Stmt::Expr(idx) => {
                let expr = database.expr(idx);
                self.write_expr(expr, database)
            }
        }
    }

    pub fn write_expr(&mut self, expr: &Expr, database: &Database) {
        use Expr::*;

        match expr {
            BoolLiteral(b) => {
                self.write_bool_constant(*b, 1);
            }
            FloatLiteral(f) => {
                self.write_float_constant(*f, 1);
            }
            IntLiteral(i) => {
                self.write_int_constant(*i, 1);
            }
            StringLiteral(s) => {
                self.write_string_constant(s, 1);
            }
            Binary {
                op,
                lhs,
                rhs,
                lhs_type,
                ..
            } => {
                let lhs = database.expr(*lhs);
                let rhs = database.expr(*rhs);

                self.write_expr(lhs, database);
                self.write_expr(rhs, database);

                match op {
                    BinaryOp::Add => match lhs_type {
                        Type::Float | Type::FloatLiteral(_) => self.write_op(Op::AddFloat, 1),
                        Type::Int | Type::IntLiteral(_) => self.write_op(Op::AddInt, 1),
                        _ => unreachable!(),
                    },
                    BinaryOp::Sub => match lhs_type {
                        Type::Float | Type::FloatLiteral(_) => self.write_op(Op::SubFloat, 1),
                        Type::Int | Type::IntLiteral(_) => self.write_op(Op::SubInt, 1),
                        _ => unreachable!(),
                    },
                    BinaryOp::Mul => match lhs_type {
                        Type::Float | Type::FloatLiteral(_) => self.write_op(Op::MulFloat, 1),
                        Type::Int | Type::IntLiteral(_) => self.write_op(Op::MulInt, 1),
                        _ => unreachable!(),
                    },
                    BinaryOp::Div => match lhs_type {
                        Type::Float | Type::FloatLiteral(_) => self.write_op(Op::DivFloat, 1),
                        Type::Int | Type::IntLiteral(_) => self.write_op(Op::DivInt, 1),
                        _ => unreachable!(),
                    },
                    BinaryOp::Rem => todo!(),
                    BinaryOp::Exp => todo!(),
                    BinaryOp::Path => todo!(),
                }
            }
            Unary { op, expr, typ } => todo!(),
            Block { stmts, typ } => todo!(),
            VariableRef { name, typ } => todo!(),
            Function {
                params,
                body,
                return_type_annotation,
            } => todo!(),
            Empty => todo!(),
        }
    }

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

    /// Reads 1 byte from the bytecode.
    ///
    /// Panics if there are not enough bytes to read from.
    #[inline]
    pub fn read_byte(&self, offset: usize) -> u8 {
        let bytes: [u8; 1] = self
            .read_n_bytes(offset, 1)
            .try_into()
            .expect("should be at least 1 byte to read");

        bytes[0]
    }

    /// Reads N bytes from the bytecode.
    ///
    /// Panics if there are not enough bytes to read from
    #[inline]
    fn read_n_bytes(&self, offset: usize, bytes: usize) -> &[u8] {
        let end = offset + bytes;
        &self.code[offset..end]
    }

    /// Gets the bytes from the given offset and amount of words.
    ///
    /// A "word" in this context is 8 bytes (64-bit), the base size
    /// of values in the VM.
    #[inline]
    fn read_n_words(&self, offset: usize, words: usize) -> &[u8] {
        self.read_n_bytes(offset, words * 8)
    }

    #[inline]
    fn read_word(&self, offset: usize) -> Word {
        self.read_n_words(offset, 1)
            .try_into()
            .expect("should be at least 8 bytes to read")
    }

    fn read_dword(&self, offset: usize) -> DWord {
        self.read_n_words(offset, 2)
            .try_into()
            .expect("should be at least 16 bytes to read")
    }

    fn read_qword(&self, offset: usize) -> QWord {
        self.read_n_words(offset, 4)
            .try_into()
            .expect("should be at least 32 bytes to read")
    }

    /// Reads an Int (i64) value from the bytecode starting
    /// at the given offset.
    ///
    /// Panics if there are not enough bytes to read from the offset.
    pub fn read_int(&self, offset: usize) -> i64 {
        let bytes = self.read_word(offset);
        i64::from_le_bytes(bytes)
    }

    /// Reads an Float (f64) value from the bytecode starting
    /// at the given offset.
    ///
    /// Panics if there are not enough bytes to read from the offset.
    pub fn read_float(&self, offset: usize) -> f64 {
        let bytes = self.read_word(offset);
        f64::from_le_bytes(bytes)
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
        let bytes = self.read_dword(offset);

        let [first, second]: [Word; 2] = unsafe { mem::transmute::<[u8; 16], [Word; 2]>(bytes) };

        // TODO: better way to clone &[u8] to u64?
        (u64::from_le_bytes(first), u64::from_le_bytes(second))
    }

    /// Given the stack representation of a String constant,
    /// return the corresponding String from the constants pool.
    pub fn get_str_constant(&self, idx: usize, len: usize) -> &str {
        let end = idx + len;
        let bytes = self
            .str_constants
            .get(idx..end)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_bytes() {
        let mut chunk = Chunk::new();

        let input = 8_000_000_000;

        chunk.write_int_constant(input, 1);
        let bytes = chunk.read_n_words(1, 1);

        let bytes: [u8; 8] = bytes.try_into().expect("slice should have correct length");
        let actual = i64::from_le_bytes(bytes);

        assert_eq!(input, actual);
    }
}
