// Takes an HIR node + maps of values & types, produces a
// bytecode chunk (vector of bytes)

mod op;
mod stack;

use std::convert::TryInto;

pub use op::{InvalidOpError, Op};
pub use stack::ValueStack;

use hir::{Database, Expr, LocalDef, Stmt};

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

    /// Gets the Op at the given index
    #[inline(always)]
    pub fn get_op(&self, i: usize) -> Result<Op, InvalidOpError> {
        self.code[i].try_into()
    }

    /// Gets the bytes from the given offset and amount of words.
    ///
    /// A "word" in this context is 8 bytes (64-bit), the base size
    /// of values in the VM.
    #[inline(always)]
    fn read_n_words(&self, offset: usize, words: usize) -> &[u8] {
        let end = offset + (words * 8);
        &self.code[offset..end]
    }

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

    pub fn read_int(&self, offset: usize) -> i64 {
        let bytes: Word = self.read_word(offset);
        i64::from_le_bytes(bytes)
    }

    pub fn read_float(&self, offset: usize) -> f64 {
        let bytes: Word = self.read_word(offset).try_into().unwrap();
        f64::from_le_bytes(bytes)
    }

    pub fn read_str(&self, offset: usize) -> (Word, Word) {
        let bytes = self.read_dword(offset);

        let (ptr, len) = bytes.split_at(8);

        // TODO: better way to clone &[u8] to [u8; 8]?
        (ptr.try_into().unwrap(), len.try_into().unwrap())
    }

    pub fn write_op(&mut self, op: Op, line: u32) {
        self.code.push(op as u8);
        self.lines.push(line);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.code.extend(bytes);
    }

    pub fn write_int_constant(&mut self, value: i64, line: u32) {
        // TODO: decide to embed i64 in bytecode or embed a u32 index to int_constants
        // self.int_constants.push(value);
        // let i = self.int_constants.len() - 1;
        self.write_op(Op::PushInt, line);
        let bytes = value.to_le_bytes();

        self.write_bytes(&bytes)
    }

    pub fn write_float_constant(&mut self, value: f64, line: u32) {
        // TODO: decide to embed f64 in bytecode or embed a u32 index to float_constants
        // self.float_constants.push(value);
        // let i = self.float_constants.len() - 1;
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
        let idx = self.str_constants.len();
        let len = value.len();

        let bytes = value.as_bytes();
        self.str_constants.extend(bytes);

        self.write_op(Op::PushString, line);
        let idx = idx.to_le_bytes();
        self.write_bytes(&idx);
        let len = len.to_le_bytes();
        self.write_bytes(&len);
    }

    /// Prints the Chunk in a disassembled format
    /// for human-reading.
    ///
    /// This format is not stable and should not be depended on.
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
                rhs_type,
            } => {
                //

                match op {
                    hir::BinaryOp::Add => {

                        // write push lhs (expr)
                        // write push rhs (expr)
                        // write Add or FAdd depending on type
                    }
                    hir::BinaryOp::Sub => todo!(),
                    hir::BinaryOp::Mul => todo!(),
                    hir::BinaryOp::Div => todo!(),
                    hir::BinaryOp::Rem => todo!(),
                    hir::BinaryOp::Exp => todo!(),
                    hir::BinaryOp::Path => todo!(),
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
