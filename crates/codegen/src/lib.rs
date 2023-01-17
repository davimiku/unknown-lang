//! Takes an HIR node + maps of values & types, produces a
//! bytecode chunk (vector of bytes)
use la_arena::Idx;
pub use op::{InvalidOpError, Op};

use std::fmt::{self, Display};
use std::mem::size_of;
use std::{convert::TryInto, fmt::Debug};

use hir::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Context, Expr, IfExpr, LetBinding, Type, UnaryExpr,
    UnaryOp,
};

mod disassemble;
mod op;
#[cfg(test)]
mod tests;

// TODO: extract to a shared "builtins" crate
pub const PRINT_STR_CONSTANT: u8 = 0;
pub const PRINT_STR: u8 = 1;
pub const PRINT_INT: u8 = 2;
pub const PRINT_FLOAT: u8 = 3;
pub const PRINT_BOOL: u8 = 4;

// needs to be replaced with a Span for each expression
const FAKE_LINE_NUMBER: u32 = 123;

pub fn codegen(idx: &Idx<Expr>, context: &Context) -> Chunk {
    let mut chunk = Chunk::new();

    chunk.write_expr(*idx, context);
    chunk.write_ret(FAKE_LINE_NUMBER);

    chunk
}

/// A VM "word" is 8 bytes (64 bits)
type Word = [u8; 8];

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Chunk {
    /// bytecode contained in the chunk
    code: Vec<u8>,

    /// string constants in the "data" section
    str_constants: Vec<u8>,

    /// tracking of the source code lines corresponding to op codes
    // FIXME: the Crafting Interpreters author calls this a "braindead" approach,
    // just a starting point for simplicity.
    // Find a better way to store source code location for bytecodes, such as Span
    // corresponding to each expression
    lines: Vec<u32>,
}

// TODO: make all pub(crate)
impl Chunk {
    pub fn new() -> Self {
        Default::default()
    }

    #[cfg(test)]
    pub fn new_with(code: Vec<u8>, str_constants: Vec<u8>, lines: Vec<u32>) -> Self {
        Self {
            code,
            str_constants,
            lines,
        }
    }
}

// Functions to write to the Chunk
impl Chunk {
    pub fn write_expr(&mut self, expr: Idx<Expr>, context: &Context) {
        use Expr::*;

        let expr = context.expr(expr);
        match expr {
            BoolLiteral(b) => self.write_bool_constant(*b, FAKE_LINE_NUMBER),
            FloatLiteral(f) => self.write_float_constant(*f, FAKE_LINE_NUMBER),
            IntLiteral(i) => self.write_int_constant(*i, FAKE_LINE_NUMBER),
            StringLiteral(s) => self.write_string_constant(s, FAKE_LINE_NUMBER),

            Binary(expr) => self.write_binary_expr(expr, context),
            Unary(expr) => self.write_unary_expr(expr, context),
            VariableRef { name } => todo!(),
            Call(expr) => self.write_call_expr(expr, context),
            Function {
                params,
                body,
                return_type_annotation,
            } => todo!(),
            LetBinding(expr) => {
                let v: u16 = 34;
                todo!()
            }

            Block(BlockExpr { exprs }) => {
                // write something for a scope?
                for expr in exprs {
                    self.write_expr(*expr, context);
                }
                // end scope?
            }

            If(IfExpr {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.write_expr(*condition, context);

                self.write_op(Op::JumpIfFalse, FAKE_LINE_NUMBER);
                let jumpiff_offset = self.code.len();
                self.write_u32_ones_bytes();

                self.write_expr(*then_branch, context);
                // backpatch offset to the JumpIfFalse Op
                let then_size = (self.code.len() - jumpiff_offset + 1) as i32;
                for (i, byte) in then_size.to_le_bytes().iter().enumerate() {
                    self.code[jumpiff_offset + i] = *byte;
                }

                self.write_op(Op::Jump, FAKE_LINE_NUMBER);
                let jump_offset = self.code.len();
                if let Some(else_branch) = else_branch {
                    self.write_u32_ones_bytes();

                    self.write_expr(*else_branch, context);
                    // backpatch offset to the Op::Jump, not including the operands of Jump itself
                    let else_size = (self.code.len() - jump_offset - 4) as i32;
                    for (i, byte) in else_size.to_le_bytes().iter().enumerate() {
                        self.code[jump_offset + i] = *byte;
                    }
                } else {
                    self.write_u32_zeros_bytes();
                }
            }

            // This should be unreachable, codegen should never start if lowering failed
            // TODO: add some machinery for some debug output and gracefully abort
            Empty => unreachable!(),
        }
    }

    fn write_binary_expr(&mut self, expr: &BinaryExpr, context: &Context) {
        let BinaryExpr { op, lhs, rhs } = expr;
        let lhs_type = context.type_of(*lhs);

        self.write_expr(*lhs, context);
        self.write_expr(*rhs, context);

        use BinaryOp::*;
        use Type::*;
        match op {
            Add => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::AddFloat, FAKE_LINE_NUMBER),
                Int | IntLiteral(_) => self.write_op(Op::AddInt, FAKE_LINE_NUMBER),
                _ => unreachable!(),
            },
            Sub => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::SubFloat, FAKE_LINE_NUMBER),
                Int | IntLiteral(_) => self.write_op(Op::SubInt, FAKE_LINE_NUMBER),
                _ => unreachable!(),
            },
            Mul => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::MulFloat, FAKE_LINE_NUMBER),
                Int | IntLiteral(_) => self.write_op(Op::MulInt, FAKE_LINE_NUMBER),
                _ => unreachable!(),
            },
            Div => match lhs_type {
                Float | FloatLiteral(_) => self.write_op(Op::DivFloat, FAKE_LINE_NUMBER),
                Int | IntLiteral(_) => self.write_op(Op::DivInt, FAKE_LINE_NUMBER),
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

        self.write_expr(*idx, context);

        use Type::*;
        use UnaryOp::*;
        match op {
            Neg => match expr_type {
                Float => self.write_op(Op::NegateFloat, FAKE_LINE_NUMBER),
                Int => self.write_op(Op::NegateInt, FAKE_LINE_NUMBER),
                _ => unreachable!(),
            },
            Not => todo!(),
        }
    }

    fn write_call_expr(&mut self, expr: &CallExpr, context: &Context) {
        let CallExpr { path, args } = expr;

        for arg in args {
            self.write_expr(*arg, context);
        }
        let arg_types: Vec<&Type> = args.iter().map(|idx| context.type_of(*idx)).collect();

        // TODO: more general check for builtin
        if path == "print" {
            self.write_builtin_call(path, arg_types);
        }
    }

    fn write_builtin_call(&mut self, path: &str, arg_types: Vec<&Type>) {
        if path == "print" {
            let arg_type = *arg_types.get(0).expect("print to have 1 argument");
            let builtin_idx = match arg_type {
                Type::Bool | Type::BoolLiteral(_) => PRINT_BOOL,
                Type::Float | Type::FloatLiteral(_) => PRINT_FLOAT,
                Type::Int | Type::IntLiteral(_) => PRINT_INT,
                Type::String => PRINT_STR,
                Type::StringLiteral(_) => PRINT_STR_CONSTANT,

                Type::Named(_) => todo!(),

                _ => unreachable!(),
            };

            self.write_builtin(builtin_idx, FAKE_LINE_NUMBER);
            // need instructions for push/pop?
        }
    }

    // fn write_block_expr(&mut self, context: &)

    fn write_local_def(&mut self, local_def: &LetBinding) {
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

    // bad name?
    fn write_u32_ones_bytes(&mut self) {
        let all_ones: u32 = 0xFFFFFFFF;
        self.write_bytes(&all_ones.to_le_bytes());
    }

    fn write_u32_zeros_bytes(&mut self) {
        let all_zeros: u32 = 0;
        self.write_bytes(&all_zeros.to_le_bytes());
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

    pub fn write_ret(&mut self, line: u32) {
        self.write_op(Op::Ret, line);
    }

    pub fn write_noop(&mut self, line: u32) {
        self.write_op(Op::Noop, line);
    }

    pub fn shrink_to_fit(&mut self) {
        self.code.shrink_to_fit();
        self.lines.shrink_to_fit();
        self.str_constants.shrink_to_fit();
    }
}

// Functions to read from the Chunk.
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
        std::str::from_utf8(bytes).expect("bytes should be valid UTF-8")
    }
}

// Functions for debugging the Chunk
#[cfg(debug_assertions)]
impl Display for Chunk {
    /// Prints the Chunk in a disassembled format for human-reading.
    ///
    /// This format is not stable and should not be depended on for
    /// any kind of machine parsing.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            write!(f, "{offset:04}  {line}  ")?;
            offset = op.disassemble(self, offset);
            writeln!(f)?;

            op_idx += 1;
        }
        writeln!(f)
    }
}

/// Types implementing this trait may be read from the bytecode.
///
/// `unsafe` could be removed after negative_impls is stabilized.
/// https://github.com/rust-lang/rust/issues/68318
///
/// # Safety
///
/// Types implementing this trait must not implement Drop.
pub unsafe trait Readable {}

unsafe impl Readable for u8 {}
unsafe impl Readable for u32 {}
unsafe impl Readable for i64 {}
unsafe impl Readable for f64 {}
unsafe impl Readable for (u64, u64) {}
