//! Takes an HIR node + maps of valuesfield1& types, produces a bytecode Chunk.
//!
//! The bytecode chunk contains:
//! - The bytecode itself, including opcode and operands
//! - Text ranges corresponding to opcodes for debug information and errors
//! - Constants pool for constants not encoded into operands, ex. strings
//!
//! This code generation is **not** a traditional single-pass compiler. For
//! example, the jump offsets for if/else operations are not [back patched][backpatching].
//! Functions that synthesize (synth) the bytecode return their values rather than directly
//! writing to the chunk. That allows us to synth the "then branch" of an if/else and get its
//! byte length to write the jump offset before writing the "then branch". This allows me to
//! write correct code because I am horrible at manually dealing with byte offsets and bitwise
//! operations.
//!
//! Overall, I'm not entirely pleased with the style of code in this module, but it's working
//! for now. Open to better abstractions to express some of this in a cleaner way. Also, need
//! to study some of the performance trade-offs of the Vec allocations with this strategy
//! vs. some other possible ways of doing it.
//!
//! [backpatching]: https://stackoverflow.com/questions/15984671/what-does-backpatching-mean
use la_arena::Idx;
pub use op::{InvalidOpError, Op};
use text_size::TextRange;

use std::convert::TryInto;
use std::fmt::Debug;
use std::fmt::{self, Display};
use std::mem::size_of;

use hir::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Context, Expr, FunctionExpr, IfExpr, LetBinding,
    Type, UnaryExpr, UnaryOp,
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

/// Byte size of an Op::Jump or Op::JumpIfFalse with their u32 operand
const JUMP_WITH_OFFSET_SIZE: usize = 5;

pub fn codegen(idx: &Idx<Expr>, context: &Context) -> Chunk {
    let mut chunk = Chunk::new();

    chunk.write_expr(*idx, context);
    chunk.write_ret(TextRange::default());

    chunk
}

// TODO: better name
#[derive(Debug, Default, PartialEq, Eq)]
struct Code {
    bytes: Vec<u8>,
    ranges: Vec<TextRange>,
}

impl Code {
    fn append(&mut self, mut source: Code) {
        self.bytes.append(&mut source.bytes);
        self.ranges.append(&mut source.ranges);
    }

    fn push(&mut self, source: (u8, TextRange)) {
        self.bytes.push(source.0);
        self.ranges.push(source.1);
    }

    fn push_byte(&mut self, source: u8) {
        self.bytes.push(source);
    }

    fn extend_from_slice(&mut self, source: &[u8]) {
        self.bytes.extend_from_slice(source);
    }

    fn shrink_to_fit(&mut self) {
        self.bytes.shrink_to_fit();
        self.ranges.shrink_to_fit();
    }
}

impl From<(u8, TextRange)> for Code {
    fn from(source: (u8, TextRange)) -> Self {
        Self {
            bytes: vec![source.0],
            ranges: vec![source.1],
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Chunk {
    /// bytecode (ops and operands) with text ranges corresponding to those ops
    code: Code,

    /// constants pool that are not encoded as operands, ex. strings
    constants: Vec<u8>,
}

// TODO: make all pub(crate)
impl Chunk {
    fn new() -> Self {
        Default::default()
    }

    #[cfg(test)]
    pub fn new_with(bytecode: Vec<u8>, ranges: Vec<TextRange>, constants: Vec<u8>) -> Self {
        let code = Code {
            bytes: bytecode,
            ranges,
        };
        Self { code, constants }
    }
}

// Functions to write to the Chunk
impl Chunk {
    pub fn write_expr(&mut self, expr: Idx<Expr>, context: &Context) {
        let code = self.synth_expr(expr, context);

        self.code.append(code);
    }

    /// Generates the bytecode and `TextRange`s for an expression.
    ///
    /// **Invariant**: the index of each `TextRange` in that `Vec` need to correspond to the index of
    /// the corresponding `Op` within the bytecode.
    fn synth_expr(&mut self, expr: Idx<Expr>, context: &Context) -> Code {
        use Expr::*;

        let expr = context.expr(expr);
        match expr {
            BoolLiteral(b) => self.synth_bool_constant(*b, TextRange::default()),
            FloatLiteral(f) => self.synth_float_constant(*f, TextRange::default()),
            IntLiteral(i) => self.synth_int_constant(*i, TextRange::default()),
            StringLiteral(s) => self.synth_string_constant(s, TextRange::default()),

            Binary(expr) => self.synth_binary_expr(expr, context),
            Unary(expr) => self.synth_unary_expr(expr, context),
            VariableRef { name } => todo!(),
            Call(expr) => self.synth_call_expr(expr, context),
            Function(expr) => self.synth_function_expr(expr, context),
            LetBinding(expr) => self.synth_let_binding(expr, context),
            Block(expr) => self.synth_block_expr(expr, context),
            If(expr) => self.synth_if_expr(expr, context),

            // This should be unreachable, codegen should never start if lowering failed
            // TODO: add some machinery for some debug output and gracefully abort
            Empty => unreachable!(),
        }
    }

    fn synth_op(&self, op: Op, range: TextRange) -> (u8, TextRange) {
        (op as u8, range)
    }

    fn synth_bool_constant(&self, value: bool, range: TextRange) -> Code {
        match value {
            true => self.synth_op(Op::PushTrue, range),
            false => self.synth_op(Op::PushFalse, range),
        }
        .into()
    }

    fn synth_float_constant(&self, value: f64, range: TextRange) -> Code {
        let mut code: Code = self.synth_op(Op::PushFloat, range).into();
        code.extend_from_slice(&value.to_ne_bytes());

        code
    }

    fn synth_int_constant(&self, value: i64, range: TextRange) -> Code {
        let mut code: Code = self.synth_op(Op::PushInt, range).into();
        code.extend_from_slice(&value.to_ne_bytes());

        code
    }

    fn synth_string_constant(&mut self, value: &str, range: TextRange) -> Code {
        let idx = self.constants.len();
        let len = value.len();
        self.constants.extend(value.as_bytes());

        let mut code = Code::default();
        code.push(self.synth_op(Op::PushString, range));
        code.extend_from_slice(&idx.to_ne_bytes());
        code.extend_from_slice(&len.to_ne_bytes());

        code
    }

    fn synth_binary_expr(&mut self, expr: &BinaryExpr, context: &Context) -> Code {
        let BinaryExpr { op, lhs, rhs } = expr;
        let lhs_type = context.type_of(*lhs);

        let mut code = self.synth_expr(*lhs, context);
        code.append(self.synth_expr(*rhs, context));

        use BinaryOp::*;
        use Type::*;
        code.push(match op {
            Add => match lhs_type {
                Float | FloatLiteral(_) => self.synth_op(Op::AddFloat, TextRange::default()),
                Int | IntLiteral(_) => self.synth_op(Op::AddInt, TextRange::default()),
                _ => unreachable!(),
            },
            Sub => match lhs_type {
                Float | FloatLiteral(_) => self.synth_op(Op::SubFloat, TextRange::default()),
                Int | IntLiteral(_) => self.synth_op(Op::SubInt, TextRange::default()),
                _ => unreachable!(),
            },
            Mul => match lhs_type {
                Float | FloatLiteral(_) => self.synth_op(Op::MulFloat, TextRange::default()),
                Int | IntLiteral(_) => self.synth_op(Op::MulInt, TextRange::default()),
                _ => unreachable!(),
            },
            Div => match lhs_type {
                Float | FloatLiteral(_) => self.synth_op(Op::DivFloat, TextRange::default()),
                Int | IntLiteral(_) => self.synth_op(Op::DivInt, TextRange::default()),
                _ => unreachable!(),
            },
            Rem => todo!(),
            Exp => todo!(),
            Path => todo!(),
        });

        code
    }

    fn synth_unary_expr(&mut self, expr: &UnaryExpr, context: &Context) -> Code {
        let UnaryExpr { op, expr: idx } = expr;
        let expr_type = context.type_of(*idx);

        let mut code = self.synth_expr(*idx, context);

        use Type::*;
        use UnaryOp::*;
        code.push(match op {
            Neg => match expr_type {
                Float => self.synth_op(Op::NegateFloat, TextRange::default()),
                Int => self.synth_op(Op::NegateInt, TextRange::default()),
                _ => unreachable!(),
            },
            Not => todo!(),
        });

        code
    }

    fn synth_function_expr(&mut self, expr: &FunctionExpr, context: &Context) -> Code {
        todo!()
    }

    fn synth_let_binding(&mut self, expr: &LetBinding, context: &Context) -> Code {
        todo!()
    }

    fn synth_block_expr(&mut self, expr: &BlockExpr, context: &Context) -> Code {
        let BlockExpr { exprs } = expr;
        let mut code = Code::default();
        for expr in exprs {
            code.append(self.synth_expr(*expr, context));
        }

        code
    }

    fn synth_if_expr(&mut self, expr: &IfExpr, context: &Context) -> Code {
        let IfExpr {
            condition,
            then_branch,
            else_branch,
        } = expr;
        let mut code = self.synth_expr(*condition, context);

        code.push(self.synth_op(Op::JumpIfFalse, TextRange::default()));

        // synth the then_branch **before** appending the operand of Op::JumpIfFalse
        let then_code = self.synth_expr(*then_branch, context);

        let mut then_length = then_code.bytes.len();
        if else_branch.is_some() {
            then_length += JUMP_WITH_OFFSET_SIZE;
        }
        let jump_iff_operand = (then_length as i32).to_ne_bytes();
        code.extend_from_slice(&jump_iff_operand);

        // **now** append the then_branch after the JumpIfFalse offset was added
        code.append(then_code);

        if let Some(else_branch) = else_branch {
            code.push(self.synth_op(Op::Jump, TextRange::default()));

            // synth the else_branch first, to calculate Op::Jump offset
            let else_code = self.synth_expr(*else_branch, context);

            let jump_operand = (else_code.bytes.len() as i32).to_ne_bytes();
            code.extend_from_slice(&jump_operand);

            code.append(else_code);
        }

        code
    }

    fn synth_call_expr(&mut self, expr: &CallExpr, context: &Context) -> Code {
        let CallExpr { path, args } = expr;

        let mut code = Code::default();

        for arg in args {
            code.append(self.synth_expr(*arg, context));
        }
        let arg_types: Vec<&Type> = args.iter().map(|idx| context.type_of(*idx)).collect();

        // TODO: more general check for builtin
        if path == "print" {
            code.append(self.synth_builtin_call(path, arg_types));
        }

        code
    }

    fn synth_builtin_call(&mut self, path: &str, arg_types: Vec<&Type>) -> Code {
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

            return self.synth_builtin(builtin_idx, TextRange::default());
            // need instructions for push/pop?
        }

        Code::default()
    }

    fn synth_builtin(&mut self, builtin_idx: u8, range: TextRange) -> Code {
        let mut code = Code::default();
        code.push(self.synth_op(Op::Builtin, range));
        code.push_byte(builtin_idx);

        code
    }

    fn write_local_def(&mut self, local_def: &LetBinding) {
        let name = local_def.name();
        let value = local_def.value;
    }

    fn write_op(&mut self, op: Op, range: TextRange) {
        self.code.bytes.push(op as u8);
        self.code.ranges.push(range);
    }

    fn write_byte(&mut self, byte: u8) {
        self.code.bytes.push(byte);
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        self.code.bytes.extend(bytes);
    }

    pub fn write_ret(&mut self, range: TextRange) {
        self.write_op(Op::Ret, range);
    }

    pub fn write_noop(&mut self, range: TextRange) {
        self.write_op(Op::Noop, range);
    }

    pub fn shrink_to_fit(&mut self) {
        self.code.shrink_to_fit();
        self.constants.shrink_to_fit();
    }
}

// Functions to read from the Chunk.
impl Chunk {
    /// Gets the Op at the given index.
    #[inline]
    pub fn get_op(&self, i: usize) -> Result<Op, InvalidOpError> {
        self.code.bytes[i].try_into()
    }

    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        self.code
            .bytes
            .as_ptr()
            .add(offset)
            .cast::<T>()
            .read_unaligned()
    }

    /// Reads bytes from the bytecode and returns as T.
    ///
    /// Panics if there are not enough bytes to read.
    #[inline]
    pub fn read<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        assert!(offset + size_of::<T>() - 1 < self.code.bytes.len());

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
            .constants
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
        while offset < self.code.bytes.len() {
            let op = self.code.bytes[offset];
            let op: Op = op.try_into().expect("byte should be convertible to Op");
            let range = self.code.ranges[op_idx];

            let line = format!("{:?}:{:?}", range.start(), range.end());
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
