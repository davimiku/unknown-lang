//! Takes an HIR node + maps of values & types, produces a bytecode Chunk.
//!
//! The bytecode chunk contains:
//! - The bytecode itself, including opcodes and operands
//! - Text ranges corresponding to opcodes for debug information and errors
//! - Constants pool for constants not encoded into operands, ex. strings
//!
//! This code generation is not really a single pass through the HIR. For
//! example, the jump offsets for if/else operations are not [back patched][backpatching].
//! Functions that synthesize (synth) the bytecode return their values rather than directly
//! writing to the chunk. That allows us to synth the "then branch" of an if/else and get its
//! byte length to write the jump offset before writing the "then branch". This allows me to
//! write correct code because I am horrible at manually dealing with byte offsets and bitwise
//! operations. There may be a performance impact of this that can be evaluated later.
//!
//! [backpatching]: https://stackoverflow.com/questions/15984671/what-does-backpatching-mean
pub use op::{InvalidOpError, Op};

use la_arena::Idx;
use text_size::TextRange;
use vm_types::vm_string::VMString;
use vm_types::words::{QWord, Word, WORD_SIZE};
use vm_types::{VMBool, VMFloat, VMInt};

use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{self, Debug, Display};
use std::mem;
use std::ops::Range;

use hir::{BinaryExpr, BinaryOp, BlockExpr, CallExpr, Context, Expr, FunctionExpr};
use hir::{IfExpr, LocalDefKey, LocalRef, LocalRefName, Type, UnaryExpr, UnaryOp};

mod disassemble;
mod op;
#[cfg(test)]
mod tests;

// TODO: extract to a shared "builtins" crate
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
#[derive(Default, PartialEq, Eq)]
struct Code {
    bytes: Vec<u8>,
    ranges: Vec<TextRange>,
}

impl Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Code")
            .field("bytes", &debug_bytes(&self.bytes))
            .field("ranges", &self.ranges)
            .finish()
    }
}

fn debug_bytes(bytes: &[u8]) -> impl fmt::Debug {
    let bytes: Vec<_> = bytes
        .chunks_exact(4)
        .map(|s| {
            let q: [u8; 4] = s.try_into().expect("slice with length 4");
            u32::from_le_bytes(q)
        })
        .collect();

    bytes
}

impl Code {
    fn from_op(op: Op, range: TextRange) -> Self {
        Self {
            bytes: vec![op as u8],
            ranges: vec![range],
        }
    }
}

impl Code {
    #[inline]
    fn append(&mut self, mut source: Code) {
        self.bytes.append(&mut source.bytes);
        self.ranges.append(&mut source.ranges);
    }

    #[inline]
    fn push(&mut self, source: (u8, TextRange)) {
        self.bytes.push(source.0);
        self.ranges.push(source.1);
    }

    #[inline]
    fn push_byte(&mut self, source: u8) {
        self.bytes.push(source);
    }

    #[inline]
    fn push_byte_pair(&mut self, source: (u8, u8)) {
        self.bytes.push(source.0);
        self.bytes.push(source.1);
    }

    #[inline]
    fn push_u16(&mut self, source: u16) {
        let source: [u8; 2] = source.to_le_bytes();

        self.bytes.push(source[0]);
        self.bytes.push(source[1]);
    }

    #[inline]
    fn extend<I: IntoIterator<Item = u8>>(&mut self, source: I) {
        self.bytes.extend(source.into_iter());
    }

    #[inline]
    fn extend_from_slice(&mut self, source: &[u8]) {
        self.bytes.extend_from_slice(source);
    }

    #[inline]
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

    /// Running total of the stack slots that have been allocated for locals
    current_stack_slots: u16,

    /// Map from a unique LocalDef to its slot in the stack (and its slot size??)
    stack_slots: HashMap<LocalDefKey, u16>,
}

// Constructors
impl Chunk {
    fn new() -> Self {
        Default::default()
    }
}

// Mutating functions
impl Chunk {
    fn write_expr(&mut self, expr: Idx<Expr>, context: &Context) {
        let code = self.synth_expr(expr, context);

        self.code.append(code);
    }

    /// Generates the bytecode and `TextRange`s for an expression.
    ///
    /// **Invariant**: the index of each `TextRange` in that `Vec` need to correspond to the index of
    /// the corresponding `Op` within the bytecode.
    fn synth_expr(&mut self, expr_idx: Idx<Expr>, context: &Context) -> Code {
        use Expr::*;

        let expr = context.expr(expr_idx);
        match expr {
            BoolLiteral(b) => self.synth_bool_constant(*b, TextRange::default()),
            FloatLiteral(f) => self.synth_float_constant(*f, TextRange::default()),
            IntLiteral(i) => self.synth_int_constant(*i, TextRange::default()),
            StringLiteral(key) => {
                let s = context.lookup(*key);
                self.synth_string_constant(s, TextRange::default())
            }

            Binary(expr) => self.synth_binary_expr(expr, context),
            Unary(expr) => self.synth_unary_expr(expr, context),
            Function(expr) => self.synth_function_expr(expr, context),
            LocalDef(expr) => self.synth_local_def(expr.key, expr.value, context),
            Call(expr) => self.synth_call_expr(expr, context),
            LocalRef(expr) => self.synth_local_ref(expr_idx, expr, context),
            Block(expr) => self.synth_block_expr(expr, context),
            If(expr) => self.synth_if_expr(expr, context),

            // This should be unreachable, codegen should never start if lowering failed
            // TODO: add some machinery for some debug output and gracefully abort
            Empty => unreachable!("encountered Empty expression during codegen"),
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
        code.extend_from_slice(&value.to_le_bytes());

        code
    }

    fn synth_int_constant(&self, value: VMInt, range: TextRange) -> Code {
        let mut code: Code = self.synth_op(Op::PushInt, range).into();
        code.extend_from_slice(&value.to_le_bytes());

        code
    }

    fn synth_string_constant(&mut self, value: &str, range: TextRange) -> Code {
        let idx = self.constants.len();
        let len = value.len() as u32;
        self.constants.extend(value.as_bytes());

        let mut code = Code::default();
        code.push(self.synth_op(Op::PushString, range));

        let string = VMString::new_constant(len, idx);
        let string_bytes: [u8; 16] = string.into();
        code.extend_from_slice(&string_bytes);

        code
    }

    fn synth_binary_expr(&mut self, expr: &BinaryExpr, context: &Context) -> Code {
        let BinaryExpr { op, lhs, rhs } = expr;
        let lhs_type = context.type_of_expr(*lhs);

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
            Concat => match lhs_type {
                String | StringLiteral(_) => self.synth_op(Op::ConcatString, TextRange::default()),
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
        let expr_type = context.type_of_expr(*idx);

        let mut code = self.synth_expr(*idx, context);

        use Type::*;
        use UnaryOp::*;
        code.push(match (op, expr_type) {
            (Neg, Int) | (Neg, IntLiteral(_)) => self.synth_op(Op::NegateInt, TextRange::default()),
            (Neg, Float) | (Neg, FloatLiteral(_)) => {
                self.synth_op(Op::NegateFloat, TextRange::default())
            }
            (Not, Bool) | (Not, BoolLiteral(_)) => self.synth_op(Op::NotBool, TextRange::default()),

            _ => unreachable!(),
        });

        code
    }

    fn synth_function_expr(&mut self, expr: &FunctionExpr, context: &Context) -> Code {
        todo!()
    }

    fn synth_local_def(&mut self, key: LocalDefKey, value: Idx<Expr>, context: &Context) -> Code {
        let mut code = self.synth_expr(value, context);

        let expr_type = context.type_of_expr(value);
        let word_size = word_size_of(expr_type);

        let set_op = match word_size {
            0 => unreachable!(),
            1 => Op::SetLocal,
            2 => Op::SetLocal2,
            4 => Op::SetLocal4,

            _ => Op::SetLocalN,
        };
        code.append(Code::from_op(set_op, TextRange::default()));
        code.extend_from_slice(&self.current_stack_slots.to_le_bytes());
        if set_op == Op::SetLocalN {
            code.extend_from_slice(&word_size.to_le_bytes());
        }

        self.stack_slots.insert(key, self.current_stack_slots);
        self.current_stack_slots += word_size;

        code
    }

    fn synth_block_expr(&mut self, expr: &BlockExpr, context: &Context) -> Code {
        let BlockExpr { exprs } = expr;

        let start_stack_slot_size = self.current_stack_slots;

        let mut code = Code::default();
        for expr in exprs {
            code.append(self.synth_expr(*expr, context));
        }

        let slots_to_pop = self.current_stack_slots - start_stack_slot_size;

        match slots_to_pop {
            0 => {}
            1 => code.push(self.synth_op(Op::Pop1, TextRange::default())),
            2 => code.push(self.synth_op(Op::Pop2, TextRange::default())),
            4 => code.push(self.synth_op(Op::Pop4, TextRange::default())),
            n => {
                code.push(self.synth_op(Op::PopN, TextRange::default()));
                code.extend_from_slice(&n.to_le_bytes());
            }
        }

        self.current_stack_slots = start_stack_slot_size;

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
        // TODO: is then_branch a BlockExpr? Otherwise we need to handle scopes
        let then_code = self.synth_expr(*then_branch, context);

        let mut then_length = then_code.bytes.len();
        if else_branch.is_some() {
            then_length += JUMP_WITH_OFFSET_SIZE;
        }
        let jump_iff_operand = (then_length as i32).to_le_bytes();
        code.extend_from_slice(&jump_iff_operand);

        // **now** append the then_branch after the JumpIfFalse offset was added
        code.append(then_code);

        if let Some(else_branch) = else_branch {
            code.push(self.synth_op(Op::Jump, TextRange::default()));

            // synth the else_branch first, to calculate Op::Jump offset
            let else_code = self.synth_expr(*else_branch, context);

            let jump_operand = (else_code.bytes.len() as i32).to_le_bytes();
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
        let arg_types: Vec<&Type> = args.iter().map(|idx| context.type_of_expr(*idx)).collect();

        // TODO: putting builtins into a "core" library and loading them into a "prelude" for name resolution
        if path == "print" {
            code.append(self.synth_builtin_call(path, arg_types));
        }

        code
    }

    fn synth_local_ref(&mut self, expr_idx: Idx<Expr>, expr: &LocalRef, context: &Context) -> Code {
        let expr_type = context.type_of_expr(expr_idx);
        let word_size = word_size_of(expr_type);
        let op = match word_size {
            0 => unreachable!(),
            1 => Op::GetLocal,
            2 => Op::GetLocal2,
            4 => Op::GetLocal4,

            _ => Op::GetLocalN,
        };

        let mut code = Code::from_op(op, TextRange::default());

        let name = expr.name;

        if let LocalRefName::Resolved(key) = name {
            let stack_slot_offset = self.stack_slots[&key];
            code.extend_from_slice(&stack_slot_offset.to_le_bytes());

            if op == Op::GetLocalN {
                code.extend_from_slice(&word_size.to_le_bytes());
            }
        } else {
            // TODO: this should go away when a MIR (control flow graph) is added after the HIR
            // we would only have resolved names in codegen
            unreachable!()
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
                Type::String | Type::StringLiteral(_) => PRINT_STR,

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
    // TODO: mutate `offset` in this function so the caller
    // doesn't need to remember to mutate it
    #[inline]
    pub fn read<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        assert!(offset + mem::size_of::<T>() - 1 < self.code.bytes.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }

    pub fn borrow_constants(&self) -> &[u8] {
        &self.constants
    }

    /// Gets a slice of bytes from the constants pool at the given index
    ///
    /// Panics: if the index is out-of-bounds
    pub fn constants_slice(&self, index: Range<usize>) -> &[u8] {
        let result = self.constants.get(index.clone());
        match result {
            Some(s) => s,
            None => {
                println!("\n\n{index:?}");
                panic!("Invalid slice index")
            }
        }
    }

    /// Gets a slice of bytes from the constants pool at the given index
    ///
    /// # Safety
    ///
    /// Triggers undefined behavior if the index is out-of-bounds
    pub unsafe fn constants_slice_unchecked(&self, index: Range<usize>) -> &[u8] {
        self.constants.get_unchecked(index)
    }
}

// Functions for debugging the Chunk
#[cfg(debug_assertions)]
impl Display for Chunk {
    /// Prints the Chunk in a disassembled format for human-reading.
    ///
    /// This format is not stable and should not be depended on for
    /// any kind of machine parsing.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

// TODO: represent this as a static map or some other efficient form
// or use once_cell if merged into std
// lazy_static!{
//     static ref MAP: HashMap<char, &'static str> = [
//         ('a', "apple"),
//         ('b', "bear"),
//         ('c', "cat"),
//     ].iter().copied().collect();
// }

/// Returns the size of the type in units of "words" or "slots"
///
/// Assumption: A type will fit within u16::MAX words
/// In reality it will be way smaller than that, as large structs will be
/// transparently heap-allocated.
fn word_size_of(ty: &Type) -> u16 {
    (match ty {
        Type::Bool | Type::BoolLiteral(_) => mem::size_of::<VMBool>() / WORD_SIZE,
        Type::Float | Type::FloatLiteral(_) => mem::size_of::<VMFloat>() / WORD_SIZE,
        Type::Int | Type::IntLiteral(_) => mem::size_of::<VMInt>() / WORD_SIZE,
        Type::String | Type::StringLiteral(_) => mem::size_of::<VMString>() / WORD_SIZE,
        Type::Named(_) => todo!(),
        Type::Unit => 0,

        Type::Undetermined => unreachable!(),
        Type::Error => unreachable!(),
    } as u16)
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
unsafe impl Readable for u16 {}
unsafe impl Readable for u32 {}
unsafe impl Readable for VMInt {}
unsafe impl Readable for VMFloat {}
unsafe impl Readable for VMString {}
