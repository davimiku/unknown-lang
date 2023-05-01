//! Takes an HIR node + maps of values & types, produces a bytecode Chunk.
//!
//! The bytecode chunk contains:
//! - The bytecode itself, including opcodes and operands
//! - Text ranges corresponding to opcodes for debug information and errors
//! - Constants pool for constants not encoded into operands, ex. strings
use code::Code;
pub use op::{InvalidOpError, Op};

use la_arena::Idx;
use text_size::TextRange;
use vm_types::string::VMString;
use vm_types::{VMBool, VMFloat, VMInt};

use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::mem;
use std::ops::Range;

pub use hir::COMPILER_BRAND;
use hir::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Expr, FunctionType, IfExpr, LocalDefKey,
    LocalRefExpr, Type, UnaryExpr, UnaryOp,
};

mod code;
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

pub fn codegen(idx: &Idx<Expr>, context: &hir::Context) -> ProgramChunk {
    let mut codegen = Codegen::new();

    codegen.write_expr(*idx, context);

    codegen.take_chunk()
}

#[derive(Debug, Default)]
struct Codegen {
    // stack of functions that are currently being compiled
    current_functions: Vec<FunctionCodegen>,

    finished_functions: Vec<FunctionChunk>,

    // TODO: what was I thinking with this? when generating a function (non-main)
    // we need to keep track of its index so that calls to that function can load
    // that function?
    // should be HashMap<id, index>?  (both are usize)
    // or current_functions should be a HashMap? Where the key is the eventual index of finished_functions
    function_map: HashMap<String, usize>,
}

impl Codegen {
    fn new() -> Self {
        let mut codegen = Codegen::default();
        // TODO: set parameter_slots to account for CLI args?

        codegen.push_func(0, "main", 0);

        codegen
    }

    fn take_chunk(mut self) -> ProgramChunk {
        self.finish_func(); // finish main

        ProgramChunk {
            functions: self.finished_functions,
        }
    }
}

impl Codegen {
    fn curr(&self) -> &FunctionCodegen {
        self.current_functions.last().unwrap()
    }

    fn curr_mut(&mut self) -> &mut FunctionCodegen {
        self.current_functions.last_mut().unwrap()
    }

    fn curr_chunk(&self) -> &FunctionChunk {
        &self.current_functions.last().unwrap().chunk
    }

    fn curr_chunk_mut(&mut self) -> &mut FunctionChunk {
        &mut self.current_functions.last_mut().unwrap().chunk
    }

    fn push_func(&mut self, id: usize, name: &str, parameter_slots: u32) {
        self.current_functions
            .push(FunctionCodegen::new(id, name, parameter_slots))
    }

    fn finish_func(&mut self) {
        self.append_ret();

        let curr = self.current_functions.pop().unwrap();
        self.finished_functions.push(curr.chunk);
    }

    fn write_expr(&mut self, expr: Idx<Expr>, context: &hir::Context) {
        let code = self.synth_expr(expr, context);

        self.curr_chunk_mut().code.append(code);
    }

    fn append_ret(&mut self) {
        self.curr_chunk_mut()
            .code
            .append(Code::from_op(Op::Ret, Default::default()))
    }

    /// Generates the bytecode and `TextRange`s for an expression.
    fn synth_expr(&mut self, expr_idx: Idx<Expr>, context: &hir::Context) -> Code {
        use Expr::*;

        let expr = context.expr(expr_idx);
        let range = context.range_of(expr_idx);
        match expr {
            Function(expr) => {
                self.push_func(0, "", 0);

                // let curr = self.curr_mut();
                // TODO: set curr.current_stack_slots using expr.params

                self.write_expr(expr.body, context);
                self.finish_func();

                // ??? what bytecode is generated here?
                Code::default()
            }

            BoolLiteral(b) => self.synth_bool_constant(*b, range),
            FloatLiteral(f) => self.synth_float_constant(*f, range),
            IntLiteral(i) => self.synth_int_constant(*i, range),
            StringLiteral(key) => self.synth_string_constant(context.lookup(*key), range),

            Binary(expr) => self.synth_binary_expr(expr, context),
            Unary(expr) => self.synth_unary_expr(expr, context),
            LocalDef(expr) => self.synth_local_def(expr.key, expr.value, context),
            Call(expr) => self.synth_call_expr(expr, context),
            LocalRef(expr) => self.synth_local_ref(expr_idx, expr, context),
            UnresolvedLocalRef { key } => unreachable!(),
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
        let idx = self.curr_chunk().constants.len();
        let len = value.len() as u32;
        self.curr_chunk_mut().constants.extend(value.as_bytes());

        let mut code = Code::default();
        code.push(self.synth_op(Op::PushString, range));

        let string = VMString::new_constant(len, idx);
        let string_bytes: [u8; 16] = string.into();
        code.extend_from_slice(&string_bytes);

        code
    }

    fn synth_binary_expr(&mut self, expr: &BinaryExpr, context: &hir::Context) -> Code {
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

    fn synth_unary_expr(&mut self, expr: &UnaryExpr, context: &hir::Context) -> Code {
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

    fn synth_local_def(
        &mut self,
        key: LocalDefKey,
        value: Idx<Expr>,
        context: &hir::Context,
    ) -> Code {
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
        code.extend_from_slice(&self.curr().current_stack_slots.to_le_bytes());
        if set_op == Op::SetLocalN {
            code.extend_from_slice(&word_size.to_le_bytes());
        }

        let mut curr = self.curr_mut();

        curr.stack_slots
            .insert(key, (curr.current_stack_slots, word_size));
        curr.current_stack_slots += word_size;

        code
    }

    fn synth_block_expr(&mut self, expr: &BlockExpr, context: &hir::Context) -> Code {
        let BlockExpr { exprs } = expr;

        let start_stack_slot_size = self.curr().current_stack_slots;

        let mut code = Code::default();
        for expr in exprs {
            code.append(self.synth_expr(*expr, context));
        }

        let slots_to_pop = self.curr().current_stack_slots - start_stack_slot_size;

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

        self.curr_mut().current_stack_slots = start_stack_slot_size;

        code
    }

    /// The jump offsets for if/else operations are not backpatched, they are synthesized
    /// (synth) first rather than directly writing to the chunk. That allows us to synth
    /// the "then branch" of an if/else and get its byte length to write the jump offset
    /// before writing the "then branch".
    ///
    /// This allows me to write correct code because I am horrible at manually dealing
    /// with byte offsets and bitwise operations. There may be a performance impact of
    /// this that can be evaluated later.
    fn synth_if_expr(&mut self, expr: &IfExpr, context: &hir::Context) -> Code {
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

    fn synth_call_expr(&mut self, expr: &CallExpr, context: &hir::Context) -> Code {
        let CallExpr {
            callee,
            args,
            callee_path,
        } = expr;

        let mut code = Code::default();

        // TODO: this is a hack
        if callee_path != "print" {
            code.append(self.synth_expr(*callee, context));
        }

        for arg in args {
            code.append(self.synth_expr(*arg, context));
        }
        let arg_types: Vec<&Type> = args.iter().map(|idx| context.type_of_expr(*idx)).collect();

        // TODO: putting builtins into a "core" library and loading them into a "prelude" for name resolution
        if callee_path == "print" {
            code.append(self.synth_builtin_call(callee_path, arg_types));
        } else {
            // TODO: generate Op::Call ?
        }

        code
    }

    fn synth_local_ref(
        &mut self,
        expr_idx: Idx<Expr>,
        expr: &LocalRefExpr,
        context: &hir::Context,
    ) -> Code {
        let (slot_number, slot_size) = self.curr().stack_slots[&expr.key];
        // let expr_type = context.type_of_expr(expr_idx);
        // let word_size = word_size_of(expr_type);
        let op = match slot_size {
            0 => unreachable!(),
            1 => Op::GetLocal,
            2 => Op::GetLocal2,
            4 => Op::GetLocal4,

            _ => Op::GetLocalN,
        };

        let mut code = Code::from_op(op, TextRange::default());

        code.extend_from_slice(&slot_number.to_le_bytes());

        if op == Op::GetLocalN {
            code.extend_from_slice(&slot_size.to_le_bytes());
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
}

#[derive(Debug, Default)]
pub struct ProgramChunk {
    pub functions: Vec<FunctionChunk>,
}

impl ProgramChunk {
    fn main(&self) -> &FunctionChunk {
        self.functions.last().unwrap()
    }
}

#[derive(Debug)]
pub struct FunctionCodegen {
    id: usize,

    /// Bytecode chunk being generated
    chunk: FunctionChunk,

    /// Running total of the stack slots that have been allocated for locals
    current_stack_slots: u16,

    /// Map from a unique LocalDef to (slot_number, slot_size) in the stack
    stack_slots: HashMap<LocalDefKey, (u16, u16)>,
}

impl FunctionCodegen {
    fn new(id: usize, name: &str, parameter_slots: u32) -> Self {
        Self {
            chunk: FunctionChunk::new(name, parameter_slots),
            id,
            current_stack_slots: 0,
            stack_slots: Default::default(),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FunctionChunk {
    /// bytecode (ops and operands) with text ranges corresponding to those ops
    code: Code,

    /// Total "slots" / words used by parameters of this function
    parameter_slots: u32,

    /// constants pool that are not encoded as operands, ex. strings
    ///
    /// The first byte is the length of the name of the function, and the next
    /// `len` bytes are the function name.
    // TODO: encapsulate this in a struct?
    constants: Vec<u8>,
}

// TODO: implement Debug

// Constructors
impl FunctionChunk {
    fn new(name: &str, parameter_slots: u32) -> Self {
        let mut chunk = Self::default();
        chunk.parameter_slots = parameter_slots;

        chunk.constants.push(name.len() as u8);
        chunk.constants.extend_from_slice(name.as_bytes());

        chunk
    }
}

// Mutating functions

impl FunctionChunk {
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

// Non-Mutating functions
impl FunctionChunk {
    /// Gets the Op at the given index.
    #[inline]
    pub fn get_op(&self, i: usize) -> Result<Op, InvalidOpError> {
        self.code.bytes[i].try_into()
    }

    /// Reads bytes from the bytecode and returns as T.
    ///
    /// Panics if there are not enough bytes to read.
    // TODO: mutate `offset` in this function so the caller
    // doesn't need to remember to mutate it
    #[inline]
    pub fn read<T>(&self, offset: usize) -> T
    where
        T: BytecodeRead,
    {
        assert!(offset + mem::size_of::<T>() - 1 < self.code.bytes.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }

    /// Gets the name of the function from the bytecode
    pub fn name(&self) -> &str {
        let len = self.constants[0] as usize;
        if len == 0 {
            return "";
        }

        let bytes = &self.constants[1..len];

        // Safety: bytes were valid UTF-8 when Self was created
        unsafe { std::str::from_utf8_unchecked(bytes) }
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

    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: BytecodeRead,
    {
        self.code
            .bytes
            .as_ptr()
            .add(offset)
            .cast::<T>()
            .read_unaligned()
    }
}

// Functions for debugging the Chunk
#[cfg(debug_assertions)]
impl Display for FunctionChunk {
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
        Type::Bool | Type::BoolLiteral(_) => vm_types::word_size_of::<VMBool>(),
        Type::Float | Type::FloatLiteral(_) => vm_types::word_size_of::<VMFloat>(),
        Type::Int | Type::IntLiteral(_) => vm_types::word_size_of::<VMInt>(),
        Type::String | Type::StringLiteral(_) => vm_types::word_size_of::<VMString>(),
        // Type::Named(_) => todo!(),
        Type::Unit => 0,

        Type::Function(FunctionType { .. }) => todo!(),

        Type::Undetermined => unreachable!(),
        Type::Error => unreachable!(),
        Type::Top => 0,
        Type::Bottom => 0,
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
pub unsafe trait BytecodeRead {}

unsafe impl BytecodeRead for u8 {}
unsafe impl BytecodeRead for u16 {}
unsafe impl BytecodeRead for u32 {}
unsafe impl BytecodeRead for VMInt {}
unsafe impl BytecodeRead for VMFloat {}
unsafe impl BytecodeRead for VMString {}
