//! Takes an HIR node + maps of values & types, produces a bytecode Chunk.
//!
//! The bytecode chunk contains:
//! - The bytecode itself, including opcodes and operands
//! - Text ranges corresponding to opcodes for debug information and errors
//! - Constants pool for constants not encoded into operands, ex. strings
pub use chunks::{FunctionChunk, ProgramChunk};
use code::Code;
pub use op::{IntoStringOperand, InvalidOpError, Op, PushStringOperand, ReturnType};

use la_arena::Idx;
use text_size::TextRange;
use vm_string::VMString;
use vm_types::{VMBool, VMFloat, VMInt};

use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;

pub use hir::COMPILER_BRAND;
use hir::{
    BinaryExpr, BinaryOp, BlockExpr, CallExpr, Expr, IfExpr, LocalDefKey, LocalRefExpr, Type,
    UnaryExpr, UnaryOp,
};

mod chunks;
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

pub fn codegen(idx: &Idx<Expr>, context: &mut hir::Context) -> ProgramChunk {
    let mut codegen = Codegen::new(context);

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

/// Word size of an array, currently used to reserve stack slots
/// for the array of CLI arguments.
///
/// TODO: Move to an `array` module (maybe in `vm_types`) when arrays are implemented
const ARRAY_WORD_SIZE: u16 = 2;

/// Casts the target expression into the expected variant, or
/// panics otherwise.
///
/// ```ignore
/// let function_ty = cast!(expr_idx, Type::Function);
/// ```
macro_rules! cast {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            a
        } else {
            panic!("mismatch variant when cast to {}", stringify!($pat));
        }
    }};
}

impl Codegen {
    fn new(context: &mut hir::Context) -> Self {
        let mut codegen = Codegen::default();

        codegen.push_func(0, "main", ARRAY_WORD_SIZE, 1);

        let args_local_key = context.find_local("args").unwrap();
        let main = codegen.curr_mut();
        main.add_local(args_local_key, ARRAY_WORD_SIZE);

        // TODO: implement Array
        main.push_alloc_type(&Type::Int);
        main.push_alloc_type(&Type::Int);

        codegen
    }

    fn take_chunk(mut self) -> ProgramChunk {
        self.finish_func(true); // finish main

        ProgramChunk::new(self.finished_functions).expect("at least one compiled function")
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

    fn push_func(&mut self, id: usize, name: &str, parameter_slots: u16, return_slots: u16) {
        let func_codegen = FunctionCodegen::new(id, name, parameter_slots, return_slots);
        self.current_functions.push(func_codegen);
    }

    fn finish_func(&mut self, main_implicit_return: bool) {
        let return_code = self.synth_return(main_implicit_return);

        let curr_chunk = self.curr_chunk_mut();
        curr_chunk.append(return_code);

        let curr = self.current_functions.pop().unwrap();
        self.finished_functions.push(curr.chunk);
    }

    fn write_expr(&mut self, expr: Idx<Expr>, context: &hir::Context) {
        let code = self.synth_expr(expr, context);

        self.curr_chunk_mut().append(code);
    }

    /// Generates the bytecode and `TextRange`s for an expression.
    #[must_use]
    fn synth_expr(&mut self, expr_idx: Idx<Expr>, context: &hir::Context) -> Code {
        use Expr::*;

        let expr = context.expr(expr_idx);
        let range = context.range_of(expr_idx);
        match expr {
            Statement(expr_idx) => {
                let mut code = self.synth_expr(*expr_idx, context);

                let ty = context.type_of_expr(*expr_idx);
                code.append(pop_code_of(ty));

                code
            }
            ReturnStatement(return_value) => {
                let mut code = self.synth_expr(*return_value, context);
                code.append(self.synth_return(false));

                code
            }
            // TODO: clean up this messy logic, extract to a separate function
            Function(expr) => {
                let name = expr.name.map_or("", |key| context.lookup(key));

                let mut param_types: BTreeMap<LocalDefKey, &Type> = BTreeMap::default();
                for param in expr.params.iter() {
                    let param_key = param.name;
                    let ty = context
                        .type_of_local(&param_key)
                        .expect("type of parameter is known");

                    param_types.insert(param_key, ty);
                }
                let return_ty = &cast!(context.type_of_expr(expr_idx), Type::Function).return_ty;
                let return_slots = word_size_of(return_ty);

                let id = self.finished_functions.len();
                let parameter_slots: u16 = param_types.values().map(|ty| word_size_of(ty)).sum();
                self.push_func(id, name, parameter_slots, return_slots);

                let curr = self.curr_mut();
                for (param_key, param_type) in param_types {
                    curr.add_local(param_key, word_size_of(param_type));
                    curr.push_alloc_type(param_type);
                }

                self.write_expr(expr.body, context);
                self.finish_func(false);

                synth_op_u32(Op::PushLocalFunc, range, id as u32)
            }

            BoolLiteral(b) => synth_bool_constant(*b, range),
            FloatLiteral(f) => synth_float_constant(*f, range),
            IntLiteral(i) => synth_int_constant(*i, range),
            StringLiteral(key) => self.synth_string_constant(context.lookup(*key), range),

            Binary(expr) => self.synth_binary_expr(expr_idx, expr, context),
            Unary(expr) => self.synth_unary_expr(expr_idx, expr, context),
            LocalDef(expr) => self.synth_local_def(expr.key, expr.value, context),
            Call(expr) => self.synth_call_expr(expr_idx, expr, context),
            LocalRef(expr) => self.synth_local_ref(expr_idx, expr, context),
            UnresolvedLocalRef { key: _ } => unreachable!(),
            EmptyBlock => Code::default(),
            Block(expr) => self.synth_block_expr(expr, context),
            If(expr) => self.synth_if_expr(expr, context),

            // This should be unreachable, codegen should never start if lowering failed
            // TODO: add some machinery for some debug output and gracefully abort
            Empty => unreachable!("encountered Empty expression during codegen"),
        }
    }

    #[must_use]
    fn synth_string_constant(&mut self, value: &str, range: TextRange) -> Code {
        let idx = self.curr_chunk().constants.len();
        let len = value.len() as u32;
        self.curr_chunk_mut().constants.extend(value.as_bytes());

        let mut code = Code::default();
        code.push((Op::PushString, range));

        let operand = PushStringOperand {
            len,
            offset: idx as u32,
        };

        code.extend_from_slice(&operand.to_bytes());

        code
    }

    #[must_use]
    fn synth_binary_expr(
        &mut self,
        expr_idx: Idx<Expr>,
        expr: &BinaryExpr,
        context: &hir::Context,
    ) -> Code {
        let BinaryExpr { op, lhs, rhs } = expr;
        let range = context.range_of(expr_idx);
        let lhs_type = context.type_of_expr(*lhs);

        let mut code = self.synth_expr(*lhs, context);
        code.append(self.synth_expr(*rhs, context));

        use BinaryOp::*;
        use Type::*;
        code.push(match op {
            Add => match lhs_type {
                Float | FloatLiteral(_) => (Op::AddFloat, range),
                Int | IntLiteral(_) => (Op::AddInt, range),
                _ => unreachable!(),
            },
            Sub => match lhs_type {
                Float | FloatLiteral(_) => (Op::SubFloat, range),
                Int | IntLiteral(_) => (Op::SubInt, range),
                _ => unreachable!(),
            },
            Mul => match lhs_type {
                Float | FloatLiteral(_) => (Op::MulFloat, range),
                Int | IntLiteral(_) => (Op::MulInt, range),
                _ => unreachable!(),
            },
            Div => match lhs_type {
                Float | FloatLiteral(_) => (Op::DivFloat, range),
                Int | IntLiteral(_) => (Op::DivInt, range),
                _ => unreachable!(),
            },
            Concat => match lhs_type {
                String | StringLiteral(_) => (Op::ConcatString, range),
                _ => unreachable!(),
            },
            Rem => todo!(),
            Exp => todo!(),
            Path => todo!(),
            Eq => match lhs_type {
                Float | FloatLiteral(_) => (Op::EqFloat, range),
                Int | IntLiteral(_) => (Op::EqInt, range),
                String | StringLiteral(_) => (Op::EqString, range),

                _ => unreachable!(),
            },
            Ne => match lhs_type {
                Float | FloatLiteral(_) => (Op::NeFloat, range),
                Int | IntLiteral(_) => (Op::NeInt, range),
                String | StringLiteral(_) => (Op::NeString, range),

                _ => unreachable!(),
            },
        });

        code
    }

    #[must_use]
    fn synth_unary_expr(
        &mut self,
        expr_idx: Idx<Expr>,
        expr: &UnaryExpr,
        context: &hir::Context,
    ) -> Code {
        let UnaryExpr { op, expr: idx } = expr;
        let expr_type = context.type_of_expr(*idx);
        let range = context.range_of(expr_idx);

        let mut code = self.synth_expr(*idx, context);

        use Type::*;
        use UnaryOp::*;
        code.append(match (op, expr_type) {
            (Neg, Int) | (Neg, IntLiteral(_)) => synth_op(Op::NegateInt, range).into(),
            (Neg, Float) | (Neg, FloatLiteral(_)) => synth_op(Op::NegateFloat, range).into(),

            (Not, Bool) | (Not, BoolLiteral(_)) => synth_op(Op::NotBool, range).into(),

            (IntoString, Bool) | (IntoString, BoolLiteral(_)) => {
                synth_op_u8(Op::IntoString, range, IntoStringOperand::Bool as u8)
            }
            (IntoString, Float) | (IntoString, FloatLiteral(_)) => {
                synth_op_u8(Op::IntoString, range, IntoStringOperand::Float as u8)
            }
            (IntoString, Int) | (IntoString, IntLiteral(_)) => {
                synth_op_u8(Op::IntoString, range, IntoStringOperand::Int as u8)
            }

            _ => unreachable!(),
        });

        code
    }

    #[must_use]
    fn synth_local_def(
        &mut self,
        local_key: LocalDefKey,
        value: Idx<Expr>,
        context: &hir::Context,
    ) -> Code {
        let mut code = self.synth_expr(value, context);

        let local_type = context.type_of_expr(value);
        let local_size = word_size_of(local_type);

        // TODO: is this needed...?
        // The value being set should already be on top of the stack, isn't this
        // basically a noop in that case that could be skipped?
        //
        // `let a = 1234`
        // will generate:
        // PushInt 1234
        // SetLocal offset: N
        //
        // Seems like the SetLocal is writing the same value to the same slot at the top
        // of the stack.
        let set_op = match (local_type, local_size) {
            (Type::String | Type::StringLiteral(_), _) => Op::SetLocalString,
            (_, 0) => unreachable!("?"),
            (_, 1) => Op::SetLocal,
            (_, 2) => Op::SetLocal2,
            (_, 4) => Op::SetLocal4,
            _ => Op::SetLocalN,
        };
        code.push((set_op, TextRange::default()));
        code.extend_from_slice(&self.curr().next_stack_slot_index.to_le_bytes());
        if set_op == Op::SetLocalN {
            code.extend_from_slice(&local_size.to_le_bytes());
        }

        let curr = self.curr_mut();

        curr.add_local(local_key, local_size);
        curr.push_alloc_type(local_type);

        code
    }

    #[must_use]
    fn synth_block_expr(&mut self, expr: &BlockExpr, context: &hir::Context) -> Code {
        let BlockExpr { exprs } = expr;

        let start_stack_slot_size = self.curr().next_stack_slot_index;

        let mut code = Code::default();
        for expr in exprs {
            code.append(self.synth_expr(*expr, context));
        }

        // TODO: handle return value
        let slots_to_pop = self.curr().next_stack_slot_index - start_stack_slot_size;
        code.append(self.synth_pops(slots_to_pop, TextRange::default(), true));
        self.curr_mut().next_stack_slot_index = start_stack_slot_size;

        code
    }

    #[must_use]
    fn synth_return(&mut self, main_implicit_return: bool) -> Code {
        let curr = self.curr();
        let return_slots = curr.return_slots;

        // slots to pop should be (total words of locals) + (return slots) because
        // at this point the return values is on the top of the stack. We need to SetLocal
        // that return value into the slot zero which is reserved for the return
        let slots_to_pop = curr.next_stack_slot_index - return_slots - FUNC_PTR_SLOT;

        let pops = self.synth_pops(slots_to_pop, TextRange::default(), false);

        // return value slots are allotted as a local at offset zero
        let set_op = match return_slots {
            0 => None,
            1 => Some(Op::SetLocal),
            2 => Some(Op::SetLocal2),
            4 => Some(Op::SetLocal4),
            _ => Some(Op::SetLocalN),
            // No need for SetLocalString/SetLocalArray because this is a *move*
            // of the value from the top of the stack to the bottom.
        };
        let mut code = Code::default();
        if let Some(set_op) = set_op {
            let mut set_return_slots: Code = if main_implicit_return {
                // main exits successfully with return code 0
                synth_int_constant(0, TextRange::default())
            } else {
                Code::default()
            };
            set_return_slots.append(synth_op(set_op, TextRange::default()).into());
            set_return_slots.push_u16(0); // return slots are offset: 0
            if set_op == Op::SetLocalN {
                set_return_slots.extend_from_slice(&return_slots.to_le_bytes());
            }

            // Put the return value in the right spot
            code.append(set_return_slots);

            // pops for top of stack value
            match return_slots {
                0 => {}
                1 => code.append(Code::from_op(Op::Pop1, TextRange::default())),
                2 => code.append(Code::from_op(Op::Pop2, TextRange::default())),
                4 => code.append(Code::from_op(Op::Pop4, TextRange::default())),
                n => {
                    code.append(synth_op_u16(Op::PopN, TextRange::default(), n));
                }
            };
        }
        // pops for the locals (including parameters)
        code.append(pops);

        // return from the function, which will pop the function pointer
        code.append(Code::from_op(Op::Return, TextRange::default()));

        code
    }

    #[must_use]
    fn synth_pops(
        &mut self,
        slots_to_pop: u16,
        range: TextRange,
        consume_alloc_types: bool,
    ) -> Code {
        let curr = self.curr_mut();
        let start = curr.alloc_types.len() - slots_to_pop as usize;

        // FIXME: early return statements are consuming these, which
        // breaks the codegen for natural returns
        let mut code = Code::default();

        // TODO: collapse adjacent plain pops to reduce emitted code
        // for example, instead of "Pop1; Pop1", emit a Pop2
        let alloc_types: Vec<AllocType> = if consume_alloc_types {
            curr.alloc_types.drain(start..).rev().collect()
        } else {
            curr.alloc_types[start..].iter().rev().cloned().collect()
        };

        alloc_types.into_iter().for_each(|item| match item {
            AllocType::WordN(n) => match n {
                0 => {}
                1 => code.push((Op::Pop1, range)),
                2 => code.push((Op::Pop2, range)),
                4 => code.push((Op::Pop4, range)),
                n => {
                    code.push((Op::PopN, range));
                    code.push_u16(n);
                }
            },
            AllocType::String => code.push((Op::PopString, range)),
            // TODO: implement Array
            AllocType::Array => code.push((Op::Pop2, range)),
            AllocType::Rc => code.push((Op::PopRc, range)),
            AllocType::Gc => code.push((Op::PopGc, range)),
            AllocType::Noop => {}
        });
        code
    }

    /// The jump offsets for if/else operations are not backpatched, they are synthesized
    /// (synth) first rather than directly writing to the chunk. That allows us to synth
    /// the "then branch" of an if/else and get its byte length to write the jump offset
    /// before writing the "then branch", which was easier to implement.
    ///
    /// This could be rewritten to use backpatching if there is a performance hit
    /// of the intermediate allocations.
    #[must_use]
    fn synth_if_expr(&mut self, expr: &IfExpr, context: &hir::Context) -> Code {
        let IfExpr {
            condition,
            then_branch,
            else_branch,
        } = expr;
        let mut code = self.synth_expr(*condition, context);

        code.push((Op::JumpIfFalse, TextRange::default()));

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
            code.push((Op::Jump, TextRange::default()));

            // synth the else_branch first, to calculate Op::Jump offset
            let else_code = self.synth_expr(*else_branch, context);

            let jump_operand = (else_code.bytes.len() as i32).to_le_bytes();
            code.extend_from_slice(&jump_operand);

            code.append(else_code);
        }

        code
    }

    #[must_use]
    fn synth_call_expr(
        &mut self,
        expr_idx: Idx<Expr>,
        expr: &CallExpr,
        context: &hir::Context,
    ) -> Code {
        let CallExpr {
            callee,
            args,
            callee_path,
        } = expr;
        let return_ty = &cast!(context.type_of_expr(*callee), Type::Function).return_ty;
        let return_slots = word_size_of(return_ty);

        let mut code = Code::default();

        for arg in args {
            code.append(self.synth_expr(*arg, context));
        }
        let arg_types: Vec<&Type> = args.iter().map(|idx| context.type_of_expr(*idx)).collect();

        // TODO: putting builtins into a "core" library and loading them into a "prelude" for name resolution
        if callee_path == "print" {
            code.append(self.synth_builtin_call(callee_path, arg_types));
        } else {
            code.append(self.synth_expr(*callee, context));
            code.push((Op::CallFunction, context.range_of(expr_idx)));
            code.push_u16(return_slots);
        }

        code
    }

    #[must_use]
    fn synth_local_ref(
        &mut self,
        expr_idx: Idx<Expr>,
        expr: &LocalRefExpr,
        context: &hir::Context,
    ) -> Code {
        let (slot_number, slot_size) = self.curr().stack_slots[&expr.key];
        let ty = context.type_of_expr(expr_idx);
        let op = match (ty, slot_size) {
            (Type::String | Type::StringLiteral(_), _) => Op::GetLocalString,
            (_, 0) => unreachable!("or could a variable be assigned to unit?"),
            (_, 1) => Op::GetLocal,
            (_, 2) => Op::GetLocal2,
            (_, 4) => Op::GetLocal4,

            _ => Op::GetLocalN,
        };

        let range = context.range_of(expr_idx);
        let mut code = Code::from_op(op, range);

        code.extend_from_slice(&slot_number.to_le_bytes());

        if op == Op::GetLocalN {
            code.extend_from_slice(&slot_size.to_le_bytes());
        }

        code
    }

    #[must_use]
    fn synth_builtin_call(&mut self, path: &str, arg_types: Vec<&Type>) -> Code {
        if path == "print" {
            // TODO: "print" is just for String. Provide a temporary IntoString operator until type parameters are
            // implemented, then "print" can take a `Into<String>`
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

    #[must_use]
    fn synth_builtin(&mut self, builtin_idx: u8, range: TextRange) -> Code {
        let mut code = Code::default();
        code.push((Op::CallBuiltin, range));
        code.push_u8(builtin_idx);

        code
    }
}

#[must_use]
fn synth_op(op: Op, range: TextRange) -> (u8, TextRange) {
    (op as u8, range)
}

#[must_use]
fn synth_op_u8(op: Op, range: TextRange, operand: u8) -> Code {
    let mut code = Code::from_op(op, range);
    code.push_u8(operand);
    code
}

#[must_use]
fn synth_op_u16(op: Op, range: TextRange, operand: u16) -> Code {
    let mut code = Code::from_op(op, range);
    code.push_u16(operand);
    code
}

#[must_use]
fn synth_op_u32(op: Op, range: TextRange, operand: u32) -> Code {
    let mut code = Code::from_op(op, range);
    code.push_u32(operand);
    code
}

#[must_use]
fn synth_op_operands(op: Op, range: TextRange, operands: &[u8]) -> Code {
    let mut code = Code::from_op(op, range);
    code.extend_from_slice(operands);

    code
}

#[must_use]
fn synth_bool_constant(value: bool, range: TextRange) -> Code {
    match value {
        true => synth_op(Op::PushTrue, range),
        false => synth_op(Op::PushFalse, range),
    }
    .into()
}

#[must_use]
fn synth_float_constant(value: f64, range: TextRange) -> Code {
    synth_op_operands(Op::PushFloat, range, &value.to_le_bytes())
}

#[must_use]
fn synth_int_constant(value: VMInt, range: TextRange) -> Code {
    synth_op_operands(Op::PushInt, range, &value.to_le_bytes())
}

/// Represents the allocation type of a value.
///
/// The allocation type is important to determine which
/// pop instruction needs to be emitted for that value.
///
/// For example, a `Rc` (reference-counted) allocated value
/// needs the Op::Rc instruction emitted so the Rc pointer
/// can be constructed from the raw pointer, then dropped to
/// reduce the strong count on that Rc.
#[derive(Debug, Clone)]
enum AllocType {
    WordN(u16),

    String,
    Array,
    Rc,
    Gc,

    Noop,
}

#[derive(Debug)]
pub struct FunctionCodegen {
    id: usize,

    /// Bytecode chunk being generated
    chunk: FunctionChunk,

    /// Total slots allocated for function return values
    return_slots: u16,

    /// Running index of the stack slots that have been allocated for this function
    ///
    /// Includes return slots, self (function) pointer, parameters, and locals.
    next_stack_slot_index: u16,

    /// Map from a unique LocalDef to (slot_number, slot_size) in the stack
    stack_slots: HashMap<LocalDefKey, (u16, u16)>,

    /// Keeps track of values to be able to emit specific pop instructions
    ///
    /// There are specialized pop instructions to handle allocated values, such
    /// as Op::PopGc and Op::PopRc. Any time we need to emit bytecode Pop instructions
    /// we pop from this which tells us what kind of Pop instruction to emit.
    alloc_types: Vec<AllocType>,
}

const FUNC_PTR_SLOT: u16 = 1;

impl FunctionCodegen {
    fn new(id: usize, name: &str, parameter_slots: u16, return_slots: u16) -> Self {
        Self {
            chunk: FunctionChunk::new(name, parameter_slots),
            id,
            return_slots,
            next_stack_slot_index: return_slots + FUNC_PTR_SLOT,
            alloc_types: Default::default(),
            stack_slots: Default::default(),
        }
    }

    fn add_local(&mut self, local_key: LocalDefKey, local_size: u16) {
        self.stack_slots
            .insert(local_key, (self.next_stack_slot_index, local_size));
        self.next_stack_slot_index += local_size;
    }

    // TODO: after Arrays are implemented, this can be moved inside of `add_local`
    // since there's nowhere that needs to call just this function
    fn push_alloc_type(&mut self, local_type: &Type) {
        let local_size = word_size_of(local_type);
        let alloc_type = match (local_type, local_size) {
            (Type::String | Type::StringLiteral(_), _) => AllocType::String,
            (Type::Function(_), _) => AllocType::Rc,

            (_, 0) => unreachable!(),
            (_, 1) => AllocType::WordN(1),
            (_, 2) => AllocType::WordN(2),
            (_, 4) => AllocType::WordN(4),

            (_, n) => AllocType::WordN(n),
        };

        match alloc_type {
            AllocType::WordN(n) => {
                self.alloc_types.push(AllocType::WordN(n));
                for _ in 0..n - 1 {
                    self.alloc_types.push(AllocType::Noop);
                }
            }
            AllocType::Array => todo!(),
            AllocType::String => {
                self.alloc_types.push(AllocType::String);
                self.alloc_types.push(AllocType::Noop); // 2nd word of string
            }
            p => self.alloc_types.push(p),
        };
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
        Type::Unit => 0,

        Type::Function(_) => 1, // pointer size
        Type::Array(_) => 2,

        Type::Undetermined => unreachable!(),
        Type::Error => unreachable!(),
        Type::Top => 0,
        Type::Bottom => 0,
    } as u16)
}

/// For a given Type, provides the bytecode required to
/// pop it from the stack.
#[must_use]
fn pop_code_of(ty: &Type) -> Code {
    let range = TextRange::default();

    match ty {
        Type::Unit => Code::default(),

        Type::Bool
        | Type::BoolLiteral(_)
        | Type::FloatLiteral(_)
        | Type::IntLiteral(_)
        | Type::Float
        | Type::Int => Code::from_op(Op::Pop1, range),
        Type::StringLiteral(_) | Type::String => Code::from_op(Op::PopString, range),
        Type::Function(_) => Code::from_op(Op::Pop1, range),
        Type::Array(_) => todo!(),

        Type::Bottom => Code::default(),
        Type::Top => unreachable!("Top is not a constructable value"),
        Type::Undetermined | Type::Error => unreachable!(),
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
pub unsafe trait BytecodeRead {}

unsafe impl BytecodeRead for u8 {}
unsafe impl BytecodeRead for u16 {}
unsafe impl BytecodeRead for u32 {}
unsafe impl BytecodeRead for VMInt {}
unsafe impl BytecodeRead for VMFloat {}
unsafe impl BytecodeRead for IntoStringOperand {}
unsafe impl BytecodeRead for PushStringOperand {}
unsafe impl BytecodeRead for ReturnType {}
