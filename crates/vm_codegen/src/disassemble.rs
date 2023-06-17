use std::mem::size_of;

use crate::{
    op::{IntoStringOperand, PushStringOperand},
    BytecodeRead, FunctionChunk, Op, VMFloat, VMInt,
};

// TODO: pull builtins out to a separate crate?
// This kind of idx -> builtin map would be used in vm_codegen and vm
pub const BUILTIN_NAMES: [&str; 5] = [
    "print_str_constant",
    "print_str",
    "print_int",
    "print_float",
    "print_bool",
];

fn read<T: BytecodeRead>(chunk: &FunctionChunk, offset: &mut usize) -> T {
    let val = chunk.read::<T>(*offset);

    *offset += size_of::<T>();

    val
}

impl Op {
    /// Prints a dissassembled form of the instruction.
    ///
    /// Any variant with an operand in the bytecode must add an implementation here.
    ///
    /// Returns the new offset to use for the next instruction.
    pub fn disassemble(&self, chunk: &FunctionChunk, offset: usize) -> usize {
        print!("{self:?}      ");

        let mut offset = offset + size_of::<Op>();
        match self {
            Op::PushInt => {
                let int = read::<VMInt>(chunk, &mut offset);

                print!("{int}");
            }
            Op::PushFloat => {
                let float = read::<VMFloat>(chunk, &mut offset);

                print!("{float}");
            }
            Op::PushString => {
                let PushStringOperand { len, offset: idx } =
                    read::<PushStringOperand>(chunk, &mut offset);
                let start = idx as usize;
                let end = start + len as usize;
                let bytes = chunk.constants_slice(start..end);

                let s = std::str::from_utf8(bytes).unwrap();

                print!("\"{s}\"");
            }
            Op::IntoString => {
                let kind = read::<IntoStringOperand>(chunk, &mut offset);

                match kind {
                    IntoStringOperand::Bool => print!("Bool"),
                    IntoStringOperand::Float => print!("Float"),
                    IntoStringOperand::Int => print!("Int"),
                }
            }

            Op::GetLocal
            | Op::GetLocal2
            | Op::GetLocal4
            | Op::GetLocalString
            | Op::SetLocal
            | Op::SetLocal2
            | Op::SetLocal4
            | Op::SetLocalString => {
                let slot_index = read::<u16>(chunk, &mut offset);

                print!("index: {slot_index}");
            }
            Op::GetLocalN | Op::SetLocalN => {
                let slot_index = read::<u16>(chunk, &mut offset);
                let slot_size = read::<u16>(chunk, &mut offset);

                print!("index: {slot_index}, size:{slot_size}");
            }
            Op::CallFunction => {
                let return_slots = read::<u16>(chunk, &mut offset);

                print!("return_slots: {return_slots}");
            }
            Op::CallBuiltin => {
                let builtin_idx = read::<u8>(chunk, &mut offset);

                print!("{}", BUILTIN_NAMES[builtin_idx as usize])
            }
            Op::Jump | Op::JumpIfFalse => {
                let jump_offset = read::<u32>(chunk, &mut offset);

                print!("{jump_offset}")
            }

            Op::PopN => {
                let num_slots = read::<u16>(chunk, &mut offset);

                print!("{num_slots}")
            }
            Op::PushLocalFunc => {
                let function_idx = read::<u32>(chunk, &mut offset);

                print!("func: {function_idx}")
            }
            _ => {}
        };

        offset
    }
}
