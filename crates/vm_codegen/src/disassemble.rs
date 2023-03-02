use std::mem::size_of;

use vm_types::string::VMString;

use crate::{Chunk, Op, Readable, VMFloat, VMInt};

// TODO: pull builtins out to a separate crate?
// This kind of idx -> builtin map would be used in vm_codegen and vm
pub const BUILTIN_NAMES: [&str; 5] = [
    "print_str_constant",
    "print_str",
    "print_int",
    "print_float",
    "print_bool",
];

fn read<T: Readable>(chunk: &Chunk, offset: &mut usize) -> T {
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
    pub fn disassemble(&self, chunk: &Chunk, offset: usize) -> usize {
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
                let s = read::<VMString>(chunk, &mut offset);

                let s = s.to_string(&chunk.constants);

                print!("\"{s}\"");
            }

            Op::GetLocal
            | Op::GetLocal2
            | Op::GetLocal4
            | Op::SetLocal
            | Op::SetLocal2
            | Op::SetLocal4 => {
                let slot_index = read::<u16>(chunk, &mut offset);

                print!("index: {slot_index}");
            }
            Op::GetLocalN | Op::SetLocalN => {
                let slot_index = read::<u16>(chunk, &mut offset);
                let slot_size = read::<u16>(chunk, &mut offset);

                print!("index: {slot_index}, size:{slot_size}");
            }
            Op::Builtin => {
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
            _ => {}
        };

        offset
    }
}
