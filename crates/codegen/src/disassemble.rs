use std::mem::size_of;

use crate::{Chunk, Op};

// TODO: pull builtins out to a separate crate?
// This kind of idx -> builtin map would be used in codegen and vm
pub const BUILTIN_NAMES: [&str; 4] = [
    "print_str_constant",
    "print_str",
    "print_int",
    "print_float",
];

impl Op {
    /// Prints a dissassembled form of the instruction.
    ///
    /// Any variant with an operand in the bytecode must
    ///
    /// Returns the new offset to use for the next instruction.
    pub fn disassemble(&self, chunk: &Chunk, offset: usize) -> usize {
        print!("{self:?}    ");

        let mut offset = offset + size_of::<Op>();
        match self {
            Op::PushInt => {
                let int = chunk.read::<i64>(offset);
                offset += size_of::<i64>();

                print!("{int}");
            }
            Op::PushFloat => {
                let float = chunk.read::<f64>(offset);
                offset += size_of::<f64>();

                print!("{float}");
            }
            Op::PushString => {
                let (idx, len) = chunk.read_str(offset);
                offset += size_of::<(u64, u64)>();

                let s = chunk.get_str_constant(idx, len);
                print!("\"{s}\"");
            }
            Op::ConcatString => {
                //
            }
            Op::Builtin => {
                let builtin_idx = chunk.read::<u8>(offset);
                offset += size_of::<u8>();

                print!("{}", BUILTIN_NAMES[builtin_idx as usize])
            }
            _ => {}
        };

        offset
    }
}
