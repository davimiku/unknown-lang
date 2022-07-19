use crate::Chunk;

type Index = usize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    /// Integer constant value
    Constant(Index),

    /// Float constant value
    FConstant(Index),

    /// Integer addition
    Add,

    /// Integer subtraction
    Sub,

    /// Integer multiplication
    Mul,

    /// Integer division
    Div,

    /// Float addition
    FAdd,

    /// Float subtraction
    FSub,

    /// Float multiplication
    FMul,

    /// Float division
    FDiv,

    /// Return from a function
    Ret,
}

impl Op {
    pub fn disassemble(&self, chunk: &Chunk) -> String {
        match self {
            // Op::Constant(i) => {
            //     // let val = chunk.constants[*i];
            //     // format!("{self:?} {val}")
            // }
            _ => format!("{self:?}"),
        }
    }
}
