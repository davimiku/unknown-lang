use crate::Chunk;

pub(crate) type Idx = u32;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    /// Boolean true constant value
    TrueConstant,

    /// Boolean false constant value
    FalseConstant,

    /// Integer constant value
    IConstant(Idx),

    /// Float constant value
    FConstant(Idx),

    /// String constant value
    SConstant(Idx),

    /// Integer addition
    IAdd,

    /// Integer subtraction
    ISub,

    /// Integer multiplication
    IMul,

    /// Integer division
    IDiv,

    /// Float addition
    FAdd,

    /// Float subtraction
    FSub,

    /// Float multiplication
    FMul,

    /// Float division
    FDiv,

    // push local?
    // pop local?
    /// Call to a built-in function
    /// Needs index of the built-in
    /// TODO: how to indicate args?
    Builtin(Idx),

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
