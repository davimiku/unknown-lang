use crate::value::Value;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Op {
    /// Constant value. usize is the index of the constant in the constants Vec
    Constant(usize),

    ///
    FAdd,

    FSubtract,

    FMultiply,

    FDivide,

    /// Unary negation of number value
    Negate,

    /// Return from a function
    Return,
}

impl Op {
    pub(crate) fn disassemble(&self, chunk: &Chunk) -> String {
        match self {
            Op::Constant(i) => {
                let val: Value = chunk.constants[*i];
                format!("{self:?} {val}")
            }
            _ => format!("{self:?}"),
        }
    }
}

pub(crate) struct Chunk {
    code: Vec<Op>,
    constants: Vec<Value>,
    lines: Vec<u32>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }
}

impl Chunk {
    pub(crate) fn new() -> Chunk {
        Default::default()
    }

    #[inline(always)]
    pub(crate) fn get_op(&self, i: usize) -> &Op {
        &self.code[i]
    }

    #[inline(always)]
    pub(crate) fn get_constant(&self, i: usize) -> Value {
        self.constants[i]
    }

    pub(crate) fn write(&mut self, op: Op, line: u32) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub(crate) fn write_constant(&mut self, value: Value, line: u32) {
        self.constants.push(value);
        let i = self.constants.len() - 1;
        self.write(Op::Constant(i), line);
    }

    pub(crate) fn disassemble(&self, name: &str) -> () {
        println!("== {name} ==");
        for (i, op) in self.code.iter().enumerate() {
            let line = self.lines[i];
            // TODO: print a "  | " if it has the same line as the previous Op
            println!("{:03} {} {}", i, line, op.disassemble(self))
        }
    }
}
