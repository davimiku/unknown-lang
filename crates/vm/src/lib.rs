use chunk::{Chunk, Op};
use exec::VM;

mod chunk;
mod exec;
mod value;

pub fn run() {
    let mut chunk = Chunk::new();

    chunk.write_constant(1.2, 123);
    chunk.write_constant(3.4, 123);
    chunk.write(Op::FAdd, 123);

    chunk.write_constant(5.6, 123);
    chunk.write(Op::FDivide, 123);

    chunk.write_constant(1.0, 123);
    chunk.write(Op::FSubtract, 123);

    chunk.write_constant(20.0, 123);
    chunk.write(Op::FMultiply, 123);

    chunk.write(Op::Negate, 123);

    chunk.write(Op::Return, 123);

    chunk.disassemble("test chunk");

    let mut vm = VM::new(&chunk);
    vm.interpret();
}

#[cfg(test)]
mod tests {
    #[test]
    fn run() {
        super::run();
    }
}
