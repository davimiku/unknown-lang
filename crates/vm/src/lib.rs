use chunk::{Chunk, Op};
use exec::VM;

mod exec;

pub fn run() {
    let mut chunk = Chunk::new();

    chunk.write_float_constant(1.2, 123);
    chunk.write_float_constant(3.4, 123);
    chunk.write(Op::FAdd, 123);

    chunk.write_float_constant(5.6, 123);
    chunk.write(Op::FDiv, 123);

    chunk.write_float_constant(1.0, 123);
    chunk.write(Op::FSub, 123);

    chunk.write_float_constant(20.0, 123);
    chunk.write(Op::FMul, 123);

    chunk.write(Op::Ret, 123);

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
