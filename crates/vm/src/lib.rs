use chunk::Chunk;
use exec::VM;

mod builtins;
mod exec;
mod macros;

pub fn run(chunk: &Chunk) {
    let mut vm = VM::new(chunk);
    vm.interpret();
}

#[cfg(test)]
mod tests {
    use chunk::{Chunk, Op};

    #[test]
    fn test_print_int() {
        // "print 1"
        let mut chunk = Chunk::new();

        chunk.write(Op::Builtin(0), 0);
        chunk.write_int_constant(1, 0);
        chunk.write(Op::Ret, 1);

        chunk.disassemble("print 1");

        super::run(&chunk);
    }

    #[test]
    fn test_print_float() {
        // "print 1.23"
        let mut chunk = Chunk::new();

        chunk.write(Op::Builtin(0), 0);
        chunk.write_float_constant(1.23, 0);
        chunk.write(Op::Ret, 1);

        chunk.disassemble("print 1.23");

        super::run(&chunk);
    }

    #[test]
    fn test_print_int_add() {
        // "print (1 + 2)"
        let mut chunk = Chunk::new();

        chunk.write_int_constant(1, 123);
        chunk.write_int_constant(2, 123);
        chunk.write(Op::IAdd, 123);

        chunk.write(Op::Builtin(0), 123);
        chunk.write(Op::Ret, 123);

        super::run(&chunk);
    }

    #[test]
    fn run() {
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

        super::run(&chunk);
    }
}
