use chunk::Chunk;
use exec::{InterpretResult, VM};

mod builtins;
mod exec;
mod macros;

pub fn run(chunk: &Chunk) -> InterpretResult {
    let mut vm = VM::new(chunk);
    vm.interpret()
}

#[cfg(test)]
mod tests {
    use chunk::{Chunk, Op};

    #[test]
    fn test_print_int() {
        // "print 1"
        let mut chunk = Chunk::new();

        chunk.write_op(Op::Builtin, 0);
        // write arity / number of args
        chunk.write_int_constant(1, 0);
        // push function object?
        chunk.write_op(Op::Ret, 1);

        chunk.disassemble("print 1");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_print_float() {
        // "print 1.23"
        let mut chunk = Chunk::new();

        chunk.write_op(Op::Builtin, 0);
        // write arity / number of args
        chunk.write_float_constant(1.23, 0);
        // push function object?
        chunk.write_op(Op::Ret, 1);

        chunk.disassemble("print 1.23");

        super::run(&chunk).unwrap();
    }

    #[test]
    fn test_print_int_add() {
        // "print (1 + 2)"
        let mut chunk = Chunk::new();

        chunk.write_int_constant(1, 123);
        chunk.write_int_constant(2, 123);
        chunk.write_op(Op::AddInt, 123);

        chunk.write_op(Op::Builtin, 123);
        // push arity
        // push function object
        chunk.write_op(Op::Ret, 123);

        super::run(&chunk).unwrap();
    }

    #[test]
    fn run() {
        let mut chunk = Chunk::new();

        chunk.write_float_constant(1.2, 123);
        chunk.write_float_constant(3.4, 123);
        chunk.write_op(Op::AddFloat, 123);

        chunk.write_float_constant(5.6, 123);
        chunk.write_op(Op::DivFloat, 123);

        chunk.write_float_constant(1.0, 123);
        chunk.write_op(Op::SubFloat, 123);

        chunk.write_float_constant(20.0, 123);
        chunk.write_op(Op::MulFloat, 123);

        chunk.write_op(Op::Ret, 123);

        chunk.disassemble("test chunk");

        super::run(&chunk).unwrap();
    }
}
