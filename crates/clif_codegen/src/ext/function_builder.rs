use cranelift::prelude::*;

pub(crate) trait FunctionBuilderExt {
    fn bool_const(&mut self, val: bool) -> Value;
    fn idiv(&mut self, x: Value, y: Value) -> Value;
}

impl FunctionBuilderExt for FunctionBuilder<'_> {
    fn bool_const(&mut self, val: bool) -> Value {
        let val = val as i64;
        self.ins().iconst(types::I64, val)
    }

    // self.builder.ins().idiv(lhs, rhs) -- there is no idiv in CLIF
    // need to implement integer division ourselves
    // https://en.wikipedia.org/wiki/Division_algorithm
    fn idiv(&mut self, x: Value, y: Value) -> Value {
        // error if y != 0
        // error if y == -1 and x == i64::MIN

        // implement integer division
        todo!()
    }
}
