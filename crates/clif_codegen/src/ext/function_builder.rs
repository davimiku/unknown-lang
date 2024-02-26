use cranelift::prelude::*;

pub(crate) trait FunctionBuilderExt {
    fn bool_const(&mut self, val: bool) -> Value;
}

impl FunctionBuilderExt for FunctionBuilder<'_> {
    fn bool_const(&mut self, val: bool) -> Value {
        let val = val as i64;
        self.ins().iconst(types::I64, val)
    }
}
