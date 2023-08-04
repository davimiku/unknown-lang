use cranelift::prelude::{settings, Configurable};
use cranelift_jit::JITBuilder;

use crate::builtins;

pub(crate) trait JITBuilderExt {
    fn with_builtins() -> Self;
}

impl JITBuilderExt for JITBuilder {
    fn with_builtins() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {msg}");
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        builder.symbol(builtins::PRINT_INT, builtins::print_int as *const u8);
        builder.symbol(builtins::PRINT_FLOAT, builtins::print_float as *const u8);
        builder.symbol(builtins::PRINT_BOOL, builtins::print_bool as *const u8);

        builder
    }
}
