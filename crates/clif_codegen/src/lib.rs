#[cfg(test)]
use std::io::stdout;

use cranelift::codegen::verify_function;
use jit::JIT;
#[cfg(test)]
use mir::MirWrite;

mod builtins;
mod ext;
mod jit;
#[cfg(test)]
mod tests;
mod translate;

pub fn compile_function(input: &str) -> Result<*const u8, String> {
    let (program, context) = mir::construct_function(input);
    assert_eq!(context.diagnostics, vec![]);

    #[cfg(test)]
    {
        let mut stdout = stdout().lock();
        let _ = program.write(&mut stdout, context, &mut 0);
    }
    let mut jit = JIT::with_builtins();
    let func = program.main();
    jit.compile_function(func, context).map_err(|e| {
        dbg!(&e);
        e.to_string()
    })
}

pub fn compile_script(input: &str) -> Result<*const u8, String> {
    let (program, context) = mir::construct_script(input);

    let mut jit = JIT::with_builtins();
    let func = program.main();
    jit.compile_function(func, context).map_err(|e| {
        dbg!(&e);
        e.to_string()
    })
}
