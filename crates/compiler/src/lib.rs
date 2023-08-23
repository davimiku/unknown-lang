pub use hir::Diagnostic;
use vm_codegen::ProgramChunk;

pub fn compile(input: &str, verbose: bool) -> Result<ProgramChunk, Vec<Diagnostic>> {
    let (exprs, mut context) = hir::lower(input, hir::LowerTarget::Module);

    if verbose {
        println!("{context}");
    }

    if context.diagnostics.is_empty() {
        Ok(vm_codegen::codegen(&exprs, &mut context))
    } else {
        Err(context.diagnostics)
    }
}
