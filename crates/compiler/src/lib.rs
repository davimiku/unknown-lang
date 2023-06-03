pub use hir::Diagnostic;
use hir::Interner;
use vm_codegen::ProgramChunk;

pub fn compile(input: &str) -> Result<ProgramChunk, Vec<Diagnostic>> {
    let parse_tree = parser::parse(input);
    let ast: ast::Root = parse_tree.into();
    let mut interner = Interner::default();
    let (exprs, mut context) = hir::lower(&ast, &mut interner);

    if context.diagnostics.is_empty() {
        Ok(vm_codegen::codegen(&exprs, &mut context))
    } else {
        Err(context.diagnostics)
    }
}
