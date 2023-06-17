pub use hir::Diagnostic;
use hir::Interner;
use vm_codegen::ProgramChunk;

pub fn compile(input: &str, verbose: bool) -> Result<ProgramChunk, Vec<Diagnostic>> {
    let parse_tree = parser::parse(input);
    let ast: ast::Root = parse_tree.into();
    let mut interner = Interner::default();
    let (exprs, mut context) = hir::lower_ast(&ast, &mut interner);

    if verbose {
        println!("{context}");
    }

    if context.diagnostics.is_empty() {
        Ok(vm_codegen::codegen(&exprs, &mut context))
    } else {
        Err(context.diagnostics)
    }
}
