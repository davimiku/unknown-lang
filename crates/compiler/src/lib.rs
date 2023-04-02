use hir::Interner;
use vm_codegen::FunctionChunk;

pub fn compile(input: &str) -> FunctionChunk {
    let parse_tree = parser::parse(input);
    let ast: ast::Root = parse_tree.into();
    let mut interner = Interner::default();
    let (exprs, context) = hir::lower(&ast, &mut interner);

    vm_codegen::codegen(&exprs, &context)
}
