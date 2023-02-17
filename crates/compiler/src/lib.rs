use codegen::Chunk;

pub fn compile(input: &str) -> Chunk {
    let parse_tree = parser::parse(input);
    let ast: ast::Root = parse_tree.into();
    let (exprs, context) = hir::lower(ast);

    codegen::codegen(&exprs, &context)
}
