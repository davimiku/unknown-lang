use codegen::Chunk;

pub fn compile(input: &str) -> Chunk {
    let parse_tree = parser::parse(input);
    let ast: ast::Root = parse_tree.into();
    let (stmts, context) = hir::lower(ast);

    codegen::codegen(&stmts, context)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_literal() {
        let actual = compile("123");

        dbg!(actual);
    }
}
