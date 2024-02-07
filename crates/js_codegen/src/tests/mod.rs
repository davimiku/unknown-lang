use crate::codegen;

mod conditionals;
mod functions;
mod scalars;
mod variables;

fn check(input: &str, expected: &str) {
    let (program, context) = hir::lower(input, hir::LowerTarget::Module);

    let actual = codegen(context.expr(program), &context);

    assert_eq!(actual, expected);
}
