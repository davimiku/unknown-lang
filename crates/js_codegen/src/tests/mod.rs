use crate::codegen;

mod conditionals;
mod functions;
mod scalars;
mod variables;

fn check(input: &str, expected: &str) {
    let (module, context) = hir::lower(input);

    let actual = codegen(&module, &context);

    assert_eq!(actual, expected);
}
