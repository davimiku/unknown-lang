mod arithmetic;
mod branches;
mod calls;
mod comparison;
mod loops;
mod mutability;
mod params;
mod scopes;

use hir::ContextDisplay;

use crate::display::write_module;
use crate::Module;

use crate::construct_module;

/// Lowers the provided input to MIR
///
/// Uses "normal mode" which represents a module
fn check_module(input: &str, expected: &str) {
    let (module, context) = hir::lower(input);
    if !context.diagnostics.is_empty() {
        for diagnostic in &context.diagnostics {
            println!("{}", diagnostic.display(&context));
        }
        assert_eq!(context.diagnostics, vec![]);
    }
    let (module, context) = construct_module(&module, &context);

    check(module, context, expected)
}

fn check(module: Module, context: &hir::Context, expected: &str) {
    let mut initial_indent = 0;
    let mut w = Vec::new();
    write_module(&module, &mut w, context, &mut initial_indent).expect("written successfully");

    let actual = String::from_utf8(w).expect("bytes to be valid UTF-8");

    let actual = actual.trim();
    let expected = expected.trim();
    if actual != expected {
        eprintln!("expected:\n{expected}");
        eprintln!("actual:\n{actual}");
        eprintln!("diff:");
        text_diff::print_diff(expected, actual, "");
        panic!("Expected did not match actual, see printed diff.");
    }
}
