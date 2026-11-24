mod arithmetic;
mod branches;
mod calls;
mod comparison;
mod loops;
mod mutability;
mod params;
mod product_types;
mod scopes;
mod sum_types;

use hir::ContextDisplay;

use crate::display::write_module;
use crate::Module;

/// Lowers the provided input to MIR
///
/// Uses "normal mode" which represents a module
fn check_module(input: &str, expected: &str) {
    let mut context = hir::Context::new(hir::Interner::default());
    let mut modules = context
        .load_stdlib()
        .expect("standard library to be available during MIR tests");
    let hir_module = context.load_module(input, hir::SourceKind::User);
    if !context.diagnostics.is_empty() {
        for diagnostic in context.diagnostics.iter() {
            let range = diagnostic.range();
            println!("expr: `{}`", &input[range]);
            println!("error: '{}'", diagnostic.display(&context));
        }
        assert_eq!(context.diagnostics, vec![]);
    }
    modules.push(hir_module);
    let primary_module_index = modules.len() - 1;
    let (module, context) =
        crate::construct_modules(&modules, &modules[primary_module_index], &mut context);

    check(module, context, expected)
}

fn check(module: Module, context: &hir::Context, expected: &str) {
    let mut initial_indent = 0;
    let mut w = Vec::new();
    write_module(&module, &mut w, context, &mut initial_indent).expect("written successfully");

    let actual = String::from_utf8(w).expect("bytes to be valid UTF-8");

    let actual = omit_stdlib_functions(&actual);
    let actual = normalize_type_module_ids(actual.trim());
    let expected = normalize_type_module_ids(expected.trim());
    if actual != expected {
        eprintln!("expected:\n{expected}");
        eprintln!("actual:\n{actual}");
        eprintln!("diff:");
        text_diff::print_diff(&expected, &actual, "");
        panic!("Expected did not match actual, see printed diff.");
    }
}

/// MIR snapshots focus on lowering of the supplied module. Standard-library
/// implementations are covered by their own integration tests and should not
/// force every snapshot to change when a core API gains a regular helper.
fn omit_stdlib_functions(input: &str) -> String {
    let mut functions = Vec::new();
    let mut current = String::new();

    for line in input.lines() {
        if line.starts_with("fun ") && !current.is_empty() {
            functions.push(std::mem::take(&mut current));
        }
        current.push_str(line);
        current.push('\n');
    }
    if !current.is_empty() {
        functions.push(current);
    }

    functions
        .into_iter()
        .filter(|function| !function.starts_with("fun stdlib_"))
        .map(|function| function.trim().to_owned())
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Module ids depend on the standard-library module inventory. The MIR tests
/// assert lowering behavior, not the allocation order of those modules.
fn normalize_type_module_ids(input: &str) -> String {
    let characters: Vec<char> = input.chars().collect();
    let mut output = String::with_capacity(input.len());
    let mut index = 0;
    while index < characters.len() {
        if characters[index] == '~' {
            let mut end = index + 1;
            while end < characters.len() && characters[end].is_ascii_digit() {
                end += 1;
            }
            if end > index + 1 && end < characters.len() && characters[end] == '.' {
                output.push_str("~module.");
                index = end + 1;
                continue;
            }
        }
        output.push(characters[index]);
        index += 1;
    }
    output
}
