use std::io;

use crate::display::MirWrite;
use hir::ContextDisplay;

use crate::construct;

fn check(input: &str) {
    let (root, hir_context) = hir::lower(input, hir::LowerTarget::Script);
    println!("{}", root.display(&hir_context));
    let program = construct(root, &hir_context);

    let mut initial_indent = 0;
    program
        .write(&mut io::stdout().lock(), &hir_context, &mut initial_indent)
        .expect("written successfully");
}

#[test]
fn basic_arithmetic() {
    let input = "3 + 4";
    check(input);
}
