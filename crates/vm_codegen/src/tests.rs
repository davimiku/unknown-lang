use super::*;
use hir::Interner;

fn generate_chunk(input: &str) -> FunctionChunk {
    let mut interner = Interner::default();
    let (root, context) = hir::lower_from_input(input, &mut interner);
    codegen(&root, &context)
}

#[test]
fn test_print_call() {
    let input = "print 1";

    let chunk = generate_chunk(input);
    println!("{chunk}");
}

#[test]
fn test_string_concatenation() {
    let input = r#"print ("Hello, " ++ "World!")"#;

    let chunk = generate_chunk(input);
    println!("{chunk}")
}

#[test]
fn test_if_else() {
    let input = "if true { print 1 } else { print 0 }";

    let chunk = generate_chunk(input);
    println!("{chunk}");
}

#[test]
fn test_local_defs() {
    let input = r#"
let a = 5
let b = 8
print b
print a"#;

    let chunk = generate_chunk(input);
    println!("{chunk}");
}
