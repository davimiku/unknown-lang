use super::*;

fn generate_chunk(input: &str) -> Chunk {
    let (root, context) = hir::lower_from_input(input);
    codegen(&root, &context)
}

#[test]
fn test_print_call() {
    let input = "print 1";

    let chunk = generate_chunk(input);
    println!("{chunk}");
}

#[test]
fn test_print_input() {
    //
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
let a = 1
print a"#;

    let chunk = generate_chunk(input);
    println!("{chunk}");
}
