use super::*;
use hir::Interner;

fn generate_chunk(input: &str) -> ProgramChunk {
    let mut interner = Interner::default();
    let (root, mut context) = hir::lower_from_input(input, &mut interner);
    codegen(&root, &mut context)
}

#[test]
fn print_call() {
    let input = "print \"Hello\"";

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn string_concatenation() {
    let input = r#"print ("Hello, " ++ "World!")"#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn string_concat_local_def() {
    // if this is a block
    // then already it has 3 stack slots used
    let input = r#"
    let a = "Hello "
    let b = "World"
    let c = a ++ b
    "#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn if_else() {
    let input = "if true { print 1 } else { print 0 }";

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn local_def() {
    let input = r#"
let a = "Hello""#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn print_local_def() {
    let input = r#"
let a = "Hello"
print a"#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn function_def() {
    let input = r#"
let repeat = (s: String) -> s ++ s
repeat "Hello"
"#;

    let chunk = generate_chunk(input);
    println!("{}\n{}", chunk.main(), chunk.functions[0]);
}

#[test]
fn local_defs_with_call() {
    let input = r#"
let a = "Hello"
let b = " World"
print b
print a"#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
}

#[test]
fn function_def_call() {
    // FIXME: `print_my_string` isn't being saved as the name of the function

    let input = r#"
let print_my_string = (s: String) -> { print s }
print_my_string "Hello ""#;

    let chunk = generate_chunk(input);
    println!("{}", chunk.main());
    println!();
    println!("{}", chunk.functions[0]);
}

#[test]
fn function_def_call_return() {
    let input = r#"
let repeat = (s: String) -> s ++ s
let hello_hello = repeat "Hello "
print hello_hello"#;

    let chunk = generate_chunk(input);
    println!("{}\n{}", chunk.main(), chunk.functions[0]);
}
