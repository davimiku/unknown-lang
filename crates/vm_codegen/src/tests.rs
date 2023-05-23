use super::*;
use hir::Interner;

fn generate_chunk(input: &str) -> ProgramChunk {
    let mut interner = Interner::default();
    let (root, mut context) = hir::lower_from_input(input, &mut interner);
    assert_eq!(context.diagnostics, vec![]);
    codegen(&root, &mut context)
}

#[test]
fn int_literal() {
    let input = "1";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn string_literal() {
    let input = "\"Hello\"";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn multiple_string_literals() {
    let input = r#""a1"
"b2"
"c3"
"d4""#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn print_int() {
    let input = "print ~1";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn print_string() {
    let input = "print \"Hello\"";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn string_concatenation() {
    let input = r#"print ("Hello, " ++ "World!")"#;

    let program = generate_chunk(input);
    println!("{program}");
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

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn if_else() {
    let input = r#"if true { print "Yes" } else { print "No" }"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn local_def() {
    let input = r#"let a = 1234
    print ~a"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn print_local_def() {
    let input = r#"let a = "Hello"
print a"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn function_def() {
    let input = r#"
let repeat = (s: String) -> s ++ s
repeat "Hello"
"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn local_defs_with_call() {
    let input = r#"
let a = "Hello"
let b = " World"
print b
print a"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn function_def_call() {
    let input = r#"
let print_my_string = (s: String) -> { print s }
print_my_string "Hello ""#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn function_def_call_return() {
    let input = r#"
let repeat = (s: String) -> s ++ s
let hello_hello = repeat "Hello "
print hello_hello"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn unused_literals() {
    let input = r#"
1
2.0
true
"str""#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
fn return_expression() {
    let input = r#"return 1"#;

    let program = generate_chunk(input);
    println!("{program}");
}
