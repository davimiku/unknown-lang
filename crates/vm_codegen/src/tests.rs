use super::*;

fn generate_chunk(input: &str) -> ProgramChunk {
    let (root, mut context) = hir::lower(input, hir::LowerTarget::Module);
    assert_eq!(context.diagnostics, vec![]);
    codegen(&root, &mut context)
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn int_literal() {
    let input = "1";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn string_literal() {
    let input = "\"Hello\"";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn multiple_string_literals() {
    let input = r#""a1"
"b2"
"c3"
"d4""#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn print_int() {
    let input = "print ~1";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn print_string() {
    let input = "print \"Hello\"";

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn string_concatenation() {
    let input = r#"print ("Hello, " ++ "World!")"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
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
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn if_else() {
    let input = r#"if true { print "Yes" } else { print "No" }"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn local_def() {
    let input = r#"let a = 1234
    print ~a"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn print_local_def() {
    let input = r#"let a = "Hello"
print a"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn function_def() {
    let input = r#"
let repeat = (s: String) -> s ++ s
repeat "Hello"
"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
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
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn function_def_call() {
    let input = r#"
let print_my_string = (s: String) -> { print s }
print_my_string "Hello ""#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn function_def_call_return() {
    let input = r#"
let repeat = (s: String) -> s ++ s
let hello_hello = repeat "Hello "
print hello_hello"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
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
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn return_expression() {
    let input = r#"return 1"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn array_int_index() {
    let input = r#"let arr = [0, 1, 2]
    let one = arr.1
    print ~one"#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn array_int_index_multiple() {
    let input = r#"let array = ["a", "b", "c"]

    let first = array.0
    let second = array.1
    let third = array.2

    print first
    print second
    print third
    "#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn array_string_index() {
    let input = r#"let array = ["a", "b", "c"]
    print array.1
    "#;

    let program = generate_chunk(input);
    println!("{program}");
}

#[test]
#[ignore = "currently a manual test, change to compare the program chunk as string or remove"]
fn array_variable_index() {
    let input = r#"let array = ["a", "b", "c"]
    let i = 0
    print array.(i)
    "#;

    let program = generate_chunk(input);
    println!("{program}");
}
