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
fn test_read_byte() {
    let mut chunk = Chunk::new();

    let input = 255;

    chunk.write_byte(input);
    let actual = chunk.read::<u8>(0);

    assert_eq!(input, actual);
}

#[test]
fn test_read_int_constant() {
    let mut chunk = Chunk::new();

    let input = 8_000_000_000;

    chunk.write_int_constant(input, 1);
    let actual = chunk.read::<i64>(1);

    assert_eq!(input, actual);
}

#[test]
fn test_read_float_constant() {
    let mut chunk = Chunk::new();

    let input = 8_000_000_000.0;

    chunk.write_float_constant(input, 1);
    let actual = chunk.read::<f64>(1);

    assert_eq!(input, actual);
}

#[test]
fn test_print_input() {
    //
}
