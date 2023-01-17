#[cfg(not(test))]
use std::fs;

use std::io;

fn main() -> io::Result<()> {
    let input = get_program_input()?;
    println!("test input: `{input}`");

    let chunk = compiler::compile(&input);
    println!("{chunk}");

    println!("Begin execution:");
    println!("===== =====");

    let result = vm::run(&chunk);

    println!("===== =====");
    println!("result: {result:?}");

    Ok(())
}

#[cfg(test)]
#[test]
fn test_main() {
    let main_result = main();

    assert!(main_result.is_ok());
}

#[cfg(test)]
fn get_program_input() -> io::Result<String> {
    // TODO: not working
    let program = r#"if false { print "then branch" } else { print "else branch" }"#;

    Ok(program.to_owned())
}

#[cfg(not(test))]
fn get_program_input() -> io::Result<String> {
    fs::read_to_string("test.txt")
}
