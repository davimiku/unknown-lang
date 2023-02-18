use std::io;

fn main() -> io::Result<()> {
    let input = get_program_input()?;
    println!("test input: `{input}`");

    let chunk = compiler::compile(&input);
    println!("{chunk}");

    println!("Begin execution:");
    println!("===== =====");

    let result = vm::run(chunk);

    println!("===== =====");
    println!("result: {result:?}");

    Ok(())
}

#[cfg(test)]
#[test]
#[ignore = "not a real unit test"]
fn test_main() {
    let main_result = main();

    if let Err(ref error) = main_result {
        eprintln!("{error}")
    }

    assert!(main_result.is_ok());
}

#[cfg(test)]
fn get_program_input() -> io::Result<String> {
    let program = r#"print ("Hello, " ++ "Caitie!")"#;

    Ok(program.to_owned())
}

#[cfg(not(test))]
fn get_program_input() -> io::Result<String> {
    std::fs::read_to_string("test.txt")
}
