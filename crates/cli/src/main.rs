use std::io;

fn main() -> io::Result<()> {
    let input = get_program_input()?;
    println!("test input: `{input}`");

    let compile_result = compiler::compile(&input);
    println!();

    match compile_result {
        Ok(chunk) => {
            for function in chunk.functions.iter() {
                println!("{function}");
            }
            println!("Begin execution:");
            println!("===== =====");

            let result = vm::run(chunk);

            println!("===== =====");
            println!("result: {result:?}");
        }
        Err(diagnostics) => {
            eprintln!("compilation failed!");
            for diagnostic in diagnostics {
                eprintln!("{diagnostic:?}");
            }
        }
    }

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
    // let program = r#"
    // let repeat = (s: String) -> s ++ s
    // let hello = repeat "Hi "
    // print hello"#;
    let program = "print ~123";

    Ok(program.to_owned())
}

#[cfg(not(test))]
fn get_program_input() -> io::Result<String> {
    std::fs::read_to_string("test.txt")
}
