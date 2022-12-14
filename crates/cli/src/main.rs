use std::{fs, io};

fn main() -> io::Result<()> {
    let input = get_program_input()?;
    println!("{input}");

    let chunk = compiler::compile(&input);

    let result = vm::run(&chunk);
    println!();
    println!("{result:?}");

    // let parsed = parser::parse(&input);

    // println!("{}", &parsed.debug_tree());

    Ok(())
}

#[cfg(test)]
#[test]
fn test_main() {
    // TODO: crashes because type checking isn't finished yet
    main().expect("OK");
}

#[cfg(test)]
fn get_program_input() -> io::Result<String> {
    let program = r#"print 1"#;

    Ok(program.to_owned())
}

#[cfg(not(test))]
fn get_program_input() -> io::Result<String> {
    fs::read_to_string("test.txt")
}
