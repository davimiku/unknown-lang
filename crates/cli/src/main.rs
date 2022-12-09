use std::{fs, io};

fn main() -> io::Result<()> {
    let input = fs::read_to_string("test.txt")?;
    println!("{input}");

    let chunk = compiler::compile(&input);

    let result = vm::run(&chunk);
    println!();
    println!("{result:?}");

    // let parsed = parser::parse(&input);

    // println!("{}", &parsed.debug_tree());

    Ok(())
}
