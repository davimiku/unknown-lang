use std::{fs, io};

fn main() -> io::Result<()> {
    let input = fs::read_to_string("test.txt")?;
    println!("{input}");

    let parsed = parser::parse(&input);

    println!("{}", &parsed.debug_tree());
    println!();

    Ok(())
}
