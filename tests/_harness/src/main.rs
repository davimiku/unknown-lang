use std::{fs, io};

use exitcode::ExitCode;

// TODO: run tests in parallel
fn main() -> Result<(), io::Error> {
    let test_programs = fs::read_dir("test_programs")?;
    for item in test_programs {
        let item = item?;
        for ok_entry in fs::read_dir(item.path().join("ok"))? {
            let ok_entry = ok_entry?;
            let test_program = fs::read_to_string(ok_entry.path())?;
            expect_ok(&test_program);
        }
        for err_entry in fs::read_dir(item.path().join("err"))? {
            let err_entry = err_entry?;
            let test_program = fs::read_to_string(err_entry.path())?;
            expect_err(&test_program);
        }
        for panic_entry in fs::read_dir(item.path().join("panic"))? {
            let panic_entry = panic_entry?;
            let test_program = fs::read_to_string(panic_entry.path())?;
            expect_panic(&test_program);
        }
    }

    Ok(())
}

fn expect_ok(input: &str) {
    let result = run(input);

    assert!(result.is_ok());
    let actual = result.unwrap();

    assert_eq!(actual, exitcode::OK);
}

fn expect_err(input: &str) {
    let result = run(input);

    assert!(result.is_ok());
    let actual = result.unwrap();

    assert_ne!(actual, exitcode::OK);
}

pub fn expect_code(input: &str, exit_code: ExitCode) {
    let result = run(input);

    assert!(result.is_ok());
    let actual = result.unwrap();

    assert_eq!(actual, exit_code);
}

// TODO: add the expected Panic variant in the test itself
fn expect_panic(input: &str /*, expected: Panic */) {
    let result = run(input);

    assert!(result.is_err());
    // let actual = result.unwrap_err();

    // assert_eq!(actual, expected);
}

fn run(input: &str) -> InterpretResult<ExitCode> {
    todo!();
}
