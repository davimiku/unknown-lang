use std::collections::HashMap;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::panic;
use std::path::Path;

use exitcode::is_error;
use exitcode::is_success;
use exitcode::ExitCode;
use rayon::prelude::*;
use vm::InterpretResult;
pub use vm_types::{VMBool, VMFloat, VMInt};

const COMPILER_ERR_PATH: &str = "compile_err";
const RUNTIME_ERR_PATH: &str = "runtime_err";
const RUNTIME_OK_PATH: &str = "ok";
const RUNTIME_PANIC_PATH: &str = "panic";

// TODO: catch panics from each test
fn main() -> io::Result<()> {
    panic::set_hook(Box::new(|panic_info| {
        // TODO: need to test and see if this allows easily finding the test that paniced
        eprintln!("panic occurred: {panic_info}");
    }));

    let test_programs = fs::read_dir("test_programs")?;
    // TODO: newtype this
    let results: Vec<(String, HashMap<TestType, (usize, usize)>)> = test_programs
        .par_bridge()
        .map(|dir_entry| {
            let mut results: HashMap<TestType, (usize, usize)> = HashMap::new();
            let path = dir_entry.unwrap().path();

            // TODO: record the failed tests
            {
                let (failed_tests, num_tests) = run_ok(&path).unwrap();
                results.insert(TestType::Ok, (failed_tests.len(), num_tests));
            }

            {
                let (failed_tests, num_tests) = run_err(&path).unwrap();
                results.insert(TestType::Err, (failed_tests.len(), num_tests));
            }

            {
                let (failed_tests, num_tests) = run_panic(&path).unwrap();
                results.insert(TestType::Panic, (failed_tests.len(), num_tests));
            }

            let test_category = path.as_os_str().to_str().unwrap().to_owned();
            (test_category, results)
        })
        .collect();

    for (test_category, test_result) in results {
        let (ok_failed, ok_total) = test_result[&TestType::Ok];
        let (err_failed, err_total) = test_result[&TestType::Err];
        let (panic_failed, panic_total) = test_result[&TestType::Panic];

        println!("{test_category}:");
        println!("  Ok tests: {}/{} passed", ok_total - ok_failed, ok_total);
        println!(
            "  Err tests: {}/{} passed",
            err_total - err_failed,
            err_total
        );
        println!(
            "  Panic tests: {}/{} passed",
            panic_total - panic_failed,
            panic_total
        );
    }
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum TestType {
    /// Expect the test program to complete with a success exit code
    Ok,

    /// Expect the test program to complete with an non-success exit code
    Err,

    /// Expect the test program to panic (caught by the VM, not a Rust panic)
    Panic,
}

/// Programs that are expected to have an exit code 0 (OK)
fn run_ok(path: &Path) -> io::Result<(Vec<OsString>, usize)> {
    let path = path.join(RUNTIME_OK_PATH);
    let num_tests = fs::read_dir(path.clone())?.count();
    let mut failed_tests = vec![];

    for dir_entry in fs::read_dir(path)? {
        let dir_entry = dir_entry?;
        let test_program = fs::read_to_string(dir_entry.path())?;
        if !expect_ok(&test_program) {
            failed_tests.push(dir_entry.file_name())
        }
    }

    Ok((failed_tests, num_tests))
}

fn expect_ok(input: &str) -> bool {
    let result = run(input);

    result.is_ok_and(is_success)
}

/// Programs that are expected to have a non-zero exit code (Err)
fn run_err(path: &Path) -> io::Result<(Vec<OsString>, usize)> {
    let path = path.join(RUNTIME_ERR_PATH);
    let num_tests = fs::read_dir(path.clone())?.count();
    let mut failed_tests = vec![];

    for dir_entry in fs::read_dir(path)? {
        let dir_entry = dir_entry?;
        let test_program = fs::read_to_string(dir_entry.path())?;
        if !expect_err(&test_program) {
            failed_tests.push(dir_entry.file_name())
        }
    }

    Ok((failed_tests, num_tests))
}

fn expect_err(input: &str) -> bool {
    let result = run(input);

    result.is_ok_and(is_error)
}

// Programs that are expected to panic
// This is a panic in the language itself within the VM, not a Rust panic
fn run_panic(path: &Path) -> io::Result<(Vec<OsString>, usize)> {
    let path = path.join(RUNTIME_PANIC_PATH);
    let num_tests = fs::read_dir(path.clone())?.count();
    let mut failed_tests = vec![];

    for dir_entry in fs::read_dir(path)? {
        let dir_entry = dir_entry?;
        let test_program = fs::read_to_string(dir_entry.path())?;
        if !expect_panic(&test_program) {
            failed_tests.push(dir_entry.file_name())
        }
    }

    Ok((failed_tests, num_tests))
}

// TODO: add the expected Panic variant in the test itself
fn expect_panic(input: &str /*, expected: Panic */) -> bool {
    let result = run(input);

    result.is_err()
    // result.is_err_and(|panic| panic == expected)
}

fn run(input: &str) -> InterpretResult<ExitCode> {
    let chunk = compiler::compile(input, false);
    let chunk = chunk.expect("valid program for this test");

    vm::run(chunk)
}
