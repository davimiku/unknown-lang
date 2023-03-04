use std::fmt::Debug;

use vm::{InterpretResult, RuntimePanic};
use vm_types::FromWordVec;
pub use vm_types::{string::VMString, VMBool, VMFloat, VMInt};

pub fn expect_ok<T>(input: &str, expected: T)
where
    T: FromWordVec + PartialEq + Debug,
{
    let result = run::<T>(input);

    assert!(result.is_ok());
    let actual = result.unwrap();

    assert_eq!(actual, expected);

    println!("Test passed!");
}

pub fn expect_panic(input: &str, expected: RuntimePanic) {
    let result = run::<()>(input);

    assert!(result.is_err());
    let actual = result.unwrap_err();

    assert_eq!(actual, expected);

    println!("Test passed!");
}

fn run<T>(input: &str) -> InterpretResult<T>
where
    T: FromWordVec,
{
    let chunk = compiler::compile(input);

    vm::run_and_return::<T>(chunk).map(|words| T::from_vec(words))
}