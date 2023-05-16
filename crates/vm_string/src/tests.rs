use static_assertions::const_assert;

use super::*;

const HEAP_STRING: &str = "123456789";
const_assert!(HEAP_STRING.len() > MAX_EMBEDDED_LENGTH);

const EMBEDDED_STRING1: &str = "a";
const_assert!(EMBEDDED_STRING1.len() <= MAX_EMBEDDED_LENGTH);

const EMBEDDED_STRING4: &str = "1234";
const EMBEDDED_STRING5: &str = "12345";
const_assert!(EMBEDDED_STRING4.len() + EMBEDDED_STRING5.len() > MAX_EMBEDDED_LENGTH);

#[test]
fn test_discriminant() {
    let embedded = VMString::new(EMBEDDED_STRING1);
    let heap = VMString::new(HEAP_STRING);

    assert_eq!(embedded.discriminant(), EMBEDDED_DISCRIMINANT);
    assert_eq!(heap.discriminant(), HEAP_DISCRIMINANT);
}

#[test]
fn test_concat_embedded() {
    let a = VMString::new(EMBEDDED_STRING1);
    let b = VMString::new(EMBEDDED_STRING1);

    let expected = format!("{EMBEDDED_STRING1}{EMBEDDED_STRING1}");

    let actual = a + b;
    assert_eq!(actual.discriminant(), EMBEDDED_DISCRIMINANT);
    assert_eq!(actual.as_str(), expected);
}

#[test]
fn test_concat_longer_than_embedded() {
    let a = VMString::new(EMBEDDED_STRING4);
    let b = VMString::new(EMBEDDED_STRING5);

    let expected = format!("{EMBEDDED_STRING4}{EMBEDDED_STRING5}");

    let actual = a + b;
    assert_eq!(actual.discriminant(), HEAP_DISCRIMINANT);
    assert_eq!(actual.as_str(), expected);
}

#[test]
fn test_concat_embedded_heap() {
    let a = VMString::new(EMBEDDED_STRING4);
    let b = VMString::new(HEAP_STRING);

    let expected = format!("{EMBEDDED_STRING4}{HEAP_STRING}");

    let actual = a + b;
    assert_eq!(actual.discriminant(), HEAP_DISCRIMINANT);
    assert_eq!(actual.as_str(), expected);
}

#[test]
fn test_concat_heap_embedded() {
    let a = VMString::new(HEAP_STRING);
    let b = VMString::new(EMBEDDED_STRING4);

    let expected = format!("{HEAP_STRING}{EMBEDDED_STRING4}");

    let actual = a + b;
    assert_eq!(actual.discriminant(), HEAP_DISCRIMINANT);
    assert_eq!(actual.as_str(), expected);
}

#[test]
fn test_concat_heap() {
    let a = VMString::new(HEAP_STRING);
    let b = VMString::new(HEAP_STRING);

    let expected = format!("{HEAP_STRING}{HEAP_STRING}");

    let actual = a + b;
    assert_eq!(actual.discriminant(), HEAP_DISCRIMINANT);
    assert_eq!(actual.as_str(), expected);
}
