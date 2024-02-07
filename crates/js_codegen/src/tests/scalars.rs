use super::check;

#[test]
fn empty() {
    let input = "";
    let expected = "";

    check(input, expected);
}

#[test]
fn integer() {
    let input = "123";
    let expected = "123;\n";

    check(input, expected);
}

#[test]
fn string_concat() {
    let input = "\"Hello\" ++ \"World!\"";
    let expected = "\"Hello\" + \"World!\";\n";

    check(input, expected);
}
