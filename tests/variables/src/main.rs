use _harness::VMInt;

fn main() {
    let input = r#"
    let a = 1234
    a
    "#;
    let expected: VMInt = 1234;

    _harness::expect_ok(input, expected);
}
