use super::check_module;

#[test]
fn always_returns_true() {
    let input = "let main = fun () -> { true }";
    let expected = "";

    check_module(input, expected);
}
