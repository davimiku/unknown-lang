use super::check_module;

#[test]
fn always_returns_true() {
    let input = "let main = fun () -> { true }";
    let expected = "
fun main:
    params: {none}
    mut _0: (false | true)
    
    BB0():
        _0 := const 1
        Return _0 ->";

    check_module(input, expected);
}
