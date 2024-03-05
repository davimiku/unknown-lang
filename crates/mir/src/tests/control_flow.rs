use super::check_function;

#[test]
fn basic_if_else() {
    let input = "
fun (condition: Bool) -> Float { 
    if condition {
        16.0
    } else {
        8.0
    }
}";
    let expected = "
fun {anonymous}:
    params: {none}
    mut _0: 16
    _1: 16
    
    BB0(_1):
        _1 = const 16
        _0 = copy _1
        return
";

    check_function(input, expected);
}
