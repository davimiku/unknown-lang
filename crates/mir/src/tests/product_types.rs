use super::check_module;

#[test]
fn pull_field_from_product_type() {
    let input = "let main = fun () -> {
        let record = [ t = 1 ]
        record.t
        }";
    let expected = "
fun main:

    BB0():
        ";

    check_module(input, expected);
}
