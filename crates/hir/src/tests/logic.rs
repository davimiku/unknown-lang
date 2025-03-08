use super::check;
use indoc::indoc;

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

    let expected_expr = indoc! {"
    fun (condition~1.0 : Bool~0.3) -> Float { if (condition~1.0) { 16.0; } else { 8.0; }; };"};

    let expected_vars = &[("condition~1.0", "Bool~0.3")];

    check(input, expected_expr, expected_vars);
}
