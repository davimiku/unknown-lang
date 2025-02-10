use super::check;

#[test]
fn define_union() {
    let input = "
    type Color = red | green | blue
";

    let expected_content = "Color~1.0 := red: () | green: () | blue: ();";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_and_pass_through_union() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let expected_content = "Color~1.0 := red: () | green: () | blue: ();
main~1.0 : ((red | green | blue)) -> (red | green | blue) = fun \"main\"(c~1.1 : (red | green | blue)) -> (red | green | blue) { c~1.1; };";
    let expected_vars = &[
        ("c~1.1", "(red | green | blue)"),
        ("main~1.0", "((red | green | blue)) -> (red | green | blue)"),
    ];

    check(input, expected_content, expected_vars);
}
