use indoc::indoc;

use super::check;

#[test]
fn match_with_basic_union() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        .green -> { 16 }
        .blue -> { 24 }
    }
}";
    let expected = indoc! {"
    Color~1.0 := red: () | green: () | blue: ()
main~1.1 : ((red | green | blue)) -> Int = fun \"main\"(condition~1.2 : (red | green | blue)) -> Int { match condition~1.2{
    
    
    
}; };"};

    let expected_vars = &[
        ("main~1.1", "((red | green | blue)) -> Int"),
        ("condition~1.2", "(red | green | blue)"),
    ];

    check(input, expected, expected_vars);
}
