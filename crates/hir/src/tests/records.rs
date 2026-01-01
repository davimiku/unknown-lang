use crate::tests::check;

#[test]
fn define_record_type() {
    let input = "
    type Point = [ x: Float, y: Float ]
";

    let expected_content = "Point~1.0 := [ x: Float~0.1, y: Float~0.1 ]";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}
